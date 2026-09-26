"""Load NFL player-side data from nflverse (nflreadpy) into DuckDB.

Replaces the nfl_data_py loader (deprecated). Every pull is cached as parquet
in data/raw/nflverse/<name>.parquet (common.cached_parquet), one file per
season where the source is split by season, so a rebuild makes no network
calls. The shared schedules cache (also used by 02c_load_nfl_stats.py) dates
the 2025+ depth charts.

Sources (nflreadpy):
  load_players          player master: bio, draft, cross-platform IDs
  load_rosters_weekly   weekly rosters with status, NFL_WEEKLY_ROSTER_SEASONS
  load_rosters          season rosters, NFL_SEASONS
  load_draft_picks      every draft since 1980 (PFR)
  load_combine          every combine since 2000 (PFR)
  load_ff_playerids     DynastyProcess ID crosswalk
  load_contracts        OverTheCap contracts with nested contract history and
                        player-year cap tables
  load_injuries         weekly injury reports, NFL_INJURY_SEASONS
  load_depth_charts     depth charts 2001+ (weekly GSIS charts through 2024,
                        daily ESPN snapshots from 2025)
  load_trades           trades since 2002 (PFR)

Tables written (full refresh; every source column kept; a franchise_id column
follows every team column, named <prefix>_franchise_id when the team column
has a prefix; position_group = positions.position_group()). Draft-team codes
from before a relocation (BAL, STL, LAR, HOU) are dated by the draft season
(see RELOCATED_CODES), because to_franchise() maps a code to today's team:
  nfl_players           one row per gsis_id (source position_group renamed
                        nflverse_position_group); draft_franchise_id from the
                        player's pick in nfl_draft_picks where present
  nfl_rosters_weekly    player x team x week x game_type; season_type (REG /
                        POST); key_rows / is_key_primary pick one row when the
                        source repeats a player-team-week (2002-2015 files add
                        TRC/TRD/TRT rows on top of the roster status)
  nfl_rosters_season    player x team x season; same key flags
  nfl_draft_picks       one row per pick, all years (season, pick)
  nfl_combine           one row per combine invitee; gsis_id + link_method
                        (pfr_id > draft_slot > name_pos_year)
  nfl_ids               ID crosswalk
  nfl_contracts         one row per OTC contract. contract_id =
                        otc_id-year_signed-seq (same-year contracts are never
                        collapsed); year_signed_missing flags year_signed = 0;
                        identical_rows counts rows with the same terms;
                        franchise_id for one team, franchise_ids + n_teams for
                        'NYJ/GB' strings; gsis_id = OTC's (kept as gsis_id_otc)
                        else nfl_players.otc_id; contract_type,
                        contract_status, amount_earned, effective_apy from the
                        matched contract_history entry (history_match)
  nfl_contract_history  every contract_history entry (player level in the
                        source), with contract_id where it matches a contract
  nfl_contract_years    otc_id x year cap table (base, bonuses, guarantees,
                        cap number, cash) with the governing contract_id
  nfl_injuries          injury report rows
  nfl_depth_charts      depth-chart rows harmonized across both formats:
                        season, week, game_type, franchise_id, gsis_id,
                        depth_position, formation, depth_rank, starter
  nfl_trades            one row per asset moved; gsis_id via pfr_id

Usage: python 02_load_nfl.py [--refresh]   (--refresh re-downloads everything)
"""
import argparse
import re
import unicodedata

import nflreadpy as nfl
import polars as pl

from common import RELOCATED_CODES, cached_parquet, connect, to_franchise, write_table
from config import (DRAFT_YEARS, LAST_SEASON, NFL_INJURY_SEASONS, NFL_SEASONS,
                    NFL_WEEKLY_ROSTER_SEASONS)
from positions import position_group

SRC = "nflverse via nflreadpy"
DEPTH_CHART_SEASONS = [s for s in NFL_SEASONS if s >= 2001]   # source starts 2001

POST_GAME_TYPES = ["WC", "DIV", "CON", "SB"]

# 2002-2015 weekly/season roster files repeat a player-team-week with these
# codes on top of the roster status row; they lose ties in is_key_primary.
LAYERED_STATUSES = ["TRC", "TRD", "TRT"]

# OverTheCap contract terms that identify a contract_history entry
CONTRACT_TERMS = ["year_signed", "years", "value", "apy", "guaranteed"]
HISTORY_TERMS = {"year_signed": "year_signed", "yrs": "years", "total": "value",
                 "apy": "apy", "guarantees": "guaranteed"}

NAME_SUFFIXES = {"jr", "sr", "ii", "iii", "iv", "v"}


# ============================================================================
# Helpers
# ============================================================================

def fetch_season(prefix, loader, season, refresh):
    """One cached parquet per season; an empty season is an error."""
    df = cached_parquet(f"{prefix}_{season}", lambda: loader(season), refresh)
    if df.height == 0:
        raise ValueError(f"{prefix}_{season}: the source returned no rows")
    return df


def fetch_seasons(prefix, loader, seasons, refresh):
    """fetch_season for each season, stacked (columns unioned)."""
    frames = [fetch_season(prefix, loader, s, refresh) for s in seasons]
    return pl.concat(frames, how="diagonal_relaxed")


def move_after(df, col, anchor):
    cols = [c for c in df.columns if c != col]
    i = cols.index(anchor) + 1
    return df.select(cols[:i] + [col] + cols[i:])


def add_franchise(df, team_col, out_col="franchise_id", season_col=None):
    """Insert out_col = to_franchise(team_col) right after team_col.

    season_col (the season the code refers to) dates RELOCATED_CODES; pass
    it for team columns that reach back before 1997.
    """
    codes = df.get_column(team_col).drop_nulls().unique().to_list()
    fid = pl.col(team_col).replace_strict({c: to_franchise(c) for c in codes},
                                          default=None, return_dtype=pl.String)
    if season_col:
        for code, (last, old) in RELOCATED_CODES.items():
            fid = (pl.when((pl.col(team_col) == code) & (pl.col(season_col) <= last))
                   .then(pl.lit(old)).otherwise(fid))
    return move_after(df.with_columns(fid.alias(out_col)), out_col, team_col)


def add_position_group(df, cols, after, out_col="position_group"):
    """out_col = position group of the first of `cols` that maps to one."""
    exprs = []
    for c in cols:
        codes = df.get_column(c).drop_nulls().unique().to_list()
        exprs.append(pl.col(c).replace_strict({v: position_group(v) for v in codes},
                                              default=None, return_dtype=pl.String))
    df = df.with_columns(pl.coalesce(exprs).alias(out_col))
    return move_after(df, out_col, after)


def add_season_type(df):
    """season_type = REG / POST from game_type, keeping a source value where
    one exists; other game types (the depth charts' SBBYE week) stay NULL."""
    derived = (pl.when(pl.col("game_type") == "REG").then(pl.lit("REG"))
               .when(pl.col("game_type").is_in(POST_GAME_TYPES)).then(pl.lit("POST")))
    source = pl.col("season_type") if "season_type" in df.columns else pl.lit(None)
    df = df.with_columns(season_type=pl.coalesce(source, derived))
    return move_after(df, "season_type", "game_type")


def flag_key_primary(df, key):
    """key_rows = rows sharing `key`; is_key_primary marks one row per key.

    Ties prefer a non-layered status, then the row with the most non-null
    fields (the source sometimes repeats a player without his IDs). Rows
    without a gsis_id are never primary.
    """
    df = df.with_row_index("_row").with_columns(
        _layered=pl.col("status").is_in(LAYERED_STATUSES).fill_null(False),
        _filled=pl.sum_horizontal(pl.all().is_not_null()))
    df = (df.sort(["_layered", "_filled", "_row"], descending=[False, True, False])
          .with_columns(key_rows=pl.len().over(key),
                        is_key_primary=(pl.int_range(pl.len()).over(key) == 0)
                        & pl.col("gsis_id").is_not_null())
          .sort("_row"))
    return df.drop("_row", "_layered", "_filled")


def norm_name(name):
    """'D.J. Moore Jr.' -> 'dj moore' (ASCII, lower case, no suffix)."""
    if name is None:
        return None
    s = unicodedata.normalize("NFKD", name).encode("ascii", "ignore").decode().lower()
    s = re.sub(r"[.'`]", "", s)
    tokens = [t for t in re.sub(r"[^a-z]+", " ", s).split() if t not in NAME_SUFFIXES]
    return " ".join(tokens) or None


def same_within(df, key, col):
    """Raise unless `col` takes one value within each `key` group."""
    bad = df.group_by(key).agg(pl.col(col).n_unique().alias("n")).filter(pl.col("n") > 1)
    if bad.height:
        raise ValueError(f"{col} varies within {key} for {bad.height} groups")


# ============================================================================
# Players, rosters, draft, combine, IDs
# ============================================================================

def build_players(draft, refresh):
    p = cached_parquet("players", nfl.load_players, refresh)
    p = p.rename({"position_group": "nflverse_position_group"})
    p = add_position_group(p, ["position"], after="position")
    p = add_franchise(p, "latest_team", "latest_franchise_id")
    # draft_team uses today's codes (a St. Louis Cardinals pick reads 'LA'),
    # so the drafting franchise comes from the player's own pick in
    # nfl_draft_picks (1980+), else from draft_team dated by draft_year
    picks = draft.drop_nulls("gsis_id").select("gsis_id", draft_year="season",
                                               _pick_fid="franchise_id")
    p = add_franchise(p, "draft_team", "draft_franchise_id", season_col="draft_year")
    p = p.join(picks, on=["gsis_id", "draft_year"], how="left", validate="1:1",
               maintain_order="left")
    return (p.with_columns(draft_franchise_id=pl.coalesce("_pick_fid", "draft_franchise_id"))
            .drop("_pick_fid"))


def build_rosters_weekly(refresh):
    w = fetch_seasons("rosters_weekly", nfl.load_rosters_weekly,
                      NFL_WEEKLY_ROSTER_SEASONS, refresh)
    w = add_franchise(w, "team")
    # draft_club is the drafting team in the player's entry (draft) year
    w = add_franchise(w, "draft_club", "draft_franchise_id", season_col="entry_year")
    # Fine codes (CB, DE, T) before 2016 and coarse codes (DB, DL, OL) after
    # both map to one group; depth_chart_position covers KR/PR/blank codes
    w = add_position_group(w, ["position", "depth_chart_position"], after="position")
    w = add_season_type(w)
    return flag_key_primary(w, ["gsis_id", "season", "week", "game_type", "team"])


def build_rosters_season(refresh):
    r = fetch_seasons("rosters", nfl.load_rosters, NFL_SEASONS, refresh)
    r = add_franchise(r, "team")
    r = add_franchise(r, "draft_club", "draft_franchise_id", season_col="entry_year")
    r = add_position_group(r, ["position", "depth_chart_position"], after="position")
    return flag_key_primary(r, ["gsis_id", "season", "team"])


def build_draft_picks(refresh):
    d = cached_parquet("draft_picks", lambda: nfl.load_draft_picks(True), refresh)
    d = add_franchise(d, "team", season_col="season")
    return add_position_group(d, ["position"], after="position")


def build_combine(players, draft, refresh):
    """Combine rows with a gsis_id found by, in order of precedence:
    (a) pfr_id -> nfl_players.pfr_id;
    (b) draft slot (draft_year, draft_ovr) -> nfl_draft_picks (season, pick);
    (c) undrafted rows only: normalized name + position group + combine
        season = rookie_season of an undrafted player, kept only when the key
        is unique among the still-unlinked rows on both sides.
    The source gives a few pfr_ids to two different same-name invitees; when
    one gsis_id links to several rows, only the best-matching row keeps it
    (see resolve_shared_links).
    """
    cb = cached_parquet("combine", lambda: nfl.load_combine(True), refresh)
    cb = add_franchise(cb, "draft_team", "draft_franchise_id")
    cb = add_position_group(cb, ["pos"], after="pos")
    cb = cb.with_row_index("_row").with_columns(
        _dy=pl.col("draft_year").cast(pl.Int32), _ovr=pl.col("draft_ovr").cast(pl.Int32),
        _name=pl.col("player_name").map_elements(norm_name, return_dtype=pl.String))

    # (a) and (b)
    by_pfr = players.select("pfr_id", _g_pfr="gsis_id").drop_nulls("pfr_id")
    by_slot = (draft.drop_nulls("gsis_id")
               .select(_dy="season", _ovr="pick", _g_slot="gsis_id"))
    cb = (cb.join(by_pfr, on="pfr_id", how="left", validate="m:1")
          .join(by_slot, on=["_dy", "_ovr"], how="left", validate="m:1"))
    both = cb.filter(pl.col("_g_pfr").is_not_null() & pl.col("_g_slot").is_not_null())
    print(f"  combine: pfr_id links {cb['_g_pfr'].is_not_null().sum():,}; draft slot links "
          f"{cb['_g_slot'].is_not_null().sum():,}, of which {both.height:,} also link by pfr_id "
          f"({(both['_g_pfr'] != both['_g_slot']).sum()} disagree; pfr_id kept)")

    # (c) unique name + position group + year among unlinked undrafted rows
    linked = set(cb["_g_pfr"].drop_nulls()) | set(cb["_g_slot"].drop_nulls())
    key = ["_name", "position_group", "_year"]
    left = (cb.filter(pl.col("_g_pfr").is_null() & pl.col("_g_slot").is_null()
                      & pl.col("_ovr").is_null())
            .select("_row", *key[:2], _year="season")
            .drop_nulls(key).filter(pl.len().over(key) == 1))
    right = (players.filter(pl.col("draft_year").is_null()
                            & ~pl.col("gsis_id").is_in(list(linked)))
             .select("gsis_id", "position_group", _year="rookie_season",
                     _name=pl.col("display_name").map_elements(norm_name, return_dtype=pl.String))
             .drop_nulls(key).filter(pl.len().over(key) == 1))
    by_name = left.join(right, on=key, how="inner").select("_row", _g_name="gsis_id")
    cb = cb.join(by_name, on="_row", how="left", validate="1:1")

    cb = cb.with_columns(
        gsis_id=pl.coalesce("_g_pfr", "_g_slot", "_g_name"),
        link_method=pl.when(pl.col("_g_pfr").is_not_null()).then(pl.lit("pfr_id"))
        .when(pl.col("_g_slot").is_not_null()).then(pl.lit("draft_slot"))
        .when(pl.col("_g_name").is_not_null()).then(pl.lit("name_pos_year")))
    cb = resolve_shared_links(cb, players)
    cb = cb.sort("_row").drop("_row", "_dy", "_ovr", "_name", "_g_pfr", "_g_slot", "_g_name")
    lead = ["season", "player_name", "pos", "position_group", "gsis_id", "link_method"]
    return cb.select(lead + [c for c in cb.columns if c not in lead])


def resolve_shared_links(cb, players):
    """When one gsis_id links to several combine rows, keep it on the row
    that best matches the player (school among his colleges, then position
    group, then combine season closest to his rookie season) and unlink the
    rest; if the best two rows tie, unlink all of them."""
    def norm_school(x):
        return re.sub(r"[^a-z0-9]", "", x.lower())

    colleges = {g: {norm_school(c) for c in str(v).split(";")}
                for g, v in players.select("gsis_id", "college_name").iter_rows()}
    shared = (cb.filter(pl.col("gsis_id").is_not_null() & (pl.len().over("gsis_id") > 1))
              .join(players.select("gsis_id", _pgrp="position_group", _rookie="rookie_season"),
                    on="gsis_id", how="left", validate="m:1"))
    if shared.height == 0:
        return cb
    shared = shared.with_columns(
        _school=pl.struct("gsis_id", "school").map_elements(
            lambda r: r["school"] is not None and norm_school(r["school"]) in colleges[r["gsis_id"]],
            return_dtype=pl.Boolean),
        _pos=(pl.col("position_group") == pl.col("_pgrp")).fill_null(False),
        _gap=(pl.col("season") - pl.col("_rookie")).abs())
    ranked = shared.sort(["gsis_id", "_school", "_pos", "_gap"],
                         descending=[False, True, True, False], nulls_last=True)
    score = ["_school", "_pos", "_gap"]
    best = ranked.group_by("gsis_id", maintain_order=True).agg(
        pl.col("_row").first(), *[pl.col(c).head(2).n_unique().alias(f"{c}_n") for c in score])
    keep = best.filter(pl.any_horizontal([pl.col(f"{c}_n") > 1 for c in score]))["_row"].to_list()
    drop = shared.filter(~pl.col("_row").is_in(keep))["_row"].to_list()
    print(f"  combine: {best.height} gsis_ids linked to more than one combine row; "
          f"kept {len(keep)} best matches, unlinked {len(drop)} rows")
    return cb.with_columns(
        [pl.when(pl.col("_row").is_in(drop)).then(None).otherwise(pl.col(c)).alias(c)
         for c in ["gsis_id", "link_method"]])


def build_ids(refresh):
    ids = cached_parquet("ff_playerids", nfl.load_ff_playerids, refresh)
    return add_franchise(ids, "team")


# ============================================================================
# Contracts (OverTheCap)
# ============================================================================

def match_history(contracts, history):
    """Pair each contract row with its contract_history entry.

    Pass 1 matches otc_id + franchise + terms (year signed, years, value,
    APY, guarantees); pass 2 matches the leftovers on otc_id + terms alone
    (multi-team contract strings, team re-attributions). Rows with identical
    terms are paired in order (contract_seq with history_seq).
    """
    def terms(df):
        return df.with_columns(pl.col("value", "apy", "guaranteed").round(4))

    def pair(top, his, key, method):
        top = top.drop_nulls(key).sort("contract_seq").with_columns(
            _k=pl.int_range(pl.len()).over(key))
        his = his.drop_nulls(key).sort("history_seq").with_columns(
            _k=pl.int_range(pl.len()).over(key))
        return top.join(his, on=[*key, "_k"], how="inner").select(
            "contract_id", "otc_id", "history_seq", history_match=pl.lit(method))

    top = terms(contracts.select("contract_id", "contract_seq", "otc_id",
                                 "franchise_id", *CONTRACT_TERMS))
    his = terms(history.select("otc_id", "history_seq", "franchise_id",
                               *HISTORY_TERMS).rename(HISTORY_TERMS))
    first = pair(top, his, ["otc_id", "franchise_id", *CONTRACT_TERMS], "terms_team")
    second = pair(top.join(first, on="contract_id", how="anti"),
                  his.join(first, on=["otc_id", "history_seq"], how="anti"),
                  ["otc_id", *CONTRACT_TERMS], "terms_only")
    return pl.concat([first, second])


def build_contracts(players, refresh):
    """nfl_contracts, nfl_contract_history and nfl_contract_years.

    The source's season_history and contract_history columns are player
    level (identical on every row of an otc_id), so they are unnested once
    per player rather than per contract; nfl_contracts keeps them as
    delivered.
    """
    raw = cached_parquet("contracts", nfl.load_contracts, refresh)
    same_within(raw, "otc_id", "season_history")
    same_within(raw, "otc_id", "contract_history")
    same_within(raw.drop_nulls("gsis_id"), "otc_id", "gsis_id")

    # Surrogate key: otc_id-year_signed-seq, seq ordered by the contract terms
    # (source row order only breaks ties between identical rows)
    c = raw.with_row_index("_row")
    c = (c.sort(["otc_id", "year_signed", "team", "years", "value", "apy",
                 "guaranteed", "_row"], nulls_last=True)
         .with_columns(contract_seq=pl.int_range(1, pl.len() + 1).over(["otc_id", "year_signed"]))
         .sort("_row").drop("_row"))
    # identical_rows > 1: the source lists the same terms more than once for
    # a player (repeat practice-squad deals, or a duplicated row)
    c = c.with_columns(contract_id=pl.format("{}-{}-{}", "otc_id", "year_signed", "contract_seq"),
                       year_signed_missing=pl.col("year_signed") == 0,
                       identical_rows=pl.len().over(["otc_id", "team", *CONTRACT_TERMS]))

    # Team string: one nickname, or '/'-joined codes for multi-team deals
    parts = {t: [to_franchise(x) for x in t.split("/")]
             for t in c["team"].drop_nulls().unique().to_list()}
    c = c.with_columns(
        n_teams=pl.col("team").replace_strict({t: len(v) for t, v in parts.items()},
                                              default=None, return_dtype=pl.Int32),
        franchise_ids=pl.col("team").replace_strict(
            {t: "/".join(v) if all(v) else None for t, v in parts.items()},
            default=None, return_dtype=pl.String),
        franchise_id=pl.col("team").replace_strict(
            {t: v[0] if len(v) == 1 else None for t, v in parts.items()},
            default=None, return_dtype=pl.String))
    c = add_franchise(c, "draft_team", "draft_franchise_id")
    c = add_position_group(c, ["position"], after="position")

    # gsis_id: OTC's own, else nfl_players.otc_id (source value kept as gsis_id_otc)
    by_otc = (players.drop_nulls("otc_id")
              .select(pl.col("otc_id").cast(pl.Int32), _g_players="gsis_id"))
    c = (c.rename({"gsis_id": "gsis_id_otc"})
         .join(by_otc, on="otc_id", how="left", validate="m:1", maintain_order="left")
         .with_columns(gsis_id=pl.coalesce("gsis_id_otc", "_g_players"),
                       gsis_link_method=pl.when(pl.col("gsis_id_otc").is_not_null())
                       .then(pl.lit("otc_source"))
                       .when(pl.col("_g_players").is_not_null()).then(pl.lit("players_otc_id")))
         .drop("_g_players"))
    same_within(c, "otc_id", "gsis_id")   # NULL counts as a value here
    player_gsis = c.select("otc_id", "gsis_id").unique("otc_id")

    # Player-level contract history, one row per entry
    per_player = raw.unique("otc_id", keep="first", maintain_order=True)
    history = (per_player.select("otc_id", "contract_history")
               .explode("contract_history", empty_as_null=False, keep_nulls=False)
               .unnest("contract_history")
               .with_columns(history_seq=pl.int_range(1, pl.len() + 1).over("otc_id")))
    history = history.filter(~pl.all_horizontal(pl.exclude("otc_id", "history_seq").is_null()))
    history = add_franchise(history, "team")

    # Attach the matched history entry to each contract
    m = match_history(c, history)
    hist_cols = history.select(
        "otc_id", "history_seq", history_team="team", history_franchise_id="franchise_id",
        contract_type="contract_type", contract_status="status",
        amount_earned="amount_earned", percent_earned="percent_earned",
        effective_apy="effective_apy")
    c = (c.join(m.drop("otc_id"), on="contract_id", how="left", validate="1:1")
         .join(hist_cols, on=["otc_id", "history_seq"], how="left", validate="m:1"))
    lead = ["contract_id", "otc_id", "gsis_id", "gsis_link_method", "contract_seq", "identical_rows",
            "player", "position", "position_group", "team", "franchise_id",
            "franchise_ids", "n_teams", "year_signed", "year_signed_missing",
            "years", "value", "apy", "guaranteed", "contract_type", "contract_status"]
    contracts = c.select(lead + [x for x in c.columns if x not in lead])

    history = (history.join(m.drop("history_match"), on=["otc_id", "history_seq"],
                            how="left", validate="1:1")
               .join(player_gsis, on="otc_id", how="left", validate="m:1"))
    lead = ["otc_id", "history_seq", "gsis_id", "contract_id"]
    history = history.select(lead + [x for x in history.columns if x not in lead])

    years = build_contract_years(per_player, contracts, player_gsis)
    return contracts, history, years


def build_contract_years(per_player, contracts, player_gsis):
    """Unnest season_history: one row per otc_id x year (base salary,
    bonuses, guaranteed salary, cap number, cap percent, cash paid).

    contract_id = the governing contract: the latest contract signed with
    that franchise on or before the year (ties: latest history entry, then
    contract_seq). Years after LAST_SEASON are OTC projections.
    """
    sh = (per_player.select("otc_id", "season_history")
          .explode("season_history", empty_as_null=False, keep_nulls=False)
          .unnest("season_history"))
    sh = sh.filter(~pl.all_horizontal(pl.exclude("otc_id").is_null()))
    is_year = pl.col("year").str.contains(r"^\d{4}$").fill_null(False)
    other = sh.filter(~is_year)["year"].value_counts().sort("year")
    years = sh.filter(is_year).with_columns(pl.col("year").cast(pl.Int32))
    # 'Total' rows repeat the sum of a player's year rows; check before dropping
    totals = (sh.filter(pl.col("year") == "Total").select("otc_id", total="cap_number")
              .join(years.group_by("otc_id").agg(pl.col("cap_number").sum().alias("summed")),
                    on="otc_id", how="left"))
    agree = ((totals["total"] - totals["summed"]).abs() < 0.01).mean()
    print(f"  contract years: dropped non-year rows {dict(other.iter_rows())}; "
          f"'Total' cap_number equals the sum of year rows for {agree:.1%} of players")
    years = add_franchise(years, "team")
    years = years.join(player_gsis, on="otc_id", how="left", validate="m:1").with_columns(
        is_projected=pl.col("year") > LAST_SEASON)

    cand = (contracts.filter(~pl.col("year_signed_missing"))
            .select("otc_id", "contract_id", "year_signed", "contract_seq",
                    "history_seq", _fid=pl.coalesce("franchise_id", "history_franchise_id")))
    gov = (years.select("otc_id", "year", "franchise_id")
           .join(cand, left_on=["otc_id", "franchise_id"], right_on=["otc_id", "_fid"])
           .filter(pl.col("year_signed") <= pl.col("year"))
           .group_by(["otc_id", "year"])
           .agg(pl.col("contract_id").sort_by(["year_signed", "history_seq", "contract_seq"],
                                              nulls_last=False).last()))
    years = years.join(gov, on=["otc_id", "year"], how="left", validate="1:1")
    lead = ["otc_id", "year", "gsis_id", "team", "franchise_id", "contract_id", "is_projected"]
    return years.select(lead + [x for x in years.columns if x not in lead])


# ============================================================================
# Injuries, depth charts, trades
# ============================================================================

def build_injuries(refresh):
    inj = fetch_seasons("injuries", nfl.load_injuries, NFL_INJURY_SEASONS, refresh)
    inj = inj.with_columns(pl.col("season", "week").cast(pl.Int32))
    inj = add_franchise(inj, "team")
    inj = add_position_group(inj, ["position"], after="position")
    return add_season_type(inj)


def team_games(schedules):
    """One row per team x game: season, week, game_type, franchise_id and
    kickoff in UTC (schedule dates and times are US Eastern)."""
    games = pl.concat([
        schedules.select("season", "week", "game_type", "gameday", "gametime", team=side)
        for side in ["home_team", "away_team"]])
    return add_franchise(games, "team").with_columns(
        kickoff=pl.concat_str("gameday", pl.lit(" "), "gametime")
        .str.to_datetime("%Y-%m-%d %H:%M").dt.replace_time_zone("America/New_York")
        .dt.convert_time_zone("UTC"))


def snapshot_formation(pos_grp):
    """ESPN personnel group ('3WR 1TE', 'Base 4-3 D') -> legacy formation."""
    return (pl.when(pos_grp == "Special Teams").then(pl.lit("Special Teams"))
            .when(pos_grp.str.contains(r" D$")).then(pl.lit("Defense"))
            .when(pos_grp.str.contains(r"\d(WR|TE|RB)")).then(pl.lit("Offense")))


def build_depth_charts(schedules, refresh):
    """Weekly GSIS charts (2001-2024) and daily ESPN snapshots (2025+),
    stacked with harmonized columns (see snapshot_rows for the 2025+ format).
    Weekly rows are kept as delivered, minus exact duplicate rows.
    """
    weekly, snaps = [], []
    for s in DEPTH_CHART_SEASONS:
        df = fetch_season("depth_charts", nfl.load_depth_charts, s, refresh)
        if "dt" in df.columns:
            snaps.append(df.with_columns(season=pl.lit(s, dtype=pl.Int32)))
        else:
            weekly.append(df)

    old = pl.concat(weekly, how="diagonal_relaxed")
    n = old.height
    old = old.unique(maintain_order=True)
    print(f"  depth charts: dropped {n - old.height:,} exact duplicate weekly rows")
    old = add_franchise(old, "club_code").with_columns(
        depth_rank=pl.col("depth_team").cast(pl.Int32), chart_format=pl.lit("weekly"))
    frames = [old] + ([snapshot_rows(pl.concat(snaps, how="diagonal_relaxed"), schedules)]
                      if snaps else [])

    dc = pl.concat(frames, how="diagonal_relaxed")
    # Snapshot base defenses list a nickel back (NB) as a 12th slot; leaving
    # it out gives 11 starters per side, as in the weekly charts
    dc = add_season_type(dc).with_columns(
        starter=(pl.col("depth_rank") == 1) & pl.col("formation").is_in(["Offense", "Defense"])
        & ~((pl.col("chart_format") == "daily_snapshot") & (pl.col("depth_position") == "NB")))
    lead = ["season", "week", "game_type", "season_type", "franchise_id", "gsis_id",
            "depth_position", "formation", "depth_rank", "starter", "chart_format"]
    return dc.select(lead + [c for c in dc.columns if c not in lead])


def snapshot_rows(snap, schedules):
    """Daily snapshots -> one chart per team-game: the team's last snapshot
    before kickoff, which supplies week and game_type. The full daily
    snapshots stay in the raw cache."""
    snap = snap.with_columns(
        snapshot_at=pl.col("dt").str.to_datetime("%Y-%m-%dT%H:%M:%SZ", time_zone="UTC"))
    snap = add_franchise(snap, "team")
    games = team_games(schedules).filter(pl.col("season").is_in(snap["season"].unique().to_list()))
    stamps = snap.select("season", "franchise_id", "snapshot_at").unique().sort("snapshot_at")
    # Both sides are sorted on the time key, as join_asof requires within groups
    games = (games.sort("kickoff")
             .join_asof(stamps, left_on="kickoff", right_on="snapshot_at",
                        by=["season", "franchise_id"], strategy="backward",
                        check_sortedness=False))
    lag = games.select((pl.col("kickoff") - pl.col("snapshot_at")).dt.total_hours().alias("h"))
    print(f"  depth charts: {games.height} snapshot team-games, "
          f"{games['snapshot_at'].null_count()} without a prior snapshot, "
          f"hours from snapshot to kickoff max {lag['h'].max()} median {lag['h'].median()}")
    snap = snap.join(games.drop_nulls("snapshot_at").select(
        "season", "franchise_id", "snapshot_at", "week", "game_type"),
        on=["season", "franchise_id", "snapshot_at"], how="inner")
    # pos_rank orders every player of a position (the three WR slots share
    # one sequence); depth_rank is the order within the slot (pos_slot)
    snap = snap.with_columns(
        depth_position=pl.col("pos_abb"),
        depth_rank=pl.col("pos_rank").rank("dense")
        .over(["season", "franchise_id", "snapshot_at", "pos_grp", "pos_slot"]).cast(pl.Int32),
        formation=snapshot_formation(pl.col("pos_grp")), chart_format=pl.lit("daily_snapshot"))
    unknown = snap.filter(pl.col("formation").is_null())["pos_grp"].unique().to_list()
    if unknown:
        raise ValueError(f"unmapped depth-chart personnel groups: {unknown}")
    return snap


def build_trades(players, refresh):
    tr = cached_parquet("trades", nfl.load_trades, refresh)
    tr = add_franchise(tr, "gave", "gave_franchise_id")
    tr = add_franchise(tr, "received", "received_franchise_id")
    by_pfr = players.select("pfr_id", "gsis_id").drop_nulls("pfr_id")
    tr = tr.join(by_pfr, on="pfr_id", how="left", validate="m:1", maintain_order="left")
    return move_after(tr, "gsis_id", "pfr_id")


# ============================================================================
# Validation
# ============================================================================

KEYS = {
    "nfl_players": ["gsis_id"],
    "nfl_rosters_weekly": ["gsis_id", "season", "week", "game_type", "team"],
    "nfl_rosters_season": ["gsis_id", "season", "team"],
    "nfl_draft_picks": ["season", "pick"],
    "nfl_combine": ["gsis_id"],
    "nfl_ids": ["mfl_id"],
    "nfl_contracts": ["contract_id"],
    "nfl_contract_history": ["otc_id", "history_seq"],
    "nfl_contract_years": ["otc_id", "year"],
    "nfl_injuries": ["gsis_id", "season", "week", "game_type", "team"],
    "nfl_depth_charts": ["season", "week", "game_type", "franchise_id", "gsis_id",
                         "formation", "depth_position", "depth_rank"],
    "nfl_trades": ["trade_id", "gave", "received", "pick_season", "pick_round",
                   "pick_number", "pfr_id"],
}
SEASON_COL = {"nfl_players": "rookie_season", "nfl_combine": "season",
              "nfl_contracts": "year_signed", "nfl_contract_history": "year_signed",
              "nfl_contract_years": "year", "nfl_ids": "draft_year"}
# Configured windows that must be fully present
WINDOWS = {"nfl_rosters_weekly": NFL_WEEKLY_ROSTER_SEASONS, "nfl_rosters_season": NFL_SEASONS,
           "nfl_draft_picks": DRAFT_YEARS, "nfl_combine": DRAFT_YEARS,
           "nfl_injuries": NFL_INJURY_SEASONS, "nfl_depth_charts": DEPTH_CHART_SEASONS}
# Every team column and the franchise column derived from it
TEAM_COLS = {
    "nfl_players": [("latest_team", "latest_franchise_id"), ("draft_team", "draft_franchise_id")],
    "nfl_rosters_weekly": [("team", "franchise_id"), ("draft_club", "draft_franchise_id")],
    "nfl_rosters_season": [("team", "franchise_id"), ("draft_club", "draft_franchise_id")],
    "nfl_draft_picks": [("team", "franchise_id")],
    "nfl_combine": [("draft_team", "draft_franchise_id")],
    "nfl_ids": [("team", "franchise_id")],
    "nfl_contracts": [("team", "franchise_ids"), ("history_team", "history_franchise_id"),
                      ("draft_team", "draft_franchise_id")],
    "nfl_contract_history": [("team", "franchise_id")],
    "nfl_contract_years": [("team", "franchise_id")],
    "nfl_injuries": [("team", "franchise_id")],
    "nfl_depth_charts": [("club_code", "franchise_id"), ("team", "franchise_id")],
    "nfl_trades": [("gave", "gave_franchise_id"), ("received", "received_franchise_id")],
}


def q(con, sql):
    """Query result as polars; DuckDB HUGEINT sums shown as integers."""
    return con.execute(sql).pl().with_columns(pl.col(pl.Decimal).cast(pl.Int64))


def validate(con):
    print("\n" + "=" * 72 + "\nCoverage summary\n" + "=" * 72)
    pl.Config.set_tbl_rows(60)
    pl.Config.set_tbl_cols(30)
    pl.Config.set_tbl_width_chars(220)
    pl.Config.set_fmt_str_lengths(90)

    # Seasons covered (and configured seasons missing), key uniqueness
    rows, missing = [], {}
    for t, key in KEYS.items():
        scol = SEASON_COL.get(t, "season")
        # Rows without a gsis_id cannot be keyed; other NULLs group together
        where = "WHERE gsis_id IS NOT NULL" if "gsis_id" in key else ""
        s = con.execute(f"""SELECT count(*), min({scol}), max({scol}),
            count(DISTINCT {scol}) FROM "{t}" """).fetchone()
        have = {r[0] for r in con.execute(f'SELECT DISTINCT {scol} FROM "{t}"').fetchall()}
        gap = sorted(set(WINDOWS.get(t, [])) - have)
        if gap:
            missing[t] = gap
        dup = con.execute(f"""SELECT count(*) FROM (SELECT {', '.join(key)} FROM "{t}"
            {where} GROUP BY ALL HAVING count(*) > 1)""").fetchone()[0]
        rows.append(dict(table=t, rows=s[0], period=f"{s[1]}-{s[2]} ({s[3]})",
                         missing_seasons=len(gap), key=",".join(key), dup_keys=dup))
    print(pl.DataFrame(rows))

    # franchise_id coverage of every team column: rows with a team code, the
    # share left unmapped, and the unmapped codes
    print("\nTeam columns: rows with a code, franchise_id NULL share, unmapped codes")
    rows = []
    for t, pairs in TEAM_COLS.items():
        for team, fid in pairs:
            n, bad = con.execute(f"""SELECT count(*), count_if({fid} IS NULL) FROM "{t}"
                WHERE {team} IS NOT NULL AND {team} <> ''""").fetchone()
            codes = con.execute(f"""SELECT {team} || ' ' || count(*) FROM "{t}"
                WHERE {team} <> '' AND {fid} IS NULL GROUP BY {team}
                ORDER BY count(*) DESC LIMIT 8""").fetchall()
            rows.append(dict(column=f"{t}.{team}", with_code=n, null_share=bad / n,
                             unmapped=", ".join(c[0] for c in codes)))
    print(pl.DataFrame(rows))
    if missing:
        raise ValueError(f"configured seasons missing: {missing}")

    print("\nWeekly rosters: rows by status and era")
    print(q(con, """SELECT CASE WHEN season <= 2015 THEN '2002-2015' WHEN season = 2016
            THEN '2016' ELSE '2017-2025' END AS era, status, count(*) AS n
        FROM nfl_rosters_weekly GROUP BY ALL""")
          .pivot(on="era", index="status", values="n", sort_columns=True)
          .sort("2017-2025", descending=True, nulls_last=True))
    # Before 2016, game-day inactives are ACT rows with status_description_abbr
    # I01/I02 (act_i: they never appear in snap counts); from 2019 they are INA
    print("  mean players per REG team-week by status (is_key_primary rows):")
    print(q(con, """SELECT season, round(avg(n_act), 1) AS act, round(avg(n_act_i), 1) AS act_i,
            round(avg(n_ina), 1) AS ina, round(avg(n_dev), 1) AS dev, round(avg(n_res), 1) AS res
        FROM (SELECT season, week, franchise_id, count_if(status = 'ACT') AS n_act,
                count_if(status = 'ACT' AND status_description_abbr LIKE 'I%') AS n_act_i,
                count_if(status = 'INA') AS n_ina, count_if(status = 'DEV') AS n_dev,
                count_if(status = 'RES') AS n_res
              FROM nfl_rosters_weekly WHERE is_key_primary AND game_type = 'REG'
              GROUP BY ALL) GROUP BY 1 ORDER BY 1"""))
    print(q(con, """SELECT count(*) AS n_rows, count_if(key_rows > 1) AS rows_in_dup_keys,
            count_if(is_key_primary) AS primary_rows, count_if(gsis_id IS NULL) AS no_gsis,
            avg((position_group IS NULL)::INT) AS pos_group_null,
            avg(coalesce(gsis_id IN (SELECT gsis_id FROM nfl_players), false)::INT) AS in_players
        FROM nfl_rosters_weekly"""))
    print(q(con, """SELECT (SELECT count(*) FROM (SELECT gsis_id, season, week, game_type, team
                FROM nfl_rosters_weekly WHERE is_key_primary GROUP BY ALL HAVING count(*) > 1))
            AS primary_dup_keys,
            (SELECT count(*) FROM (SELECT gsis_id, season, week, game_type
                FROM nfl_rosters_weekly WHERE is_key_primary GROUP BY ALL HAVING count(*) > 1))
            AS player_weeks_on_two_teams"""))

    print("\nPlayers by rookie season (draft info among drafted players per nfl_draft_picks)")
    print(q(con, """SELECT CASE WHEN rookie_season < 2000 THEN '<2000' WHEN rookie_season < 2010
            THEN '2000-09' WHEN rookie_season < 2016 THEN '2010-15' ELSE '2016+' END AS band,
            count(*) AS n, avg((d.gsis_id IS NOT NULL)::INT) AS drafted,
            avg((draft_round IS NOT NULL)::INT) FILTER (WHERE d.gsis_id IS NOT NULL) AS round_if_drafted,
            avg((college_name IS NOT NULL)::INT) AS has_college, avg((pfr_id IS NOT NULL)::INT) AS has_pfr,
            avg((otc_id IS NOT NULL)::INT) AS has_otc,
            count_if(weight < 150 OR weight > 400) AS odd_weight,
            count_if(height < 64 OR height > 84) AS odd_height
        FROM nfl_players p LEFT JOIN (SELECT DISTINCT gsis_id FROM nfl_draft_picks) d USING (gsis_id)
        GROUP BY 1 ORDER BY min(rookie_season)"""))

    print("\nDraft picks: gsis_id and players link by decade")
    print(q(con, """SELECT season // 10 * 10 AS draft_decade, count(*) AS n,
            avg((gsis_id IS NOT NULL)::INT) AS has_gsis,
            avg(coalesce(gsis_id IN (SELECT gsis_id FROM nfl_players), false)::INT) AS in_players
        FROM nfl_draft_picks GROUP BY 1 ORDER BY 1"""))

    print("\nCombine: link rate by position group and method")
    print(q(con, """SELECT CASE WHEN grouping(position_group) = 1 THEN 'ALL'
            ELSE coalesce(position_group, '?') END AS grp, count(*) AS n,
            avg((link_method = 'pfr_id')::INT) AS pfr_id,
            avg((link_method = 'draft_slot')::INT) AS draft_slot,
            avg((link_method = 'name_pos_year')::INT) AS name_pos_year,
            avg((gsis_id IS NOT NULL)::INT) AS linked,
            avg((gsis_id IS NOT NULL)::INT) FILTER (WHERE draft_ovr IS NOT NULL) AS linked_drafted,
            avg((gsis_id IS NOT NULL)::INT) FILTER (WHERE draft_ovr IS NULL) AS linked_undrafted
        FROM nfl_combine GROUP BY ROLLUP (position_group) ORDER BY 1"""))
    print("  drafted players (draft 2000-2026) with a linked combine row:")
    print(q(con, """SELECT p.position_group, count(*) AS drafted,
            avg((c.gsis_id IS NOT NULL)::INT) AS with_combine_row
        FROM nfl_draft_picks p LEFT JOIN (SELECT DISTINCT gsis_id FROM nfl_combine) c USING (gsis_id)
        WHERE p.season BETWEEN 2000 AND 2026 AND p.gsis_id IS NOT NULL GROUP BY 1 ORDER BY 1"""))

    print("\nContracts")
    print(q(con, """SELECT count(*) AS contracts, count(DISTINCT otc_id) AS players,
            avg((gsis_id_otc IS NOT NULL)::INT) AS gsis_source, avg((gsis_id IS NOT NULL)::INT) AS gsis_any,
            avg(coalesce(gsis_id IN (SELECT gsis_id FROM nfl_players), false)::INT) AS in_players,
            min(year_signed) FILTER (WHERE NOT year_signed_missing) AS min_year,
            max(year_signed) AS max_year, count_if(year_signed_missing) AS year_signed_0,
            count_if(n_teams > 1) AS multi_team, avg((contract_type IS NOT NULL)::INT) AS has_type,
            avg((guaranteed = 0)::INT) AS guaranteed_zero
        FROM nfl_contracts"""))
    print("  contracts per (otc_id, year_signed); the old (otc_id, year_signed) key kept one:")
    print(q(con, """SELECT n_contracts, count(*) AS player_years, sum(n_contracts) AS contracts,
            sum(n_contracts - 1) AS collapsed_by_old_key
        FROM (SELECT otc_id, year_signed, count(*) AS n_contracts FROM nfl_contracts GROUP BY ALL)
        GROUP BY ROLLUP (1) ORDER BY 1"""))
    print(q(con, """SELECT history_match, count(*) AS n, count_if(year_signed_missing) AS year_signed_0,
            count_if(identical_rows > 1) AS identical_terms,
            count_if(otc_id NOT IN (SELECT otc_id FROM nfl_contract_history)) AS player_has_no_history
        FROM nfl_contracts GROUP BY 1 ORDER BY 2 DESC"""))
    print(q(con, """SELECT contract_type, count(*) AS n, round(median(apy), 3) AS median_apy
        FROM nfl_contracts GROUP BY 1 ORDER BY 2 DESC"""))
    print(q(con, """SELECT count(*) AS entries, avg((contract_id IS NOT NULL)::INT) AS matched_to_contract
        FROM nfl_contract_history"""))
    print("  cap table (nfl_contract_years), realized seasons by 5-year band:")
    print(q(con, """SELECT year // 5 * 5 AS band_start, count(*) AS player_years,
            avg((cap_number IS NOT NULL)::INT) AS has_cap, avg((cash_paid IS NOT NULL)::INT) AS has_cash,
            avg((base_salary IS NOT NULL)::INT) AS has_base,
            avg((contract_id IS NOT NULL)::INT) AS has_contract,
            avg((gsis_id IS NOT NULL)::INT) AS has_gsis
        FROM nfl_contract_years WHERE NOT is_projected GROUP BY 1 ORDER BY 1"""))

    print("\nInjuries / trades links")
    print(q(con, """SELECT count(*) AS n, avg(coalesce(gsis_id IN (SELECT gsis_id FROM nfl_players), false)::INT) AS in_players,
            avg((position_group IS NULL)::INT) AS pos_group_null FROM nfl_injuries"""))
    print(q(con, """SELECT count(*) AS n_rows, count_if(pfr_id <> '') AS player_rows,
            avg((gsis_id IS NOT NULL)::INT) FILTER (WHERE pfr_id <> '') AS gsis_linked FROM nfl_trades"""))

    print("\nDepth charts: starter identification by season (REG team-games from schedules)")
    print(q(con, """WITH dc AS (SELECT season, week, franchise_id,
                count(DISTINCT gsis_id) FILTER (WHERE starter AND formation = 'Offense') AS off_st,
                count(DISTINCT gsis_id) FILTER (WHERE starter AND formation = 'Defense') AS def_st
              FROM nfl_depth_charts WHERE game_type = 'REG' GROUP BY ALL)
        SELECT season, count(*) AS team_games, avg((dc.franchise_id IS NOT NULL)::INT) AS has_chart,
            round(avg(off_st), 1) AS off_starters, round(avg(def_st), 1) AS def_starters,
            avg((coalesce(off_st, 0) >= 11 AND coalesce(def_st, 0) >= 11)::INT) AS full_22
        FROM _reg_team_games g LEFT JOIN dc USING (season, week, franchise_id)
        GROUP BY 1 ORDER BY 1"""))


def main():
    ap = argparse.ArgumentParser(description="Load nflverse player-side data")
    ap.add_argument("--refresh", action="store_true", help="re-download every nflverse file")
    args = ap.parse_args()
    refresh = args.refresh
    con = connect()

    print("Draft picks...")
    draft = build_draft_picks(refresh)
    write_table(con, "nfl_draft_picks", draft, source=f"{SRC} load_draft_picks(True)",
                note="all drafts since 1980")
    print("Players...")
    players = build_players(draft, refresh)
    write_table(con, "nfl_players", players, source=f"{SRC} load_players",
                note="one row per gsis_id; position_group from positions.py")
    print("Weekly rosters...")
    write_table(con, "nfl_rosters_weekly", build_rosters_weekly(refresh),
                source=f"{SRC} load_rosters_weekly",
                note="all statuses; is_key_primary = one row per gsis_id-season-week-game_type-team")
    print("Season rosters...")
    write_table(con, "nfl_rosters_season", build_rosters_season(refresh),
                source=f"{SRC} load_rosters", note="is_key_primary = one row per gsis_id-season-team")
    print("Combine...")
    write_table(con, "nfl_combine", build_combine(players, draft, refresh),
                source=f"{SRC} load_combine(True)", note="gsis_id via pfr_id > draft_slot > name_pos_year")
    print("ID crosswalk...")
    write_table(con, "nfl_ids", build_ids(refresh), source=f"{SRC} load_ff_playerids")
    print("Contracts...")
    contracts, history, years = build_contracts(players, refresh)
    write_table(con, "nfl_contracts", contracts, source=f"{SRC} load_contracts (OverTheCap)",
                note="one row per contract; contract_id = otc_id-year_signed-seq")
    write_table(con, "nfl_contract_history", history, source=f"{SRC} load_contracts contract_history",
                note="player-level contract history entries")
    write_table(con, "nfl_contract_years", years, source=f"{SRC} load_contracts season_history",
                note="otc_id x year cap table; years > LAST_SEASON are projections")
    print("Injuries...")
    write_table(con, "nfl_injuries", build_injuries(refresh), source=f"{SRC} load_injuries")
    print("Depth charts...")
    schedules = cached_parquet("schedules", lambda: nfl.load_schedules(NFL_SEASONS), refresh)
    write_table(con, "nfl_depth_charts", build_depth_charts(schedules, refresh),
                source=f"{SRC} load_depth_charts",
                note="weekly charts 2001-2024; 2025+ = last daily snapshot before kickoff")
    print("Trades...")
    write_table(con, "nfl_trades", build_trades(players, refresh), source=f"{SRC} load_trades")

    reg_games = (team_games(schedules)
                 .filter((pl.col("game_type") == "REG") & pl.col("season").is_in(DEPTH_CHART_SEASONS))
                 .select("season", "week", "franchise_id"))
    con.register("_reg_team_games", reg_games.to_arrow())
    validate(con)
    con.close()


if __name__ == "__main__":
    main()
