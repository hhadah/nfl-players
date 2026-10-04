"""Name-based race probabilities (BIFSG) for every person table in the DB.

SECONDARY race measure: the primary measure is human hand-coding. Read the
race_utils.py docstring (model, data handling, limits) before using these
columns in an analysis.

Source tables are never modified. Every entity table that exists is read,
reduced to one row per person id, and scored; absent tables are skipped with
a message. Entities and id columns (the first id column found is used):
  nfl_players       gsis_id           first_name (legal), common_first_name
                                      (preferred, used if the legal name is
                                      not in the first-name list), last_name
  staff_persons     person_id         first_name, last_name / person_name
  college_players   CFBD athlete id   first_name, last_name
  recruits          CFBD recruit id   first_name, last_name / name
  college_coaches   CFBD coach id     first_name, last_name (the name
                                      itself is the id if there is no id)
  cfbd_draft_picks  year-overall      name
Where first/last name columns are missing or NULL they are split from the
full-name column ('First [Middle] Last [Suffix]').

Reference files: data/raw/reference/ (run 00_fetch_reference.py first; this
script fails if they are missing or do not match SOURCES.json).

Tables written (full refresh):
  race_bifsg      one row per (entity, entity_id): first_name_used,
                  last_name_used, p_white, p_black, p_hispanic, p_api, p_aian,
                  p_multi (0-1, sum to 1), race_bifsg (argmax),
                  race_surname_only (argmax of the surname-only Census
                  posterior), first_name_matched, surname_matched, method
                  (bifsg / surname_only / first_name_only / no_name).
  race_bifsg_geo  same columns plus county_fips and county_matched, for the
                  entities whose table has a hometown county FIPS column
                  (CFBD rosters, recruits, draft picks): names x 2010 Census
                  county race composition. method ends in '+county' where
                  the county was used; other rows equal race_bifsg. A
                  county-only code gets the home state's prefix; a county
                  outside the stated U.S. home state (a CFBD geocoder error)
                  is set to NULL.
"""
import pandas as pd

from common import connect, write_table
from race_utils import (PROB_COLS, RACES, county_fips5, load_county, load_reference,
                        predict_race, split_full_name)

# Candidate columns per entity, in order of preference (CFBD tables may keep
# the API's camelCase names). `ids` lists alternative id keys; a key with
# several columns is joined with '-'.
FIRST, LAST = ["first_name", "firstName"], ["last_name", "lastName"]
ENTITIES = [
    dict(table="nfl_players", ids=[("gsis_id",)],
         first=["first_name"], first_alt=["common_first_name", "football_name"],
         last=["last_name"], full=["display_name", "full_name", "player_name"]),
    dict(table="staff_persons", ids=[("person_id",)],
         first=FIRST, last=LAST, full=["person_name", "full_name", "name"]),
    dict(table="college_players",
         ids=[("athlete_id",), ("player_id",), ("cfbd_id",), ("id",)],
         first=FIRST, last=LAST, full=["full_name", "name", "player_name"]),
    dict(table="recruits", ids=[("recruit_id",), ("id",)],
         first=FIRST, last=LAST, full=["name", "full_name"]),
    dict(table="college_coaches",
         ids=[("coach_id",), ("id",), ("first_name", "last_name"), ("firstName", "lastName")],
         first=FIRST, last=LAST, full=["full_name", "coach_name", "name"]),
    dict(table="cfbd_draft_picks",
         ids=[("pick_id",), ("year", "overall"), ("season", "overall"),
              ("draft_year", "overall")],
         first=FIRST, last=LAST, full=["name", "player_name", "full_name"]),
]

OUT_COLS = ["entity", "entity_id", "first_name_used", "last_name_used", *PROB_COLS,
            "race_bifsg", "race_surname_only", "first_name_matched",
            "surname_matched", "method"]
GEO_COLS = OUT_COLS + ["county_fips", "county_matched"]

# Hometown state columns (CFBD rosters, recruits) and USPS -> state FIPS, used
# to drop county codes that the CFBD geocoder placed in the wrong state
# (e.g. Saint Rose, LA coded to Clinton County, IL).
STATE_COLS = ["home_state", "state_province", "hometown_state", "hometown_info_state",
              "hometown_info_state_province", "state"]
STATE_FIPS = dict(
    AL="01", AK="02", AZ="04", AR="05", CA="06", CO="08", CT="09", DE="10", DC="11",
    FL="12", GA="13", HI="15", ID="16", IL="17", IN="18", IA="19", KS="20", KY="21",
    LA="22", ME="23", MD="24", MA="25", MI="26", MN="27", MS="28", MO="29", MT="30",
    NE="31", NV="32", NH="33", NJ="34", NM="35", NY="36", NC="37", ND="38", OH="39",
    OK="40", OR="41", PA="42", RI="44", SC="45", SD="46", TN="47", TX="48", UT="49",
    VT="50", VA="51", WA="53", WV="54", WI="55", WY="56", AS="60", GU="66", MP="69",
    PR="72", VI="78")


def pick(columns, candidates):
    return next((c for c in candidates if c in columns), None)


def fips_column(columns):
    """Hometown county FIPS column (e.g. home_county_fips,
    hometown_info_fips_code), preferring names that mention 'county'."""
    fips = sorted(c for c in columns if "fips" in c.lower())
    return next((c for c in fips if "county" in c.lower()), fips[0] if fips else None)


def load_county_fips(con, table, id_expr, fips_col, state_col):
    """Most frequent (county FIPS, home state) per id, ties broken by the codes
    so that reruns agree. A county-only code ('053') gets the home state's
    prefix; a county whose state prefix disagrees with a known U.S. home state
    is set to NULL. Returns (frame of entity_id, county_fips; number of ids
    dropped)."""
    state = f'upper(CAST("{state_col}" AS VARCHAR))' if state_col else "NULL"
    geo = con.execute(f"""
        SELECT {id_expr} AS entity_id, CAST("{fips_col}" AS VARCHAR) AS county_fips,
               {state} AS state, count(*) AS n_rows
        FROM "{table}" WHERE "{fips_col}" IS NOT NULL GROUP BY ALL""").df()
    geo = (geo.sort_values(["entity_id", "n_rows", "county_fips", "state"],
                           ascending=[True, False, True, True], na_position="last")
              .drop_duplicates("entity_id"))
    state_fips = [STATE_FIPS.get(s) for s in geo["state"]]
    fips = [county_fips5(f, s) for f, s in zip(geo["county_fips"], state_fips)]
    bad = [bool(f and s and f[:2] != s) for f, s in zip(fips, state_fips)]
    geo["county_fips"] = [None if b else f for f, b in zip(fips, bad)]
    return geo[["entity_id", "county_fips"]], sum(bad)


def load_entity(con, spec):
    """One row per entity id with first / first_alt / last name columns and,
    if the table has one, the most frequent hometown county FIPS. Returns
    (frame, description of columns), or (None, None) if the table is absent."""
    tables = {t for (t,) in con.execute("SHOW TABLES").fetchall()}
    if spec["table"] not in tables:
        return None, None
    columns = {c for (c, *_) in con.execute(f'DESCRIBE "{spec["table"]}"').fetchall()}
    id_cols = next((key for key in spec["ids"] if set(key) <= columns), None)
    if id_cols is None:
        raise KeyError(f"{spec['table']}: none of the id columns {spec['ids']} exist")
    cols = {role: pick(columns, spec.get(role, []))
            for role in ("first", "first_alt", "last", "full")}
    if not (cols["last"] or cols["full"]):
        raise KeyError(f"{spec['table']}: no surname or full-name column found")

    # Distinct (id, names) combinations with their row counts.
    id_expr = " || '-' || ".join(f'CAST("{c}" AS VARCHAR)' for c in id_cols)
    name_exprs = [f'CAST("{c}" AS VARCHAR) AS {role}' if c else f"NULL::VARCHAR AS {role}"
                  for role, c in cols.items()]
    df = con.execute(f"""
        SELECT {id_expr} AS entity_id, {', '.join(name_exprs)}, count(*) AS n_rows
        FROM "{spec['table']}" GROUP BY ALL""").df()
    n_null_id = int(df.loc[df["entity_id"].isna(), "n_rows"].sum())
    df = df[df["entity_id"].notna()]

    # If an id carries several spellings, keep the most frequent one (ties
    # broken alphabetically, so reruns agree).
    df = (df.sort_values(["entity_id", "n_rows", "last", "first", "first_alt", "full"],
                         ascending=[True, False, True, True, True, True],
                         na_position="last")
            .drop_duplicates("entity_id").reset_index(drop=True))

    fips_col = fips_column(columns)
    n_bad_state = 0
    if fips_col:
        geo, n_bad_state = load_county_fips(con, spec["table"], id_expr, fips_col,
                                            pick(columns, STATE_COLS))
        df = df.merge(geo, on="entity_id", how="left")

    # Fill missing first/last names from the full-name column.
    split = df["full"].map(split_full_name)
    df["first"] = df["first"].where(df["first"].notna(), split.str[0])
    df["last"] = df["last"].where(df["last"].notna(), split.str[1])
    desc = (f"id={'+'.join(id_cols)}; " +
            "; ".join(f"{role}={c}" for role, c in cols.items() if c) +
            (f"; county={fips_col} ({n_bad_state} dropped: FIPS outside home state)"
             if fips_col else "") +
            (f"; {n_null_id} rows with NULL id dropped" if n_null_id else ""))
    return df, desc


def summarize(out):
    """Coverage and label distribution per entity."""
    g = out.groupby("entity", sort=False)
    coverage = {"county_matched_%": 100 * g["county_matched"].mean()} \
        if "county_matched" in out else {}
    summary = pd.DataFrame({
        "n": g.size(),
        "surname_matched_%": 100 * g["surname_matched"].mean(),
        "first_matched_%": 100 * g["first_name_matched"].mean(),
        **coverage,
        "mean_p_black": 100 * g["p_black"].mean(),
        **{f"{r}_%": 100 * g["race_bifsg"].apply(lambda x, r=r: (x == r).mean())
           for r in RACES},
        "black_surname_only_%": 100 * g["race_surname_only"].apply(
            lambda x: (x == "black").mean()),
    })
    return summary.round(1)


def main():
    ref = load_reference()
    print("Reference tables:")
    for name, rec in ref["sources"].items():
        print(f"  {name}: sha256 {rec['sha256'][:16]}... from {rec['source_url']}")
    print(f"  Dirichlet smoothing: {ref['census_alpha']:.2f} pseudo-obs (surnames), "
          f"{ref['first_alpha']:.2f} (first names)")
    print("  first-name file marginal P_H(r): " +
          ", ".join(f"{r} {100 * p:.2f}%" for r, p in zip(RACES, ref["first_marginal"])))
    geo_ref = load_county()
    print(f"  county table: {len(geo_ref['county']):,} counties, "
          f"{geo_ref['county_alpha']:.2f} pseudo-obs smoothing")

    con = connect()
    frames, geo_frames, notes, geo_notes = [], [], [], []
    for spec in ENTITIES:
        df, desc = load_entity(con, spec)
        if df is None:
            print(f"  {spec['table']}: table not in DB -- skipped")
            continue
        print(f"  {spec['table']}: {len(df):,} persons ({desc})")
        names = (df["last"].tolist(), df["first"].tolist(), df["first_alt"].tolist())
        ids = dict(entity=spec["table"], entity_id=df["entity_id"].to_numpy())
        frames.append(predict_race(*names).assign(**ids))
        notes.append(f"{spec['table']}: {desc}")
        if "county_fips" in df:
            geo = predict_race(*names, county_fips=df["county_fips"].tolist())
            geo_frames.append(geo.assign(**ids))
            geo_notes.append(notes[-1])
    if not frames:
        raise RuntimeError("No entity tables found: run the loaders first")

    out = pd.concat(frames, ignore_index=True)[OUT_COLS]
    if out.duplicated(["entity", "entity_id"]).any():
        raise RuntimeError("race_bifsg key (entity, entity_id) is not unique")
    write_table(con, "race_bifsg", out,
                source="Census 2010 surnames + Tzioumis (2018) first names; "
                       "BIFSG without geography (scripts/race_utils.py)",
                note=" | ".join(notes))
    if geo_frames:
        geo_out = pd.concat(geo_frames, ignore_index=True)[GEO_COLS]
        write_table(con, "race_bifsg_geo", geo_out,
                    source="race_bifsg inputs + 2010 Census county race counts "
                           "(CC-EST2019 YEAR=1); BIFSG with county",
                    note=" | ".join(geo_notes))
    else:
        # Full refresh: no entity has a county column, so no stale table.
        print("  no entity table has a county FIPS column: race_bifsg_geo not built")
        con.execute("DROP TABLE IF EXISTS race_bifsg_geo")
        con.execute("DELETE FROM _build_log WHERE table_name = 'race_bifsg_geo'")
    con.close()

    print("\nrace_bifsg: coverage and predicted shares (%), by entity:")
    with pd.option_context("display.width", 220, "display.max_columns", 30):
        print(summarize(out).to_string())
        print("\nmethod counts:")
        print(out.groupby(["entity", "method"], sort=False).size().to_string())
        if geo_frames:
            print("\nrace_bifsg_geo (names + home county): coverage and shares (%):")
            print(summarize(geo_out).to_string())


if __name__ == "__main__":
    main()
