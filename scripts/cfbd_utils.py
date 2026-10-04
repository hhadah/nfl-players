"""Helpers shared by the CFBD loaders (03_load_college.py, 04_load_recruiting.py).

- pull_years(): one cached CFBD call per season -> {year: rows}.
- stack(): concatenate those into one frame with a checked `season` column.
- frame(): flatten nested JSON objects into snake_case columns (lists kept).
- drop_repeats(): drop source rows repeated on a key that differ only in a
  blank field (fails if the repeats disagree on anything else).
- as_id(): cast CFBD id strings to nullable integers (fails on non-numeric).
- clean_height()/clean_weight(): NULL out implausible measurables.
- position_group_cfbd(): positions.position_group() after mapping CFBD labels
  (247 codes PRO, DUAL, WDE, SDE, OC, APB; spelled-out draft positions).
- split_name(): first / last name / suffix from a single 'name' string.
- key_report(): rows, seasons covered and key uniqueness of a written table.
"""
import re
import time

import pandas as pd

from positions import position_group

# Plausible ranges for self-reported measurables (inches, pounds).
HEIGHT_RANGE = (60, 84)
WEIGHT_RANGE = (130, 420)

# CFBD position labels that positions.py does not know. The 247 recruiting
# codes changed in the 2021 class (PRO/DUAL -> QB, WDE/SDE -> EDGE, DT -> DL,
# ILB/OLB -> LB, OG/OC -> IOL); mapping both vintages to one group keeps
# position comparable across recruiting classes. /draft/picks spells
# positions out in words. ATH (athlete) and returners have no position.
CFBD_POSITION_ALIAS = {
    "PRO": "QB", "DUAL": "QB", "APB": "RB", "WDE": "DE", "SDE": "DE", "OC": "C",
    "PK": "K", "DS": "LS",
    "QUARTERBACK": "QB", "RUNNING BACK": "RB", "FULLBACK": "FB",
    "WIDE RECEIVER": "WR", "TIGHT END": "TE", "OFFENSIVE TACKLE": "OT",
    "OFFENSIVE GUARD": "OG", "CENTER": "C", "DEFENSIVE TACKLE": "DT",
    "DEFENSIVE END": "DE", "DEFENSIVE EDGE": "EDGE", "LINEBACKER": "LB",
    "OUTSIDE LINEBACKER": "OLB", "INSIDE LINEBACKER": "ILB", "CORNERBACK": "CB",
    "SAFETY": "S", "DEFENSIVE BACK": "DB", "PLACE KICKER": "K", "PUNTER": "P",
    "LONG SNAPPER": "LS"}

NAME_SUFFIXES = {"jr", "jr.", "sr", "sr.", "ii", "iii", "iv", "v"}


def snake(name):
    """camelCase / dotted JSON key -> snake_case column name."""
    # A plural acronym is one word: passingTDs -> passing_tds, not passing_t_ds
    s = re.sub(r"([A-Z]{2,})s(?![a-z])", lambda m: m.group(1).capitalize() + "s", name)
    s = re.sub(r"(?<=[a-z0-9])([A-Z])", r"_\1", s)
    s = re.sub(r"([A-Z]+)([A-Z][a-z])", r"\1_\2", s)
    return re.sub(r"[^0-9a-zA-Z]+", "_", s).strip("_").lower()


def frame(rows, **const):
    """Flatten a list of JSON objects (nested dicts become prefix_key
    columns, lists stay lists) and add constant columns."""
    df = pd.json_normalize(rows, sep="_") if rows else pd.DataFrame()
    df.columns = [snake(c) for c in df.columns]
    for k, v in const.items():
        df[k] = v
    return df


def as_id(series):
    """CFBD ids arrive as strings; cast to Int64 (raises if non-numeric)."""
    return pd.to_numeric(series, errors="raise").astype("Int64")


def clean_height(s):
    s = pd.to_numeric(s, errors="coerce")
    return s.where(s.between(*HEIGHT_RANGE))


def clean_weight(s):
    s = pd.to_numeric(s, errors="coerce")
    return s.where(s.between(*WEIGHT_RANGE))


def position_group_cfbd(code):
    if code is None or pd.isna(code):
        return None
    code = str(code).strip().upper()
    return position_group(CFBD_POSITION_ALIAS.get(code, code))


def split_name(name):
    """'Amon-Ra St. Brown Jr.' -> ('Amon-Ra', 'St. Brown', 'Jr.').

    First token is the first name, a trailing generational suffix is split
    off (with the comma of 'Smith, Jr.'), and everything in between is the
    last name.
    """
    if name is None or pd.isna(name):
        return None, None, None
    parts = str(name).split()
    suffix = None
    if len(parts) > 2 and parts[-1].lower().rstrip(",") in NAME_SUFFIXES:
        suffix = parts.pop()
    if len(parts) == 1:
        return parts[0], None, suffix
    return parts[0], " ".join(parts[1:]).rstrip(","), suffix


def pull_years(api, endpoint, years, refresh=False, pause=0.5, **params):
    """Call `endpoint` once per year (cached) and return {year: rows}.

    Sleeps `pause` seconds after each live call to be polite to the API.
    """
    out = {}
    for year in years:
        before = api.live_calls
        out[year] = api.get(endpoint, refresh=refresh, year=year, **params)
        if api.live_calls > before:
            time.sleep(pause)
    n = sum(len(v) for v in out.values())
    empty = [y for y, v in out.items() if not v]
    tag = " ".join(f"{k}={v}" for k, v in params.items())
    print(f"  {endpoint} {tag}: {n:,} rows over {len(years)} years"
          + (f"; EMPTY years: {empty}" if empty else ""))
    return out


def stack(by_year, year_field=None, **const):
    """Concatenate {year: rows} into one flattened frame with a `season` column.

    Without `year_field` the requested year becomes `season`. With it (the
    payload's own year/season field), every row must carry the requested
    year -- raises otherwise -- and that field is renamed to `season`.
    """
    parts = []
    for year, rows in by_year.items():
        if not rows:
            continue
        df = frame(rows, **const)
        if year_field is None:
            df["season"] = year
        elif df[year_field].ne(year).any():
            raise ValueError(f"rows requested for {year} carry other {year_field} values")
        else:
            df = df.rename(columns={year_field: "season"})
        parts.append(df)
    return pd.concat(parts, ignore_index=True) if parts else pd.DataFrame()


def drop_repeats(df, key, may_differ, label):
    """Drop source rows that repeat `key` and differ only in the `may_differ`
    columns (e.g. a blank conference), keeping the most complete copy.
    Raises if the repeats disagree on any other column."""
    dup = df.duplicated(key, keep=False)
    check = [c for c in df.columns if c not in key and c not in may_differ]
    if dup.any() and df[dup].groupby(key)[check].nunique(dropna=False).gt(1).any().any():
        raise ValueError(f"{label}: rows repeat {key} with different values")
    filled = df.replace("", pd.NA).notna().sum(axis=1)
    out = (df.assign(_filled=filled).sort_values("_filled", ascending=False, kind="stable")
             .drop_duplicates(key).drop(columns="_filled").sort_index())
    if len(out) < len(df):
        print(f"  {label}: dropped {len(df) - len(out)} repeated rows "
              f"(differing only in {', '.join(may_differ)})")
    return out


def key_report(con, table, key, season_col=None):
    """Print rows, seasons covered and whether `key` is unique in `table`."""
    cols = ", ".join(key)
    n, n_key = con.execute(
        f'SELECT count(*), count(DISTINCT ({cols})) FROM "{table}"').fetchone()
    n_null = con.execute(
        f'SELECT count(*) FROM "{table}" WHERE ' +
        " OR ".join(f"{k} IS NULL" for k in key)).fetchone()[0]
    line = f"  {table:<22} rows={n:>9,}  key=({cols}) unique={n == n_key and n_null == 0}"
    if n_null:
        line += f" [NULL key parts: {n_null:,}]"
    if season_col:
        lo, hi, k = con.execute(
            f'SELECT min({season_col}), max({season_col}), count(DISTINCT {season_col}) '
            f'FROM "{table}"').fetchone()
        line += f"  {season_col} {lo}-{hi} ({k} distinct)"
    print(line)
    return n == n_key and n_null == 0
