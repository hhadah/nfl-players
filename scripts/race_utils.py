"""Surname → race / ethnicity inference, with optional first-name signal.

Two reference tables:
  * Census 2010 surname file — surname → race percentages.
    Source: U.S. Census Bureau, frequencies for surnames with ≥100 occurrences.
  * Tzioumis (2018) first-names file — firstname → race percentages.
    Source: doi:10.7910/DVN/TYJKEZ (Harvard Dataverse), 4,250 first names
    derived from HMDA mortgage records.

When `infer_race` is called with both first AND last name we combine the two
signals using BIFSG-style Bayes:

    P(race | first, last) ∝ P(race | first) * P(race | last) / P(race)

with P(race) = U.S. population marginal. This is the standard approach used
in the academic literature (Tzioumis 2018, Voicu 2018, Imai & Khanna 2016)
when geocoding is not available.

If the first-name table is missing or the first name doesn't match, we fall
back to surname-only Census inference. If even the surname doesn't match the
prediction columns are NaN (no fabrication).

Output columns appended to a DataFrame:
  race_white, race_black, race_api, race_aian, race_2prace, race_hispanic
  race_pred  (one of: white, black, api, aian, 2prace, hispanic; NULL if no match)
  race_source  ("bifsg" if both signals merged, "surname" if last-only,
                "firstname" if first-only fallback, NULL if no match)
"""
from __future__ import annotations
import re
import unicodedata
from pathlib import Path

import numpy as np
import pandas as pd

DATA_DIR = Path(__file__).resolve().parent.parent / "data" / "raw"
CENSUS_CSV = DATA_DIR / "census_2010_surnames.csv"
TZIOUMIS_CSV = DATA_DIR / "tzioumis_firstnames.csv"

_PCT_COLS = ["pctwhite", "pctblack", "pctapi", "pctaian", "pct2prace", "pcthispanic"]
_OUT_COLS = ["race_white", "race_black", "race_api", "race_aian",
             "race_2prace", "race_hispanic"]
_LABELS = ["white", "black", "api", "aian", "2prace", "hispanic"]
_RENAME = dict(zip(_PCT_COLS, _OUT_COLS))

# U.S. population marginals (2020 ACS, used as the BIFSG prior).
# Order matches _LABELS. Sums to 1.
_PRIOR = np.array([0.601, 0.124, 0.061, 0.012, 0.022, 0.184])

_CENSUS_DF: pd.DataFrame | None = None
_TZIOUMIS_DF: pd.DataFrame | None = None


def _normalize_token(s):
    if not isinstance(s, str):
        return None
    n = unicodedata.normalize("NFKD", s).encode("ascii", "ignore").decode()
    n = re.sub(r"\b(jr|sr|ii|iii|iv|v)\b\.?", "", n, flags=re.I)
    n = re.sub(r"[^A-Za-z\s'\-]", "", n)
    n = re.sub(r"\s+", " ", n).strip().upper()
    return n or None


def _surname_from_full(s):
    """Last whitespace-separated token (after stripping suffixes)."""
    n = _normalize_token(s)
    if not n:
        return None
    parts = n.split()
    return parts[-1] if parts else None


def _firstname_from_full(s):
    n = _normalize_token(s)
    if not n:
        return None
    parts = n.split()
    return parts[0] if parts else None


def _load_table(path, name_col):
    df = pd.read_csv(path, dtype={name_col: str})
    df[name_col] = df[name_col].str.upper()
    for c in _PCT_COLS:
        if c in df.columns:
            df[c] = pd.to_numeric(df[c], errors="coerce")
    return df.set_index(name_col)[[c for c in _PCT_COLS if c in df.columns]]


def load_census() -> pd.DataFrame:
    global _CENSUS_DF
    if _CENSUS_DF is None:
        if not CENSUS_CSV.exists():
            raise FileNotFoundError(f"Census surname file not found at {CENSUS_CSV}")
        _CENSUS_DF = _load_table(CENSUS_CSV, "name")
    return _CENSUS_DF


def load_firstnames() -> pd.DataFrame | None:
    """Tzioumis first-name table, or None if not bundled."""
    global _TZIOUMIS_DF
    if _TZIOUMIS_DF is None:
        if not TZIOUMIS_CSV.exists():
            return None
        _TZIOUMIS_DF = _load_table(TZIOUMIS_CSV, "firstname")
    return _TZIOUMIS_DF


def _to_pct_array(df: pd.DataFrame) -> np.ndarray:
    """Pull the six percentage columns into a (n, 6) float array, NaN where
    the source had '(S)' suppression or didn't have the column at all."""
    arr = np.full((len(df), 6), np.nan)
    for j, c in enumerate(_PCT_COLS):
        if c in df.columns:
            arr[:, j] = df[c].values
    return arr


def _bifsg_combine(first_pct: np.ndarray, last_pct: np.ndarray) -> np.ndarray:
    """Combine row-aligned (n, 6) pct matrices via BIFSG.

      P(r | f, s) ∝ P(r | f) * P(r | s) / P(r)

    Inputs are percentages (0..100). Output is normalized percentages (0..100).
    Wherever a cell is NaN we treat it as "no information" — we replace it
    with the prior so it doesn't drag the product to zero.
    """
    prior_pct = _PRIOR * 100.0
    f = np.where(np.isnan(first_pct), prior_pct, first_pct)
    s = np.where(np.isnan(last_pct), prior_pct, last_pct)
    raw = (f * s) / prior_pct
    row_sums = raw.sum(axis=1, keepdims=True)
    safe = np.where(row_sums == 0, np.nan, row_sums)
    return raw / safe * 100.0


def infer_race(df: pd.DataFrame,
               last_col: str | None = None,
               first_col: str | None = None,
               full_col: str | None = None) -> pd.DataFrame:
    """Append race columns to df (LEFT-join semantics — no rows dropped).

    Pass any of:
      * last_col + first_col: per-column names. Both signals are used (BIFSG).
      * last_col only: surname-only Census lookup.
      * full_col: a single "First Last" string; we split on whitespace.
    """
    if not (last_col or full_col):
        raise ValueError("infer_race needs last_col, full_col, or both first_col+last_col")

    # Resolve first / last as Series.
    if last_col is not None:
        last_raw = df[last_col]
    else:
        last_raw = df[full_col].apply(_surname_from_full)
    if first_col is not None:
        first_raw = df[first_col]
    elif full_col is not None:
        first_raw = df[full_col].apply(_firstname_from_full)
    else:
        first_raw = pd.Series([None] * len(df), index=df.index)

    last = last_raw.apply(_normalize_token)
    first = first_raw.apply(_normalize_token)

    census = load_census()
    last_match = census.reindex(last.values)
    last_match.index = df.index
    last_pct = _to_pct_array(last_match)
    last_has = ~np.isnan(last_pct).all(axis=1)

    fn = load_firstnames()
    if fn is not None and first.notna().any():
        first_match = fn.reindex(first.values)
        first_match.index = df.index
        first_pct = _to_pct_array(first_match)
        first_has = ~np.isnan(first_pct).all(axis=1)
    else:
        first_pct = np.full((len(df), 6), np.nan)
        first_has = np.zeros(len(df), dtype=bool)

    # Decide combination per row.
    out_pct = np.full((len(df), 6), np.nan)
    source = np.array([None] * len(df), dtype=object)

    both = last_has & first_has
    if both.any():
        out_pct[both] = _bifsg_combine(first_pct[both], last_pct[both])
        source[both] = "bifsg"

    last_only = last_has & ~first_has
    if last_only.any():
        out_pct[last_only] = last_pct[last_only]
        source[last_only] = "surname"

    first_only = ~last_has & first_has
    if first_only.any():
        out_pct[first_only] = first_pct[first_only]
        source[first_only] = "firstname"

    out = pd.DataFrame(out_pct, columns=_OUT_COLS, index=df.index)
    has_any = out.notna().any(axis=1)
    pred = pd.Series(pd.NA, index=df.index, dtype="object")
    if has_any.any():
        pred.loc[has_any] = (
            out.loc[has_any].idxmax(axis=1).str.replace("race_", "", regex=False)
        )
    out["race_pred"] = pred
    out["race_source"] = pd.Series(source, index=df.index, dtype="object")

    out.index = df.index
    return pd.concat([df, out], axis=1)


__all__ = ["infer_race", "load_census", "load_firstnames",
           "CENSUS_CSV", "TZIOUMIS_CSV"]
