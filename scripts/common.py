"""Shared helpers for every loader.

- DuckDB: connect(), write_table() (full refresh, keeps every source column).
- Caching: cached_parquet() for nflverse, CFBD.get() and Wiki.get() for APIs.
  Every network response is cached under data/raw/ so rebuilding the DuckDB
  never needs the network (and never spends CFBD quota twice).
- Teams: to_franchise() and team_season_code() harmonize every team code or
  name used by nflverse, GSIS, PFR, OverTheCap and Wikipedia.
"""
import datetime as dt
import hashlib
import json
import re
import time
from pathlib import Path

import duckdb
import requests
from requests.adapters import HTTPAdapter
from urllib3.util.retry import Retry

from config import (CFBD_API_KEY, CFBD_BASE_URL, CFBD_CACHE, CFBD_MAX_CALLS_PER_RUN,
                    DB_PATH, NFLVERSE_CACHE, USER_AGENT, WIKI_CACHE,
                    WIKI_MIN_INTERVAL_S)


def now_utc():
    return dt.datetime.now(dt.timezone.utc).isoformat(timespec="seconds")


# ============================================================================
# DuckDB
# ============================================================================

def connect(read_only=False):
    DB_PATH.parent.mkdir(parents=True, exist_ok=True)
    return duckdb.connect(str(DB_PATH), read_only=read_only)


def _as_relation_source(df):
    """Return an object DuckDB can register (pandas, polars, or arrow)."""
    mod = type(df).__module__
    if mod.startswith("polars"):
        return df.to_arrow()
    return df


def write_table(con, name, df, source="", note=""):
    """Replace table `name` with the full contents of `df` (all columns kept).

    Full refresh (not upsert) so that a rebuild never mixes rows from older
    code versions. Records row/column counts in _build_log.
    """
    con.register("_staging", _as_relation_source(df))
    con.execute(f'CREATE OR REPLACE TABLE "{name}" AS SELECT * FROM _staging')
    con.unregister("_staging")
    n = con.execute(f'SELECT count(*) FROM "{name}"').fetchone()[0]
    k = len(con.execute(f'DESCRIBE "{name}"').fetchall())
    con.execute("""CREATE TABLE IF NOT EXISTS _build_log (
        table_name VARCHAR, n_rows BIGINT, n_cols INTEGER, source VARCHAR,
        note VARCHAR, built_at VARCHAR)""")
    con.execute("DELETE FROM _build_log WHERE table_name = ?", [name])
    con.execute("INSERT INTO _build_log VALUES (?, ?, ?, ?, ?, ?)",
                [name, n, k, source, note, now_utc()])
    print(f"  -> {name}: {n:,} rows x {k} cols")
    return n


# ============================================================================
# nflverse parquet cache
# ============================================================================

def cached_parquet(name, fetch, refresh=False):
    """Return a polars DataFrame from data/raw/nflverse/<name>.parquet,
    calling fetch() and saving the result on a cache miss."""
    import polars as pl
    path = NFLVERSE_CACHE / f"{name}.parquet"
    if path.exists() and not refresh:
        return pl.read_parquet(path)
    df = fetch()
    df.write_parquet(path)
    with open(NFLVERSE_CACHE / "_manifest.jsonl", "a") as fh:
        fh.write(json.dumps({"name": name, "rows": df.height, "cols": df.width,
                             "fetched_at": now_utc()}) + "\n")
    return df


# ============================================================================
# HTTP
# ============================================================================

def http_session():
    s = requests.Session()
    s.headers.update({"User-Agent": USER_AGENT})
    retry = Retry(total=6, backoff_factor=2.0,
                  status_forcelist=(429, 500, 502, 503, 504),
                  allowed_methods=("GET",), respect_retry_after_header=True)
    s.mount("https://", HTTPAdapter(max_retries=retry))
    s.mount("http://", HTTPAdapter(max_retries=retry))
    return s


def _cache_key(params):
    blob = json.dumps(params, sort_keys=True, default=str)
    return hashlib.sha1(blob.encode()).hexdigest()[:16]


def _slug(text):
    return re.sub(r"[^A-Za-z0-9]+", "_", text).strip("_")


class CFBD:
    """CollegeFootballData REST client with a disk cache and a call budget.

    The free tier allows 1,000 calls per month, so every response is cached
    in data/raw/cfbd/<endpoint>/<params>.json and live calls per process are
    capped at CFBD_MAX_CALLS_PER_RUN. Every live call is logged to
    data/raw/cfbd/_calls.log.
    """

    def __init__(self, max_calls=CFBD_MAX_CALLS_PER_RUN):
        if not CFBD_API_KEY:
            raise RuntimeError("CFBD_API_KEY is not set (put it in .env)")
        # No automatic retries: each retry would spend quota without being
        # counted. A failed call raises; re-running resumes from the cache.
        self.s = requests.Session()
        self.s.headers.update({"User-Agent": USER_AGENT,
                               "Authorization": f"Bearer {CFBD_API_KEY}"})
        self.max_calls = max_calls
        self.live_calls = 0

    def _log(self, endpoint, params, status):
        with open(CFBD_CACHE / "_calls.log", "a") as fh:
            fh.write(json.dumps({"at": now_utc(), "endpoint": endpoint,
                                 "params": params, "status": status}) + "\n")

    def get(self, endpoint, refresh=False, **params):
        params = {k: v for k, v in params.items() if v is not None}
        folder = CFBD_CACHE / _slug(endpoint)
        name = "_".join(f"{k}-{params[k]}" for k in sorted(params)) or "all"
        path = folder / f"{_slug(name)}.json"
        if path.exists() and not refresh:
            return json.loads(path.read_text())
        if self.live_calls >= self.max_calls:
            raise RuntimeError(f"CFBD call budget ({self.max_calls}) exhausted")
        r = self.s.get(f"{CFBD_BASE_URL}{endpoint}", params=params, timeout=120)
        self.live_calls += 1
        self._log(endpoint, params, r.status_code)
        r.raise_for_status()
        data = r.json()
        folder.mkdir(parents=True, exist_ok=True)
        path.write_text(json.dumps(data))
        return data

    def info(self):
        """Quota status (patron level, remaining calls). Not cached."""
        r = self.s.get(f"{CFBD_BASE_URL}/info", timeout=60)
        self._log("/info", {}, r.status_code)
        r.raise_for_status()
        return r.json()


class Wiki:
    """Wikipedia / Wikidata API client: cached, rate limited, maxlag-aware."""

    def __init__(self, min_interval=WIKI_MIN_INTERVAL_S):
        self.s = http_session()
        self.min_interval = min_interval
        self._last = 0.0

    def _wait(self):
        gap = time.monotonic() - self._last
        if gap < self.min_interval:
            time.sleep(self.min_interval - gap)
        self._last = time.monotonic()

    def get(self, params, namespace="api", refresh=False,
            url="https://en.wikipedia.org/w/api.php"):
        params = {"format": "json", "formatversion": 2, **params}
        if "wikipedia.org" in url:
            params.setdefault("maxlag", 5)
        folder = WIKI_CACHE / namespace
        folder.mkdir(parents=True, exist_ok=True)
        path = folder / f"{_cache_key({'url': url, **params})}.json"
        if path.exists() and not refresh:
            return json.loads(path.read_text())
        for attempt in range(8):
            self._wait()
            r = self.s.get(url, params=params, timeout=120)
            if r.status_code == 200:
                data = r.json()
                if data.get("error", {}).get("code") == "maxlag":
                    time.sleep(5 * (attempt + 1))
                    continue
                path.write_text(json.dumps(data))
                return data
            time.sleep(min(60, 5 * 2 ** attempt))
        r.raise_for_status()
        raise RuntimeError(f"Wikipedia request failed: {params}")

    def sparql(self, query, refresh=False):
        return self.get({"query": query, "format": "json"}, namespace="wikidata",
                        refresh=refresh, url="https://query.wikidata.org/sparql")


# ============================================================================
# Team codes
# ============================================================================
# franchise_id = current nflverse abbreviation. Every alias below (nflverse
# historical, GSIS, PFR, OverTheCap nickname, full names past and present,
# ESPN) maps to one franchise_id.

FRANCHISES = {
    "ARI": dict(name="Arizona Cardinals", pfr="crd",
                aliases=["ARZ", "CRD", "CARDINALS", "PHO", "PHX", "PHOENIX CARDINALS"]),
    "ATL": dict(name="Atlanta Falcons", pfr="atl", aliases=["FALCONS"]),
    "BAL": dict(name="Baltimore Ravens", pfr="rav", aliases=["BLT", "RAV", "RAVENS"]),
    "BUF": dict(name="Buffalo Bills", pfr="buf", aliases=["BILLS"]),
    "CAR": dict(name="Carolina Panthers", pfr="car", aliases=["PANTHERS"]),
    "CHI": dict(name="Chicago Bears", pfr="chi", aliases=["BEARS"]),
    "CIN": dict(name="Cincinnati Bengals", pfr="cin", aliases=["BENGALS"]),
    "CLE": dict(name="Cleveland Browns", pfr="cle", aliases=["CLV", "BROWNS"]),
    "DAL": dict(name="Dallas Cowboys", pfr="dal", aliases=["COWBOYS"]),
    "DEN": dict(name="Denver Broncos", pfr="den", aliases=["BRONCOS"]),
    "DET": dict(name="Detroit Lions", pfr="det", aliases=["LIONS"]),
    "GB": dict(name="Green Bay Packers", pfr="gnb", aliases=["GNB", "GBP", "PACKERS"]),
    "HOU": dict(name="Houston Texans", pfr="htx", aliases=["HST", "HTX", "TEXANS"]),
    "IND": dict(name="Indianapolis Colts", pfr="clt", aliases=["CLT", "COLTS"]),
    "JAX": dict(name="Jacksonville Jaguars", pfr="jax", aliases=["JAC", "JAGUARS"]),
    "KC": dict(name="Kansas City Chiefs", pfr="kan", aliases=["KAN", "KCC", "CHIEFS"]),
    "LV": dict(name="Las Vegas Raiders", pfr="rai",
               aliases=["LVR", "OAK", "RAI", "RAIDERS", "OAKLAND RAIDERS"]),
    "LAC": dict(name="Los Angeles Chargers", pfr="sdg",
                aliases=["SD", "SDG", "SDC", "CHARGERS", "SAN DIEGO CHARGERS"]),
    "LA": dict(name="Los Angeles Rams", pfr="ram",
               aliases=["LAR", "STL", "SL", "RAM", "RAMS", "ST. LOUIS RAMS",
                        "ST LOUIS RAMS"]),
    "MIA": dict(name="Miami Dolphins", pfr="mia", aliases=["DOLPHINS"]),
    "MIN": dict(name="Minnesota Vikings", pfr="min", aliases=["VIKINGS"]),
    "NE": dict(name="New England Patriots", pfr="nwe", aliases=["NWE", "NEP", "PATRIOTS"]),
    "NO": dict(name="New Orleans Saints", pfr="nor", aliases=["NOR", "NOS", "SAINTS"]),
    "NYG": dict(name="New York Giants", pfr="nyg", aliases=["GIANTS"]),
    "NYJ": dict(name="New York Jets", pfr="nyj", aliases=["JETS"]),
    "PHI": dict(name="Philadelphia Eagles", pfr="phi", aliases=["EAGLES"]),
    "PIT": dict(name="Pittsburgh Steelers", pfr="pit", aliases=["STEELERS"]),
    "SF": dict(name="San Francisco 49ers", pfr="sfo", aliases=["SFO", "49ERS", "NINERS"]),
    "SEA": dict(name="Seattle Seahawks", pfr="sea", aliases=["SEAHAWKS"]),
    "TB": dict(name="Tampa Bay Buccaneers", pfr="tam",
               aliases=["TAM", "TBB", "BUCCANEERS", "BUCS"]),
    "TEN": dict(name="Tennessee Titans", pfr="oti",
                aliases=["OTI", "TITANS", "OILERS", "HOUSTON OILERS", "TENNESSEE OILERS"]),
    "WAS": dict(name="Washington Commanders", pfr="was",
                aliases=["WSH", "COMMANDERS", "REDSKINS", "FOOTBALL TEAM",
                         "WASHINGTON", "WASHINGTON REDSKINS",
                         "WASHINGTON FOOTBALL TEAM"]),
}

_ALIAS = {}
for _fid, _meta in FRANCHISES.items():
    for _a in [_fid, _meta["name"], _meta["pfr"], *_meta["aliases"]]:
        _ALIAS[_a.upper()] = _fid


# Codes that named another franchise before a relocation: code -> (last
# season of the earlier meaning, that franchise). Only matters before 1997.
RELOCATED_CODES = {"BAL": (1983, "IND"),   # Baltimore Colts
                   "STL": (1987, "ARI"),   # St. Louis Cardinals
                   "LAR": (1994, "LV"),    # Los Angeles Raiders (GSIS draft_club)
                   "HOU": (1996, "TEN")}   # Houston Oilers


def to_franchise(code, season=None):
    """Map any team code or name to its franchise_id, or None if unknown.

    Pass `season` for codes that may predate 1997 (e.g. HOU in 1990 is the
    Oilers, now TEN). Multi-team strings (e.g. OverTheCap 'Bills/Jets')
    return None; split them first if needed.
    """
    if code is None:
        return None
    key = str(code).strip().upper()
    if season is not None and key in RELOCATED_CODES:
        last, old = RELOCATED_CODES[key]
        if season <= last:
            return old
    return _ALIAS.get(key)


def team_season_code(franchise_id, season):
    """Historical nflverse schedule code for a franchise in a given season."""
    if franchise_id == "LV":
        return "OAK" if season <= 2019 else "LV"
    if franchise_id == "LAC":
        return "SD" if season <= 2016 else "LAC"
    if franchise_id == "LA":
        return "STL" if season <= 2015 else "LA"
    return franchise_id


def team_season_name(franchise_id, season):
    """Team name as used in that season (e.g. 'Oakland Raiders' in 2015)."""
    if franchise_id == "LV" and season <= 2019:
        return "Oakland Raiders"
    if franchise_id == "LAC" and season <= 2016:
        return "San Diego Chargers"
    if franchise_id == "LA" and season <= 2015:
        return "St. Louis Rams"
    if franchise_id == "WAS":
        if season <= 2019:
            return "Washington Redskins"
        if season <= 2021:
            return "Washington Football Team"
    return FRANCHISES[franchise_id]["name"]


def franchise_table():
    """One row per franchise-season (1999-2025) with every naming variant."""
    import pandas as pd
    from config import NFL_SEASONS
    rows = []
    for fid, meta in FRANCHISES.items():
        for season in NFL_SEASONS:
            if fid == "HOU" and season < 2002:
                continue
            rows.append(dict(franchise_id=fid, season=season,
                             team_code=team_season_code(fid, season),
                             team_name=team_season_name(fid, season),
                             franchise_name=meta["name"], pfr_slug=meta["pfr"]))
    return pd.DataFrame(rows)
