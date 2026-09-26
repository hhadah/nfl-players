"""Project configuration: paths, sample windows, API keys, request etiquette.

Every loader imports from here. The DuckDB path can be overridden with the
NFL_DB_PATH environment variable (used for development builds).
"""
import os
from pathlib import Path

# --- Paths ---
ROOT = Path(__file__).resolve().parent.parent
DATA_DIR = ROOT / "data"
RAW_DIR = DATA_DIR / "raw"                 # untouched extracts (API caches)
DATASETS_DIR = DATA_DIR / "datasets"       # derived data (DuckDB, analysis samples)
HAND_CODED_DIR = DATA_DIR / "hand_coded"   # human-coded inputs (tracked in git)
ENV_PATH = ROOT / ".env"

NFLVERSE_CACHE = RAW_DIR / "nflverse"      # parquet snapshots of nflverse releases
CFBD_CACHE = RAW_DIR / "cfbd"              # JSON responses from the CFBD REST API
WIKI_CACHE = RAW_DIR / "wikipedia"         # Wikipedia/Wikidata API responses
REFERENCE_DIR = RAW_DIR / "reference"      # Census surnames, Tzioumis first names

for _d in (RAW_DIR, DATASETS_DIR, HAND_CODED_DIR, NFLVERSE_CACHE, CFBD_CACHE,
           WIKI_CACHE, REFERENCE_DIR):
    _d.mkdir(parents=True, exist_ok=True)

DB_PATH = Path(os.environ.get("NFL_DB_PATH", DATASETS_DIR / "nfl_research.duckdb"))

# --- Sample windows (inclusive ranges) ---
# Last completed NFL season as of the 2026-09 rebuild. The 2026 season is in
# progress, so it is excluded from season-level tables.
LAST_SEASON = 2025

NFL_SEASONS = list(range(1999, LAST_SEASON + 1))            # schedules, pbp, player stats
NFL_WEEKLY_ROSTER_SEASONS = list(range(2002, LAST_SEASON + 1))
NFL_SNAP_SEASONS = list(range(2013, LAST_SEASON + 1))       # nflverse 2012 file is empty
NFL_INJURY_SEASONS = list(range(2009, LAST_SEASON + 1))
NFL_NGS_SEASONS = list(range(2016, LAST_SEASON + 1))
NFL_PFR_ADV_SEASONS = list(range(2018, LAST_SEASON + 1))
NFL_PARTICIPATION_SEASONS = list(range(2016, LAST_SEASON + 1))
STAFF_SEASONS = list(range(2007, LAST_SEASON + 1))          # Wikipedia staff templates
COLLEGE_SEASONS = list(range(2004, LAST_SEASON + 1))
RECRUIT_YEARS = list(range(2000, LAST_SEASON + 1))
DRAFT_YEARS = list(range(2000, LAST_SEASON + 2))             # 2026 draft is complete


# --- API keys ---
def _load_dotenv(path):
    """Read KEY=value lines from .env into os.environ (without overriding)."""
    if not path.exists():
        return
    for raw in path.read_text().splitlines():
        line = raw.strip()
        if not line or line.startswith("#") or "=" not in line:
            continue
        k, v = line.split("=", 1)
        os.environ.setdefault(k.strip(), v.strip().strip('"').strip("'"))


_load_dotenv(ENV_PATH)
CFBD_API_KEY = os.environ.get("CFBD_API_KEY", "")
CFBD_BASE_URL = "https://api.collegefootballdata.com"
# The free CFBD tier allows 1,000 calls per month. Loaders cache every
# response and refuse to exceed this many live calls per process.
CFBD_MAX_CALLS_PER_RUN = int(os.environ.get("CFBD_MAX_CALLS_PER_RUN", "400"))

# --- Request etiquette ---
USER_AGENT = ("nfl-players-research/1.0 (academic research; "
              "https://github.com/hhadah/nfl-players)")
WIKI_MIN_INTERVAL_S = 1.0     # seconds between Wikipedia API requests
