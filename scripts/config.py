"""Project configuration. Edit YEARS to control how much data you pull."""
import os
from pathlib import Path

# --- Paths ---
ROOT = Path(__file__).resolve().parent.parent
DATA_DIR = ROOT / "data"
RAW_DIR = DATA_DIR / "raw"
RAW_DIR.mkdir(parents=True, exist_ok=True)
DB_PATH = RAW_DIR / "nfl_research.duckdb"
ENV_PATH = ROOT / ".env"

# --- Year ranges ---
# Start small to test, then expand. nflverse goes back to 1999 for play-by-play,
# 1920+ for basic stats. CFBD has decent coverage from ~2000.
NFL_YEARS = list(range(2010, 2025))      # 2010-2024 seasons
COLLEGE_YEARS = list(range(2010, 2025))
RECRUIT_YEARS = list(range(2010, 2025))

# --- API keys ---
# Look in (a) the process environment first, then (b) a .env file at the
# project root. The .env file is gitignored so you can paste secrets in once
# instead of re-exporting them every shell. Format: KEY=value, one per line.
def _load_dotenv(path):
    if not path.exists():
        return
    for raw in path.read_text().splitlines():
        line = raw.strip()
        if not line or line.startswith("#") or "=" not in line:
            continue
        k, v = line.split("=", 1)
        k = k.strip()
        v = v.strip().strip('"').strip("'")
        os.environ.setdefault(k, v)


_load_dotenv(ENV_PATH)
CFBD_API_KEY = os.environ.get("CFBD_API_KEY", "")

# --- Behavior flags ---
INCLUDE_WEEKLY_NFL = True       # weekly player stats (big table)
INCLUDE_PBP = False             # play-by-play is huge (~1GB/season); off by default
INCLUDE_NEXTGEN = True          # NextGen Stats (2016+)
