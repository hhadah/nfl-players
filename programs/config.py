"""Project configuration. Edit YEARS to control how much data you pull."""
import os
from pathlib import Path

# --- Paths ---
ROOT = Path(__file__).resolve().parent.parent
DATA_DIR = ROOT / "data"
DATA_DIR.mkdir(exist_ok=True)
DB_PATH = DATA_DIR / "nfl_research.duckdb"

# --- Year ranges ---
# Start small to test, then expand. nflverse goes back to 1999 for play-by-play,
# 1920+ for basic stats. CFBD has decent coverage from ~2000.
NFL_YEARS = list(range(2010, 2025))      # 2010-2024 seasons
COLLEGE_YEARS = list(range(2010, 2025))
RECRUIT_YEARS = list(range(2010, 2025))

# --- API keys ---
CFBD_API_KEY = os.environ.get("CFBD_API_KEY", "")

# --- Behavior flags ---
INCLUDE_WEEKLY_NFL = True       # weekly player stats (big table)
INCLUDE_PBP = False             # play-by-play is huge (~1GB/season); off by default
INCLUDE_NEXTGEN = True          # NextGen Stats (2016+)
