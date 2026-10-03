"""Master runner: rebuild the DuckDB (data/datasets/nfl_research.duckdb).

Run order:
  00_fetch_reference     Census 2010 surnames + Tzioumis first names
  01_build_db            initialize DB, franchise-season crosswalk
  02_load_nfl            nflverse players, rosters, draft, combine, contracts, injuries
  02c_load_nfl_stats     nflverse schedules, team games/seasons (pbp EPA), player stats, snaps
  02b_load_nfl_staff     full coaching staff + front office (Wikipedia staff templates)
  03_load_college        CFBD teams, rosters, player/team stats, coaches, transfers
  04_load_recruiting     CFBD recruits (247 composite) + CFBD draft picks
  04c_infer_race         name-based BIFSG (secondary race measure)
  05_join_players        NFL <-> college <-> recruit crosswalk
  09_race_coding_sheets  human race-coding sheets (appends, never overwrites codes)
  04d_race_documented    documented race evidence: Wikidata P172 + Wikipedia
                         candidate sentences (needs player_wiki_signals from 09)
  04e_predict_race       predicted race: model-only posterior (primary; BIFSG
                         likelihood x EM-estimated NFL prior on predetermined
                         covariates) + documented sensitivity variant (preddoc)
                         (needs 04c, 05, 09, 04d, data/derived/race_text_labels.csv)
  06_validate_db        cross-table checks; fails on critical problems

Every network response is cached under data/raw/, so after the first run a
rebuild is offline (and spends no CFBD quota). --refresh is passed through to
the loaders to force re-downloads. Every step is required: a failure stops the
run unless --continue-on-error is given. On macOS the run holds a caffeinate
assertion so idle or maintenance sleep cannot suspend it.

The analysis samples are built afterwards in R: Rscript programs/95-make-all.R

Examples:
  .venv/bin/python scripts/99_run_all.py
  .venv/bin/python scripts/99_run_all.py --only 02b_load_nfl_staff.py
  .venv/bin/python scripts/99_run_all.py --skip 03_load_college.py 04_load_recruiting.py
"""
import argparse
import datetime as dt
import os
import shutil
import subprocess
import sys
import time
from pathlib import Path

HERE = Path(__file__).resolve().parent

# (filename, label, accepts --refresh)
STEPS = [
    ("00_fetch_reference.py",    "Fetch race reference files",          True),
    ("01_build_db.py",           "Initialize DB + franchise crosswalk", False),
    ("02_load_nfl.py",           "nflverse players/rosters/contracts",  True),
    ("02c_load_nfl_stats.py",    "nflverse games, team + player stats", True),
    ("02b_load_nfl_staff.py",    "Coaching staff + front office",       True),
    ("03_load_college.py",       "CFBD college data",                   True),
    ("04_load_recruiting.py",    "CFBD recruits + draft picks",         True),
    ("04c_infer_race.py",        "Name-based race inference (BIFSG)",   False),
    ("05_join_players.py",       "Player ID crosswalk",                 False),
    ("09_race_coding_sheets.py", "Race-coding sheets",                  False),
    ("04d_race_documented.py",   "Documented race evidence (Wiki)",     True),
    ("04e_predict_race.py",      "Predicted race (EM prior; + documented)", False),
    ("06_validate_db.py",        "Validate DB",                         False),
]


def run_step(name, label, extra):
    path = HERE / name
    if not path.exists():
        print(f"  [missing] {name}")
        return False
    bar = "=" * 72
    started = dt.datetime.now().strftime("%Y-%m-%d %H:%M:%S")
    print(f"\n{bar}\n=> {label}\n   {name} {' '.join(extra)}  (started {started})\n{bar}",
          flush=True)
    t0 = time.time()
    rc = subprocess.run([sys.executable, str(path), *extra], cwd=HERE).returncode
    ended = dt.datetime.now().strftime("%H:%M:%S")
    print(f"   --> {'ok' if rc == 0 else f'FAILED (exit {rc})'} in {time.time() - t0:.1f}s "
          f"(ended {ended})", flush=True)
    return rc == 0


def main():
    ap = argparse.ArgumentParser(description="Rebuild the NFL research DuckDB")
    ap.add_argument("--skip", nargs="*", default=[], help="step filenames to skip")
    ap.add_argument("--only", nargs="*", default=None, help="run only these steps")
    ap.add_argument("--refresh", action="store_true",
                    help="force re-download in the loaders that support it")
    ap.add_argument("--continue-on-error", action="store_true")
    args = ap.parse_args()

    # Keep macOS awake for the whole run (released when this process exits)
    if sys.platform == "darwin" and shutil.which("caffeinate"):
        subprocess.Popen(["caffeinate", "-i", "-w", str(os.getpid())])

    t0 = time.time()
    failed = []
    for name, label, refreshable in STEPS:
        if (args.only and name not in args.only) or name in args.skip:
            continue
        extra = ["--refresh"] if (args.refresh and refreshable) else []
        if not run_step(name, label, extra):
            failed.append(name)
            if not args.continue_on_error:
                print(f"\nAborting: {name} failed.", flush=True)
                sys.exit(1)

    print("\n" + "=" * 72)
    print(f"Pipeline finished in {(time.time() - t0) / 60:.1f} min; failures: {failed or 'none'}")
    sys.exit(1 if failed else 0)


if __name__ == "__main__":
    main()
