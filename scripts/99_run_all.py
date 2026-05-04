"""Master runner — executes the full pipeline end-to-end.

Default run order (matches README.md):
  01_build_db          schema
  02_load_nfl          nflverse pulls (rosters, weekly rosters, stats, contracts, ...)
  02b_load_nfl_coaches PFR scrape for HC/OC/DC
  03_load_college      CFBD (teams, rosters, stats, coaches)
  04_load_recruiting   CFBD recruiting / 247 composite
  05_join_players      build player_id_map
  06_example_queries   sanity checks (read-only)
  07_export_csv        write all CSVs incl. the player-team-week panel

Each step is its own subprocess so a crash in one step doesn't poison the
others. Steps marked optional below (network-bound or read-only) won't stop
the pipeline if they fail; non-optional failures abort by default.

Examples:
  python scripts/99_run_all.py
  python scripts/99_run_all.py --skip 02b_load_nfl_coaches.py
  python scripts/99_run_all.py --only 07_export_csv.py
  python scripts/99_run_all.py --continue-on-error
"""
import argparse
import os
import subprocess
import sys
import time
from pathlib import Path

HERE = Path(__file__).resolve().parent

# (filename, human label, optional?, needs_cfbd_key?)
STEPS = [
    ("01_build_db.py",          "Build / refresh schema",            False, False),
    ("02_load_nfl.py",          "Load NFL data (nflverse)",          False, False),
    ("02b_load_nfl_coaches.py", "Scrape NFL coaches (PFR/Wayback)",  True,  False),
    ("03_load_college.py",      "Load college data (CFBD)",          True,  True),
    ("04_load_recruiting.py",   "Load recruiting (CFBD)",            True,  True),
    ("04c_infer_race.py",       "Infer race from surnames (Census)", False, False),
    ("05_join_players.py",      "Build player_id_map",               False, False),
    ("06_example_queries.py",   "Sanity-check queries",              True,  False),
    ("07_export_csv.py",        "Export CSVs (panel + topic files)", False, False),
    ("08_nfl_coach_data.py",    "Export team-week × coach × race",   False, False),
]


def run_step(name, label):
    path = HERE / name
    if not path.exists():
        print(f"  [skip] {name} — file not found")
        return False
    bar = "=" * 72
    print(f"\n{bar}\n=> {label}\n   {name}\n{bar}", flush=True)
    t0 = time.time()
    rc = subprocess.run([sys.executable, str(path)], cwd=HERE).returncode
    dt = time.time() - t0
    status = "ok" if rc == 0 else f"FAILED (exit {rc})"
    print(f"   --> {status} in {dt:.1f}s", flush=True)
    return rc == 0


def main():
    ap = argparse.ArgumentParser(description="Run the NFL-players pipeline end-to-end")
    ap.add_argument("--skip", nargs="*", default=[],
                    help="Step filenames to skip (e.g. 02b_load_nfl_coaches.py)")
    ap.add_argument("--only", nargs="*", default=None,
                    help="Run only these step filenames")
    ap.add_argument("--continue-on-error", action="store_true",
                    help="Keep going past failures of non-optional steps")
    args = ap.parse_args()

    # Preflight: warn if CFBD-dependent steps will be attempted without a key.
    cfbd_key = os.environ.get("CFBD_API_KEY", "")
    will_run_cfbd = any(
        needs and (args.only is None or name in args.only) and name not in args.skip
        for name, _, _, needs in STEPS
    )
    if will_run_cfbd and not cfbd_key:
        print("WARNING: CFBD_API_KEY is not set — college / recruiting steps "
              "will fail. Get a free key at https://collegefootballdata.com/key "
              "and `export CFBD_API_KEY=...` before re-running.\n", flush=True)

    pipeline_t0 = time.time()
    ran, skipped, failed = [], [], []
    for name, label, optional, _ in STEPS:
        if args.only and name not in args.only:
            skipped.append(name)
            continue
        if name in args.skip:
            print(f"\n[skip] {name}", flush=True)
            skipped.append(name)
            continue
        ok = run_step(name, label)
        ran.append(name)
        if not ok:
            failed.append(name)
            if not (optional or args.continue_on_error):
                print(f"\nAborting: {name} is required. "
                      f"Re-run with --continue-on-error to ignore.", flush=True)
                sys.exit(1)

    total = time.time() - pipeline_t0
    print("\n" + "=" * 72)
    print(f"Pipeline finished in {total/60:.1f} min  "
          f"(ran={len(ran)}, skipped={len(skipped)}, failed={len(failed)})")
    if failed:
        print(f"Failures: {failed}")
        # Exit non-zero only if a non-optional step failed.
        non_opt_failed = [n for n in failed
                          if not next(o for f, _, o, _ in STEPS if f == n)]
        sys.exit(1 if non_opt_failed else 0)


if __name__ == "__main__":
    main()
