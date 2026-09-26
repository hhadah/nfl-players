"""Infer race / ethnicity from surnames using the U.S. Census 2010 file.

Updates these tables in place with seven new columns each:
  nfl_players, college_players, recruits, nfl_coaches, college_coaches

Columns: race_white, race_black, race_api, race_aian, race_2prace,
         race_hispanic (percentages 0..100), race_pred (argmax label).

Inference is name-only and surname-only. Read the docstring of race_utils.py
for caveats before using `race_pred` as a discrete label in analysis.
"""
import duckdb
import pandas as pd

from config import DB_PATH
from race_utils import infer_race, _OUT_COLS  # noqa: F401  (column list reused)

OUT_COLS = ["race_white", "race_black", "race_api", "race_aian",
            "race_2prace", "race_hispanic", "race_pred", "race_source",
            "race_pred_surname"]

# Each entry: table, PK columns, plus name-source columns. We pass first AND
# last where available — race_utils combines them via BIFSG (Bayesian
# Improved First-name Surname Geocoding without geocoding). Falls back to
# surname-only if first name is missing or unmatched.
TARGETS = [
    {
        "table": "nfl_players",
        "key":   ["gsis_id"],
        "first": "first_name",
        "last":  "last_name",
    },
    {
        "table": "college_players",
        "key":   ["cfbd_id"],
        "first": "first_name",
        "last":  "last_name",
    },
    {
        "table": "recruits",
        "key":   ["recruit_id"],
        # The CFBD recruiting loader leaves first/last NULL — derive both
        # from the combined `name` field.
        "full":  "name",
    },
    {
        "table": "nfl_coaches",
        "key":   ["coach_id", "team", "season", "role"],
        "first": "first_name",
        "last":  "last_name",
    },
    {
        "table": "college_coaches",
        "key":   ["coach_id", "school", "season"],
        "first": "first_name",
        "last":  "last_name",
    },
    {
        "table": "nfl_weekly_coaches",
        "key":   ["team", "season", "week", "season_type"],
        "first": "first_name",
        "last":  "last_name",
    },
]


def _update_table(con, table, key_cols, first_col=None, last_col=None, full_col=None):
    needed = key_cols + [c for c in (first_col, last_col, full_col) if c]
    df = con.execute(f"SELECT {', '.join(needed)} FROM {table}").df()
    if df.empty:
        print(f"  {table}: empty — skipping")
        return
    enriched = infer_race(
        df, first_col=first_col, last_col=last_col, full_col=full_col
    )[key_cols + OUT_COLS]
    matched = enriched["race_pred"].notna().sum()
    total = len(enriched)
    via = enriched["race_source"].value_counts(dropna=False).to_dict()
    print(f"  {table}: {matched:,}/{total:,} matched "
          f"({100 * matched / max(total, 1):.1f}%)  source={via}")

    con.register("race_staging", enriched)
    set_clause = ", ".join([f"{c} = race_staging.{c}" for c in OUT_COLS])
    where = " AND ".join([f"{table}.{k} = race_staging.{k}" for k in key_cols])
    con.execute(f"UPDATE {table} SET {set_clause} FROM race_staging WHERE {where}")
    con.unregister("race_staging")


def main():
    con = duckdb.connect(str(DB_PATH))
    print("Inferring race from surnames (Census 2010)...")
    for t in TARGETS:
        try:
            _update_table(
                con, t["table"], t["key"],
                first_col=t.get("first"),
                last_col=t.get("last"),
                full_col=t.get("full"),
            )
        except Exception as e:
            print(f"  {t['table']} failed: {e}")

    # Cross-table summary so we can sanity-check the distribution.
    print("\nrace_pred distribution across player tables:")
    for table in ["nfl_players", "college_players", "recruits", "nfl_coaches"]:
        try:
            counts = con.execute(
                f"SELECT race_pred, COUNT(*) FROM {table} "
                f"GROUP BY race_pred ORDER BY 2 DESC"
            ).df()
            counts.columns = ["race_pred", table]
            print(f"\n  {table}:")
            print(counts.to_string(index=False))
        except Exception as e:
            print(f"  {table}: {e}")

    con.close()
    print("\nRace inference complete.")


if __name__ == "__main__":
    main()
