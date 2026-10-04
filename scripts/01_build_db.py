"""Initialize the DuckDB and write the franchise-season crosswalk.

Each loader creates and fully refreshes its own tables (keeping every source
column), so there is no hand-maintained schema here. This step only writes
`franchise_seasons`: one row per franchise x season (1999-2025) with the
franchise_id used as the team key everywhere, plus the historical nflverse
code, the team name used that season and the PFR slug.
"""
from common import connect, franchise_table, write_table
from config import DB_PATH


def main():
    print(f"DuckDB: {DB_PATH}")
    con = connect()
    write_table(con, "franchise_seasons", franchise_table(),
                source="scripts/common.py FRANCHISES")
    con.close()


if __name__ == "__main__":
    main()
