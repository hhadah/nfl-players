"""Build player_id_map by joining nfl_players, college_players, recruits.

Strategy (conservative — favors precision over recall):
  1. Exact match on (normalized_name, college). High confidence.
  2. Exact match on (normalized_name, college) between college_players and recruits
     using committed_to == college team.
  3. Fuzzy fallback for names that differ only by punctuation/jr/sr.

Names that don't match cleanly stay unmapped — better than wrong joins.
"""
import re
import unicodedata
import uuid
import duckdb
import pandas as pd
from config import DB_PATH


def normalize_name(name):
    if not name:
        return ""
    n = unicodedata.normalize("NFKD", str(name)).encode("ascii", "ignore").decode()
    n = n.lower()
    n = re.sub(r"\b(jr|sr|ii|iii|iv|v)\b\.?", "", n)
    n = re.sub(r"[^a-z0-9 ]", "", n)
    n = re.sub(r"\s+", " ", n).strip()
    return n


def normalize_school(school):
    if not school:
        return ""
    s = school.lower().strip()
    # Common variants
    replacements = {
        "university of ": "", " university": "", "saint ": "st ",
        "st.": "st", "&": "and",
    }
    for k, v in replacements.items():
        s = s.replace(k, v)
    return re.sub(r"\s+", " ", s).strip()


def main():
    con = duckdb.connect(str(DB_PATH))

    # Load source tables
    nfl_df = con.execute("""
        SELECT gsis_id, pfr_id, full_name, college, birth_date
        FROM nfl_players
    """).df()
    cfb_df = con.execute("""
        SELECT cfbd_id, full_name, team
        FROM college_players
    """).df()
    rec_df = con.execute("""
        SELECT recruit_id, name AS full_name, committed_to AS team, high_school
        FROM recruits
    """).df()

    # Normalize
    for df in (nfl_df, cfb_df, rec_df):
        df["name_norm"] = df["full_name"].apply(normalize_name)
    nfl_df["school_norm"] = nfl_df["college"].apply(normalize_school)
    cfb_df["school_norm"] = cfb_df["team"].apply(normalize_school)
    rec_df["school_norm"] = rec_df["team"].apply(normalize_school)

    # 1. NFL <-> College (name + school)
    print("Matching NFL to college...")
    nfl_to_cfb = nfl_df.merge(
        cfb_df[["cfbd_id", "name_norm", "school_norm"]],
        on=["name_norm", "school_norm"], how="left"
    )

    # 2. College <-> Recruits (name + committed school)
    print("Matching college to recruits...")
    cfb_to_rec = cfb_df.merge(
        rec_df[["recruit_id", "high_school", "name_norm", "school_norm"]],
        on=["name_norm", "school_norm"], how="left"
    )

    # Build the id_map by chaining: nfl player -> cfb player -> recruit
    print("Building canonical map...")
    cfb_lookup = cfb_to_rec.set_index("cfbd_id")[["recruit_id", "high_school"]].to_dict("index")

    rows = []
    matched_cfb_ids = set()
    matched_rec_ids = set()

    for _, r in nfl_to_cfb.iterrows():
        cfbd_id = r.get("cfbd_id")
        rec_info = cfb_lookup.get(cfbd_id, {}) if pd.notna(cfbd_id) else {}
        recruit_id = rec_info.get("recruit_id")
        high_school = rec_info.get("high_school")
        rows.append({
            "canonical_id": str(uuid.uuid4()),
            "gsis_id": r["gsis_id"],
            "pfr_id": r.get("pfr_id"),
            "cfbd_id": cfbd_id if pd.notna(cfbd_id) else None,
            "recruit_id": recruit_id,
            "full_name": r["full_name"],
            "birth_date": r["birth_date"],
            "college": r["college"],
            "high_school": high_school,
            "confidence": "exact" if pd.notna(cfbd_id) else "nfl_only",
        })
        if pd.notna(cfbd_id):
            matched_cfb_ids.add(cfbd_id)
        if recruit_id:
            matched_rec_ids.add(recruit_id)

    # Add unmatched college players (never made NFL) so the map is complete
    for _, r in cfb_to_rec.iterrows():
        if r["cfbd_id"] in matched_cfb_ids:
            continue
        rows.append({
            "canonical_id": str(uuid.uuid4()),
            "gsis_id": None, "pfr_id": None,
            "cfbd_id": r["cfbd_id"],
            "recruit_id": r.get("recruit_id"),
            "full_name": r["full_name"],
            "birth_date": None,
            "college": r["team"],
            "high_school": r.get("high_school"),
            "confidence": "exact" if pd.notna(r.get("recruit_id")) else "college_only",
        })
        if pd.notna(r.get("recruit_id")):
            matched_rec_ids.add(r["recruit_id"])

    # Add unmatched recruits (never played college, transferred, JUCO, etc.)
    for _, r in rec_df.iterrows():
        if r["recruit_id"] in matched_rec_ids:
            continue
        rows.append({
            "canonical_id": str(uuid.uuid4()),
            "gsis_id": None, "pfr_id": None, "cfbd_id": None,
            "recruit_id": r["recruit_id"],
            "full_name": r["full_name"],
            "birth_date": None,
            "college": r["team"],
            "high_school": r.get("high_school"),
            "confidence": "recruit_only",
        })

    df = pd.DataFrame(rows)
    con.execute("DELETE FROM player_id_map")
    con.register("staging", df)
    con.execute("INSERT INTO player_id_map SELECT * FROM staging")
    con.unregister("staging")

    # Report
    summary = con.execute("""
        SELECT confidence, COUNT(*) AS n FROM player_id_map GROUP BY confidence ORDER BY n DESC
    """).df()
    print("\nMap summary:")
    print(summary.to_string(index=False))

    # NFL match rate
    nfl_total = con.execute("SELECT COUNT(*) FROM nfl_players").fetchone()[0]
    nfl_matched = con.execute("""
        SELECT COUNT(*) FROM player_id_map WHERE gsis_id IS NOT NULL AND cfbd_id IS NOT NULL
    """).fetchone()[0]
    print(f"\nNFL players with college match: {nfl_matched}/{nfl_total} "
          f"({100*nfl_matched/max(nfl_total,1):.1f}%)")

    con.close()


if __name__ == "__main__":
    main()
