"""Download the reference files used by the name-based race inference.

Sources (all official, all public):
  * U.S. Census Bureau, "Frequently Occurring Surnames from the 2010 Census"
    https://www2.census.gov/topics/genealogy/2010surnames/names.zip
    -> 162,253 surnames with >= 100 bearers plus an 'ALL OTHER NAMES' row.
  * Tzioumis (2018), "Demographic aspects of first names", Scientific Data
    5:180025; data on Harvard Dataverse, doi:10.7910/DVN/TYJKEZ.
    -> 4,250 first names from mortgage applications (HMDA) plus an
       'ALL OTHER FIRST NAMES' row. The file URL is resolved through the
       Dataverse API and the download is checked against Dataverse's MD5.
  * U.S. Census Bureau, Population Division, CC-EST2019-ALLDATA (county
    population by age, sex, race and Hispanic origin), row YEAR = 1 (April 1,
    2010 Census population, modified race) and AGEGRP = 0 (all ages).
    https://www2.census.gov/programs-surveys/popest/datasets/2010-2019/counties/asrh/cc-est2019-alldata.csv
    -> county x race counts for the optional geography (BIFSG) variant.

Files written to data/raw/reference/:
  census_2010_names.zip          raw download
  census_2010_surnames.csv       Names_2010Census.csv extracted from the zip
  tzioumis_dataverse.json        Dataverse dataset metadata (API response)
  tzioumis_firstnames.xlsx       raw download (original file format)
  tzioumis_firstnames.csv        the xlsx 'Data' sheet as CSV
  cc-est2019-alldata.csv         raw download (176 MB)
  census_2010_county_race.csv    county_fips, total and the six race counts
  SOURCES.json                   source URL, retrieval time, SHA-256 and byte
                                 size of every file above

If the surname or first-name download fails, the script copies the CSV from
the archived May 2026 build (data/archive/2026-05-04-build/), prints a loud
warning and records "fallback_from_archive": true in SOURCES.json. The county
file has no archived copy, so its failure stops the script. A second run
reads the cache and makes no network calls; --refresh forces a re-download.
"""
import argparse
import csv
import hashlib
import json
import shutil
import sys
import xml.etree.ElementTree as ET
import zipfile

import pandas as pd

from common import http_session, now_utc
from config import DATA_DIR, REFERENCE_DIR

CENSUS_URL = "https://www2.census.gov/topics/genealogy/2010surnames/names.zip"
COUNTY_URL = ("https://www2.census.gov/programs-surveys/popest/datasets/"
              "2010-2019/counties/asrh/cc-est2019-alldata.csv")
TZIOUMIS_DOI = "doi:10.7910/DVN/TYJKEZ"
DATAVERSE = "https://dataverse.harvard.edu"
ARCHIVE_DIR = DATA_DIR / "archive" / "2026-05-04-build"
SOURCES_PATH = REFERENCE_DIR / "SOURCES.json"

CENSUS_CSV = "census_2010_surnames.csv"
TZIOUMIS_CSV = "tzioumis_firstnames.csv"
COUNTY_CSV = "census_2010_county_race.csv"

# CC-EST2019 columns (male + female) that make up each race category;
# modified race has no 'Some Other Race', so the six categories sum to TOT_POP.
COUNTY_RACES = {"white": ["NHWA"], "black": ["NHBA"], "hispanic": ["H"],
                "api": ["NHAA", "NHNA"], "aian": ["NHIA"], "multi": ["NHTOM"]}

XLSX_NS = {"m": "http://schemas.openxmlformats.org/spreadsheetml/2006/main",
           "r": "http://schemas.openxmlformats.org/officeDocument/2006/relationships"}


def sha256(path):
    h = hashlib.sha256()
    with open(path, "rb") as fh:
        for chunk in iter(lambda: fh.read(1 << 20), b""):
            h.update(chunk)
    return h.hexdigest()


def file_record(name, **meta):
    path = REFERENCE_DIR / name
    return {"file": name, "bytes": path.stat().st_size, "sha256": sha256(path),
            "recorded_at": now_utc(), **meta}


def download(session, url, dest):
    # Stream to a .part file and rename, so an interrupted download never
    # leaves a truncated file under the final name.
    part = dest.with_name(dest.name + ".part")
    with session.get(url, timeout=300, stream=True) as r:
        r.raise_for_status()
        with open(part, "wb") as fh:
            for chunk in r.iter_content(1 << 20):
                fh.write(chunk)
    part.replace(dest)
    return dest


# ----------------------------------------------------------------------------
# Census 2010 surnames
# ----------------------------------------------------------------------------

def fetch_census(session):
    zip_path = download(session, CENSUS_URL, REFERENCE_DIR / "census_2010_names.zip")
    with zipfile.ZipFile(zip_path) as zf:
        (REFERENCE_DIR / CENSUS_CSV).write_bytes(zf.read("Names_2010Census.csv"))
    return [file_record("census_2010_names.zip", source_url=CENSUS_URL),
            file_record(CENSUS_CSV, source_url=CENSUS_URL,
                        derived_from="census_2010_names.zip:Names_2010Census.csv")]


# ----------------------------------------------------------------------------
# Tzioumis (2018) first names
# ----------------------------------------------------------------------------

def xlsx_sheet_rows(path, sheet_name):
    """Rows of one worksheet as lists of strings (stdlib only; the venv has
    no openpyxl). Handles shared strings and numeric cells, which is all the
    Tzioumis workbook uses."""
    with zipfile.ZipFile(path) as zf:
        shared = [si.findtext(".//m:t", default="", namespaces=XLSX_NS)
                  for si in ET.fromstring(zf.read("xl/sharedStrings.xml"))]
        book = ET.fromstring(zf.read("xl/workbook.xml"))
        rels = ET.fromstring(zf.read("xl/_rels/workbook.xml.rels"))
        rid = next(s.get(f"{{{XLSX_NS['r']}}}id")
                   for s in book.find("m:sheets", XLSX_NS)
                   if s.get("name") == sheet_name)
        target = next(r.get("Target") for r in rels if r.get("Id") == rid)
        sheet = ET.fromstring(zf.read(f"xl/{target}"))
    rows = []
    for row in sheet.find("m:sheetData", XLSX_NS):
        cells = []
        for c in row:
            v = c.findtext("m:v", default="", namespaces=XLSX_NS)
            if v == "":        # formatted but empty cell
                continue
            if c.get("t") == "s":
                cells.append(shared[int(v)])
            else:
                # Shortest float repr ('91.606999999999999' -> '91.607').
                num = repr(float(v))
                cells.append(num[:-2] if num.endswith(".0") else num)
        rows.append(cells)
    return rows


def fetch_tzioumis(session):
    # Resolve the data file through the Dataverse API (keeps the file id and
    # checksum tied to the DOI rather than hard-coded).
    meta_url = f"{DATAVERSE}/api/datasets/:persistentId/?persistentId={TZIOUMIS_DOI}"
    r = session.get(meta_url, timeout=120)
    r.raise_for_status()
    (REFERENCE_DIR / "tzioumis_dataverse.json").write_text(r.text)
    files = r.json()["data"]["latestVersion"]["files"]
    datafile = next(f["dataFile"] for f in files
                    if f["dataFile"].get("originalFileName") == "firstnames.xlsx")
    file_url = f"{DATAVERSE}/api/access/datafile/{datafile['id']}?format=original"

    # Download the original xlsx and check it against Dataverse's MD5.
    xlsx = download(session, file_url, REFERENCE_DIR / "tzioumis_firstnames.xlsx")
    md5 = hashlib.md5(xlsx.read_bytes()).hexdigest()
    if md5 != datafile["md5"]:
        raise RuntimeError(f"Tzioumis xlsx MD5 {md5} != Dataverse {datafile['md5']}")

    # Convert the 'Data' sheet to CSV (header + 4,250 names + ALL OTHER row).
    rows = xlsx_sheet_rows(xlsx, "Data")
    if (rows[0][:2] != ["firstname", "obs"] or len(rows) != 4252
            or any(len(r) != 8 for r in rows)):
        raise RuntimeError(f"Unexpected Tzioumis layout: header {rows[0]}, {len(rows)} rows")
    with open(REFERENCE_DIR / TZIOUMIS_CSV, "w", newline="") as fh:
        csv.writer(fh, lineterminator="\n").writerows(rows)
    return [file_record("tzioumis_dataverse.json", source_url=meta_url),
            file_record("tzioumis_firstnames.xlsx", source_url=file_url,
                        doi=TZIOUMIS_DOI, dataverse_md5=datafile["md5"]),
            file_record(TZIOUMIS_CSV, source_url=file_url, doi=TZIOUMIS_DOI,
                        derived_from="tzioumis_firstnames.xlsx:Data")]


# ----------------------------------------------------------------------------
# Census 2010 county population by race (geography variant)
# ----------------------------------------------------------------------------

def fetch_county(session):
    raw = download(session, COUNTY_URL, REFERENCE_DIR / "cc-est2019-alldata.csv")
    df = pd.read_csv(raw, encoding="latin-1", dtype={"STATE": str, "COUNTY": str})
    df = df[(df["YEAR"] == 1) & (df["AGEGRP"] == 0)]
    out = pd.DataFrame({"county_fips": df["STATE"] + df["COUNTY"],
                        "county_name": df["CTYNAME"] + ", " + df["STNAME"],
                        "total": df["TOT_POP"]})
    for race, groups in COUNTY_RACES.items():
        out[race] = sum(df[f"{g}_{sex}"] for g in groups for sex in ("MALE", "FEMALE"))
    if (out[list(COUNTY_RACES)].sum(axis=1) != out["total"]).any():
        raise RuntimeError("county race counts do not add up to TOT_POP")
    if out["county_fips"].duplicated().any():
        raise RuntimeError("duplicate county FIPS in CC-EST2019 YEAR=1 rows")
    out.to_csv(REFERENCE_DIR / COUNTY_CSV, index=False)
    return [file_record("cc-est2019-alldata.csv", source_url=COUNTY_URL),
            file_record(COUNTY_CSV, source_url=COUNTY_URL,
                        derived_from="cc-est2019-alldata.csv: YEAR=1 (2010 Census), "
                                     "AGEGRP=0; api = NHAA+NHNA")]


# ----------------------------------------------------------------------------
# Driver
# ----------------------------------------------------------------------------

def fallback_to_archive(csv_name, error):
    src = ARCHIVE_DIR / csv_name
    bar = "!" * 78
    print(f"\n{bar}\nWARNING: download of {csv_name} FAILED: {error!r}\n"
          f"Copying the archived copy from {src}.\n"
          f"The race inference will run on the archived file; re-run with "
          f"--refresh once the source is reachable.\n{bar}\n", file=sys.stderr)
    if not src.exists():
        raise FileNotFoundError(f"No archived copy at {src}") from error
    shutil.copyfile(src, REFERENCE_DIR / csv_name)
    return [file_record(csv_name, source_url=str(src), fallback_from_archive=True,
                        download_error=repr(error))]


def main():
    ap = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    ap.add_argument("--refresh", action="store_true", help="re-download every file")
    args = ap.parse_args()

    sources = json.loads(SOURCES_PATH.read_text()) if SOURCES_PATH.exists() else {}
    session = http_session()
    targets = [(CENSUS_CSV, fetch_census, True), (TZIOUMIS_CSV, fetch_tzioumis, True),
               (COUNTY_CSV, fetch_county, False)]
    for csv_name, fetch, has_archive in targets:
        cached = sources.get(csv_name)
        if cached and (REFERENCE_DIR / csv_name).exists() and not args.refresh:
            # Cache hit: verify the file has not changed since it was recorded.
            if sha256(REFERENCE_DIR / csv_name) != cached["sha256"]:
                raise RuntimeError(f"{csv_name} does not match its SHA-256 in "
                                   f"{SOURCES_PATH}; re-run with --refresh")
            print(f"  {csv_name}: cached ({cached['source_url']})")
            continue
        print(f"  {csv_name}: downloading")
        try:
            records = fetch(session)
        except Exception as e:  # noqa: BLE001  (loud archive fallback, else re-raise)
            if not has_archive:
                raise
            records = fallback_to_archive(csv_name, e)
        for rec in records:
            sources[rec["file"]] = rec
        SOURCES_PATH.write_text(json.dumps(sources, indent=2) + "\n")

    # Coverage summary.
    print(f"\nReference files in {REFERENCE_DIR}:")
    for name, *_ in targets:
        rec = sources[name]
        with open(REFERENCE_DIR / name) as fh:
            n_lines = sum(1 for _ in fh) - 1
        flag = "  ** ARCHIVE FALLBACK **" if rec.get("fallback_from_archive") else ""
        print(f"  {name}: {n_lines:,} rows, sha256 {rec['sha256'][:16]}..., "
              f"from {rec['source_url']}{flag}")


if __name__ == "__main__":
    main()
