#!/usr/bin/env python3
"""Source-backed coach-policy reference inputs: program participation, named
offensive-assistant mandate participants and Wikidata gender statements.

Purpose: give programs/16-coach-policy-sample.R DOCUMENTED (not inferred)
inputs for the Rooney-policy analysis. Nothing here infers gender, race or
eligibility from a name or a photograph. Four acquisitions:

1. NFL Coach Accelerator participant lists published by the league
   (nfl.com press releases, May 2022 / May 2023 / May 2024). The league
   describes participants as "diverse" (women and minority) head-coach
   prospects nominated by clubs, so a listed participant is documented as
   NFL-eligible for the diversity provisions by the league's own
   designation. The 2026 cohort is excluded: the league reopened the
   program to candidates of all backgrounds, so listing no longer documents
   eligibility.
2. Named participants of the 2022-2024 minority/female offensive-assistant
   mandate, from ESPN's May 2026 report on the mandate's end, and the
   league's own naming of the minority head coaches hired in the 2021 cycle
   (nfl.com, 2021-10-26; it names a coach of Middle Eastern descent, whom Census-style
   ancestry mapping would file as white, so NFL-definition eligibility is
   documented where ancestry mapping alone is not). Each evidence row
   carries the quoted sentence, and the script FAILS if the quote is not
   found in the fetched article (so a silently edited source cannot pass).
   The league never published a 32-club mandate participant list, and the
   article says compliance was league-wide but roles varied, so the mandate
   rows document two spells, not the mandate population.
3. Wikidata "sex or gender" (P21) statements for every staff person with a
   Wikidata item (staff_persons.wikidata_qid). Only persons with an item
   get a value; everyone else stays unknown.
4. Head-coach appointment dates stated in the coaches' cached Wikipedia
   biographies (02b/04d text caches), by a strict single-sentence rule
   (one date, one NFL club, hiring verb, "head coach" as the role taken,
   the article's person as subject, no firing/extension/interview wording;
   see the HC DATES block). Announcement/introduction dates are kept apart
   from hired/named/signed dates; 16 validates each date against the
   game-data hiring window before using it. Dates that fail the rule stay
   unknown; nothing is imputed.

Data governance: person-level files go to data/derived/coach_policy/
(gitignored by notes/race-coding-protocol.md convention); only aggregate
counts and source URLs go to the tracked data/reference/ registry.

Caching: HTML pages are cached verbatim under data/raw/coach_policy/ with a
manifest (URL, sha256, fetched_at); SPARQL responses go through common.Wiki
(data/raw/wikipedia/wikidata/). Reruns are offline; --refresh re-downloads.

Matching: a listed name is linked to staff_persons by (a) an exact
normalized match against person_name/display_names restricted to persons on
the listed club's staff in the event season or the season before, (b) a
unique last-name match within that club-season (press releases shorten
first names: Nate / Nathan), (c) a unique exact normalized match among all
staff persons. Unmatched names are kept with person_id empty.

Output (data/derived/coach_policy/):
  program_participation.csv   one row per listed person x program event
  staff_gender_wikidata.csv   one row per staff person with a Wikidata item
  hc_hire_dates.csv           one row per candidate appointment sentence
Output (data/reference/):
  coach_policy_programs.csv   one row per program event: counts and sources
DuckDB is not touched.

Example:
  .venv/bin/python scripts/02e_coach_policy_reference.py
"""
import argparse
import csv
import hashlib
import json
import re
import sys
import time
import unicodedata
from collections import defaultdict
from pathlib import Path

import duckdb
from bs4 import BeautifulSoup

from common import Wiki, http_session, now_utc, to_franchise
from config import DATA_DIR, DB_PATH, RAW_DIR

HTML_CACHE = RAW_DIR / "coach_policy"
OUT_DIR = DATA_DIR / "derived" / "coach_policy"
REGISTRY_PATH = DATA_DIR / "reference" / "coach_policy_programs.csv"
SPARQL_BATCH = 200
GENDER_PROPERTY = "P21"

# Program events. `season` is the NFL season whose opening staff the listed
# club affiliation describes (a May accelerator precedes that season).
ACCELERATOR_EVENTS = [
    dict(program="nfl_coach_accelerator", event="2022-05 Atlanta", event_date="2022-05-23",
         season=2022, parser="nfl_club_list",
         url="https://www.nfl.com/news/inaugural-nfl-coach-and-front-office-accelerator-program-slated-for-spring-leagu",
         eligibility_basis="NFL: 'senior women and minority prospects' nominated by clubs"),
    dict(program="nfl_coach_accelerator", event="2023-05 Minneapolis", event_date="2023-05-21",
         season=2023, parser="nfl_table",
         url="https://www.nfl.com/news/nfl-coach-accelerator-program-to-take-place-during-spring-league-meeting-in-minn",
         eligibility_basis="NFL: 'diverse coaching talent'; 40 head-coach prospects"),
    dict(program="nfl_coach_accelerator", event="2024-05 Nashville", event_date="2024-05-20",
         season=2024, parser="nfl_table",
         url="https://www.nfl.com/news/nfl-continues-efforts-to-boost-inclusion-across-league-with-coach-accelerator-during-spring-league-meeting-in-nashville",
         eligibility_basis="NFL: 'qualified coaching candidates from diverse backgrounds'"),
]

# Quote-parsed sources: the article is fetched and the evidence sentences are
# located by program-specific phrases at run time, so no person name or race
# label is written in this file. Each parser returns rows with the quoted
# sentence and fails loudly when the expected sentences are absent.
TEAM_WORD = r"[A-Z][A-Za-z.]+(?: [A-Z][A-Za-z.]+){0,2}"
PERSON = r"[A-Z][\w.'\"-]+(?: [\w.'\"-]+){0,3}?"


def parse_mandate_espn(text):
    """ESPN 2026-05-13 report: (1) the coach whose NFL start came 'through
    this hiring mandate' with a club and year; (2) his promotion sentence
    ('promoted <Surname> before the YYYY season'); (3) the coach the same
    club 'hired <Name> ... as an offensive assistant' after his departure."""
    rows = []
    m1 = re.search(r"assistant coach (" + PERSON + r") got his start in the NFL through this "
                   r"hiring mandate, as an assistant with the (" + TEAM_WORD + r") in (\d{4})\.", text)
    if not m1:
        raise RuntimeError("ESPN mandate article: 'got his start ... through this hiring mandate' sentence not found")
    name, team, year = m1.group(1), m1.group(2), int(m1.group(3))
    rows.append(dict(name_listed=name, team_listed=team, season=year,
                     title_listed="assistant (mandate hire)", quote=m1.group(0)))
    surname = name.split()[-1]
    m2 = re.search(r"[^.]*promoted " + re.escape(surname) + r" before the (\d{4}) season to be an "
                   r"offensive assistant[^.]*\.", text)
    if not m2:
        raise RuntimeError("ESPN mandate article: promotion sentence not found")
    rows.append(dict(name_listed=name, team_listed=team, season=int(m2.group(1)),
                     title_listed="offensive assistant (promoted)", quote=m2.group(0).strip()))
    m3 = re.search(r"After " + re.escape(surname) + r"'s departure in (\d{4}), the (" + TEAM_WORD +
                   r") hired (" + PERSON + r"), who [^.]*, as an offensive assistant\.", text)
    if not m3:
        raise RuntimeError("ESPN mandate article: successor hire sentence not found")
    rows.append(dict(name_listed=m3.group(3), team_listed=m3.group(2), season=int(m3.group(1)),
                     title_listed="offensive assistant (mandate hire)", quote=m3.group(0)))
    return rows


def parse_nfl_minority_hc(text):
    """NFL.com 2021-10-26: the sentence counting 'minority head coaches hired
    in the last cycle' names each coach with his club. The league's own
    designation is wider than Census 'non-white' (one named coach is of
    Middle Eastern descent), so this documents NFL-definition eligibility."""
    m = re.search(r"There were (\w+) minority head coaches hired in the last cycle -- ([^.]*)\.", text)
    if not m:
        raise RuntimeError("NFL.com 2021 article: minority head coaches sentence not found")
    sentence = m.group(0)
    rows = []
    for item in re.finditer(r"the (" + TEAM_WORD + r")' (" + PERSON + r"), who is", m.group(2)):
        rows.append(dict(name_listed=item.group(2), team_listed=item.group(1), season=2021,
                         title_listed="head coach (NFL-named minority hire, 2021 cycle)",
                         quote=sentence))
    if len(rows) != {"two": 2, "three": 3, "one": 1}.get(m.group(1), -1):
        raise RuntimeError(f"NFL.com 2021 article: expected {m.group(1)} coaches, parsed {len(rows)}")
    return rows


QUOTE_SOURCES = [
    dict(program="offensive_assistant_mandate", event="ESPN 2026-05-13 report",
         event_date="2026-05-13", season_label="2022-2024", parser=parse_mandate_espn,
         url="https://www.espn.com/nfl/story/_/id/48762073/nfl-ended-minority-offensive-assistant-mandate-25-season",
         eligibility_basis="ESPN 2026-05-13: job created under the 2022 minority/female offensive-assistant mandate"),
    dict(program="nfl_minority_designation", event="NFL.com 2021-10-26 Rooney Rule enhancements",
         event_date="2021-10-26", season_label="2021", parser=parse_nfl_minority_hc,
         url="https://www.nfl.com/news/nfl-plans-to-enact-new-enhancements-to-rooney-rule-ahead-of-next-hiring-cycle",
         eligibility_basis="NFL.com 2021-10-26: named by the league as a minority head coach hired in the 2021 cycle"),
]


# ============================================================================
# Fetching and caching
# ============================================================================

def fetch_html(session, url, refresh=False):
    """Page HTML, cached verbatim under data/raw/coach_policy/<sha1(url)>.html
    with a JSONL manifest line (url, sha256 of the body, fetched_at)."""
    HTML_CACHE.mkdir(parents=True, exist_ok=True)
    path = HTML_CACHE / f"{hashlib.sha1(url.encode()).hexdigest()[:16]}.html"
    if path.exists() and not refresh:
        return path.read_text(encoding="utf-8")
    r = session.get(url, timeout=120)
    r.raise_for_status()
    body = r.text
    path.write_text(body, encoding="utf-8")
    with open(HTML_CACHE / "_manifest.jsonl", "a") as fh:
        fh.write(json.dumps({"url": url, "file": path.name,
                             "sha256": hashlib.sha256(body.encode()).hexdigest(),
                             "status": r.status_code, "fetched_at": now_utc()}) + "\n")
    return body


def clean(text):
    """Collapse whitespace, drop zero-width characters, normalize quotes."""
    text = unicodedata.normalize("NFKC", text).replace("\u200b", "")
    text = text.replace("\u2019", "'").replace("\u201c", '"').replace("\u201d", '"')
    return re.sub(r"\s+", " ", text).strip()


# ============================================================================
# Parsers
# ============================================================================

def parse_nfl_club_list(html, season):
    """2022 release: <p><strong>Club</strong></p><ul><li>Name, Title</li>...
    League-office rows (no club) are kept with team_listed empty."""
    soup = BeautifulSoup(html, "html.parser")
    head = soup.find(lambda t: t.name == "h3" and "Accelerator participants" in t.get_text())
    if head is None:
        raise RuntimeError("2022 accelerator page: participants heading not found")
    rows, club = [], None
    for el in head.find_all_next(["p", "ul"]):
        if el.name == "p":
            label = clean(el.get_text())
            if label.endswith(":"):
                club = None if label.startswith("NFL") else club
                continue
            if label.startswith("*"):
                break
            club = label
        else:
            for li in el.find_all("li"):
                text = clean(li.get_text())
                name, _, title = text.partition(",")
                rows.append(dict(name_listed=name.strip(), team_listed=club or "",
                                 title_listed=title.strip(), season=season,
                                 returning=0))
    if len(rows) < 50:
        raise RuntimeError(f"2022 accelerator page: only {len(rows)} rows parsed")
    return rows


def parse_nfl_table(html, season):
    """2023/2024 releases: a two-column table (Name/Coach, Team/Affiliate);
    a trailing * marks a returning participant; '--' or blank = no club."""
    soup = BeautifulSoup(html, "html.parser")
    rows = []
    for table in soup.find_all("table"):
        header = [clean(th.get_text()) for th in table.find_all("th")]
        if len(header) != 2:
            continue
        for tr in table.find_all("tr"):
            cells = [clean(td.get_text()) for td in tr.find_all("td")]
            if len(cells) != 2 or not cells[0]:
                continue
            name, team = cells
            returning = int(name.endswith("*"))
            name = name.rstrip("*").strip()
            team = "" if team in ("--", "-", "") else team
            rows.append(dict(name_listed=name, team_listed=team, title_listed="",
                             season=season, returning=returning))
        if rows:
            break
    if len(rows) < 20:
        raise RuntimeError(f"accelerator table page {season}: only {len(rows)} rows parsed")
    return rows


def article_text(html):
    """Whitespace-normalized visible text of a news page."""
    return clean(BeautifulSoup(html, "html.parser").get_text(" "))


# ============================================================================
# Head-coach appointment dates from cached Wikipedia biographies
# ============================================================================
# Strict extraction: a sentence yields a date only when ALL hold:
#   - exactly one full date (Month D, YYYY);
#   - exactly one NFL club mentioned (or none in the sentence and the
#     section heading is a club name);
#   - a hiring verb, and "head coach" used as the role being taken
#     (preceded by as / to be / to become / their / its / new / ordinal),
#     not another person's role ("under head coach X") and not an
#     assistant/associate/co- head-coach role; "interim head coach" is kept
#     and flagged;
#   - the subject is the article's person: after dropping a leading
#     "On <date>," or one subordinate clause, the sentence starts with the
#     person's surname, "He", or the club (which then names him or "him");
#   - no wording that moves the date to another event (fired/firing,
#     resign, extension, suspended, interview, offered, reported, declined,
#     candidate, finalist, re-signed, remain, contract through).
# The verb distinguishes an announcement/introduction date from a
# hired/named/signed/agreed date; neither is asserted to be the contract's
# effective date. Validation against the game data happens in 16.
MONTHS = ("January|February|March|April|May|June|July|August|September|October|"
          "November|December")
DATE_RE = re.compile(r"\b(" + MONTHS + r") (\d{1,2}), (\d{4})\b")
HIRE_VERB_RE = re.compile(r"\b(hired|named|signed|appointed|introduced|announced|agreed|"
                          r"accepted|became|hiring)\b", re.I)
ROLE_RE = re.compile(r"\b(as|to be|to become|became|their|its|his|new|the \d+(?:st|nd|rd|th)|"
                     r"\d+(?:st|nd|rd|th)|an NFL|the team's|the franchise's|the club's|position of)\s+"
                     r"(?:the |new |permanent |full-time |interim |\d+(?:st|nd|rd|th) |next |NFL )*"
                     r"head coach\b", re.I)
BAD_ROLE_RE = re.compile(r"\b(assistant|associate|co-|offensive|defensive|special teams) head coach",
                         re.I)
# Case-sensitive: "head coach" followed by a capitalized name is someone
# else's role ("under head coach Gus Bradley")
OTHER_HC_RE = re.compile(r"head coach [A-Z][a-z]+ [A-Z]|"
                         r"\b(under|with|alongside|replacing|succeeding|by|for) head coach [A-Z]")
# The first job title after the hiring verb must be the head-coach role
FIRST_JOB_RE = re.compile(r"\b(head coach|coordinator|coach|manager|director|executive|president|"
                          r"scout|analyst|assistant|consultant|advisor|adviser)\b", re.I)
EXCLUDE_RE = re.compile(r"\b(fired|firings?|dismiss\w*|resign\w*|extension|extended|suspend\w*|"
                        r"interview\w*|offered|reported|declined|turned down|candidate|finalist|"
                        r"re-signed|remain\w*|through the \d{4}|contract through|college|university|"
                        r"USFL|XFL|UFL|AAF|CFL|Arena)\b", re.I)
ANNOUNCE_VERBS = {"introduced", "announced"}


def team_regex():
    """Alternation of every multi-letter club name / nickname known to
    common.FRANCHISES, longest first."""
    import common
    names = sorted({a for a in common._ALIAS if len(a) > 3 and re.fullmatch(r"[A-Z. ]+", a)},
                   key=len, reverse=True)
    return re.compile(r"\b(" + "|".join(re.escape(n.title()) for n in names) + r")\b")


def strip_leading_clauses(sentence):
    """Drop a leading 'On <date>,' and at most one further subordinate
    clause ending in a comma; the remainder must start with the subject."""
    s = sentence.strip()
    s = re.sub(r"^(On|In) (" + MONTHS + r") \d{1,2}, \d{4},\s*", "", s)
    s = re.sub(r"^(In|During|Before|After|Following|With|Despite|Although|When|Once|Having)"
               r"[^,]{0,120},\s*", "", s)
    s = re.sub(r"^(On|In) (" + MONTHS + r") \d{1,2}, \d{4},\s*", "", s)
    return s


def hc_date_rows(person_id, title, revid, wikitext, surname, team_re, art):
    """Candidate appointment-date rows for one head coach's article."""
    out = []
    for section, paras in art.article_sections(wikitext):
        section_team = team_re.fullmatch(section.strip()) if section else None
        for para in paras:
            for sent in art.split_sentences(para):
                if "head coach" not in sent.lower():
                    continue
                dates = DATE_RE.findall(sent)
                if len(dates) != 1:
                    continue
                if EXCLUDE_RE.search(sent) or BAD_ROLE_RE.search(sent) or OTHER_HC_RE.search(sent):
                    continue
                if not ROLE_RE.search(sent):
                    continue
                first_verb = HIRE_VERB_RE.search(sent)
                first_job = FIRST_JOB_RE.search(sent, first_verb.end()) if first_verb else None
                if first_job is None or first_job.group(1).lower() != "head coach":
                    continue
                verbs = {v.lower() for v in HIRE_VERB_RE.findall(sent)}
                if not verbs:
                    continue
                mentions = list(team_re.finditer(sent))
                # A nickname preceded by another capitalized word that does
                # not form a known club name is a college/other team
                # ("Wyoming Cowboys", "Iowa State Cyclones")
                qualified = False
                for mt in mentions:
                    before = sent[:mt.start()].rstrip()
                    prev = before.split()[-1] if before else ""
                    if " " not in mt.group(1) and prev[:1].isupper() and \
                            to_franchise(f"{prev} {mt.group(1)}") is None and prev not in ("The",):
                        qualified = True
                if qualified:
                    continue
                teams = {to_franchise(t) for t in team_re.findall(sent)}
                teams.discard(None)
                if len(teams) == 0 and section_team:
                    teams = {to_franchise(section_team.group(1))}
                    teams.discard(None)
                if len(teams) != 1:
                    continue
                core = strip_leading_clauses(sent)
                starts_person = re.match(r"^(He|" + re.escape(surname) + r")\b", core) is not None
                starts_team = team_re.match(re.sub(r"^[Tt]he ", "", core)) is not None
                if starts_team and not re.search(r"\b(him|" + re.escape(surname) + r")\b", core):
                    continue
                if not (starts_person or starts_team):
                    continue
                month, day, year = dates[0]
                mnum = MONTHS.split("|").index(month) + 1
                out.append(dict(
                    person_id=person_id, wiki_title=title, revid=revid,
                    source_url=f"https://en.wikipedia.org/w/index.php?title={title.replace(' ', '_')}&oldid={revid}",
                    franchise_id=next(iter(teams)),
                    date=f"{int(year):04d}-{mnum:02d}-{int(day):02d}",
                    date_kind="announced" if verbs & ANNOUNCE_VERBS and not (verbs - ANNOUNCE_VERBS)
                    else "hired_named",
                    verbs="|".join(sorted(verbs)),
                    interim=int("interim head coach" in sent.lower()),
                    subject="person" if starts_person else "club",
                    section=section, sentence=sent))
    return out


def extract_hc_dates(db_path):
    """Appointment-date candidates for every staff person who was ever a
    head coach and has a cached Wikipedia article (02b/04d caches)."""
    import importlib
    from config import WIKI_CACHE
    art = importlib.import_module("04d_race_documented")
    con = duckdb.connect(str(db_path), read_only=True)
    try:
        hcs = con.execute("""
            SELECT person_id, person_name, wiki_title FROM staff_persons
            WHERE ever_head_coach AND wiki_title IS NOT NULL""").fetchall()
    finally:
        con.close()
    index = art.TextIndex([WIKI_CACHE / art.STAFF_TEXT_NS, WIKI_CACHE / art.TEXT_NS])
    index.scan()
    by_title = {t: (pid, name) for pid, name, t in hcs}
    team_re = team_regex()
    rows, n_articles = [], 0
    for title, revid, _ts, wikitext in art.iter_texts(index, list(by_title)):
        pid, name = by_title[title]
        parts = re.sub(r",?\s+(Jr\.?|Sr\.?|II|III|IV)$", "", name).split()
        surname = parts[-1] if parts else name
        n_articles += 1
        rows += hc_date_rows(pid, title, revid, wikitext, surname, team_re, art)
    print(f"HC appointment dates: {n_articles:,} of {len(hcs):,} head-coach articles read; "
          f"{len(rows):,} candidate sentences for {len({r['person_id'] for r in rows}):,} coaches")
    return rows


# ============================================================================
# Name matching against staff_persons
# ============================================================================

def norm_name(name):
    """Lower-case ASCII letters and spaces; suffixes and nicknames dropped."""
    name = unicodedata.normalize("NFKD", name).encode("ascii", "ignore").decode()
    name = re.sub(r'"[^"]*"', " ", name)                 # quoted nickname
    name = re.sub(r",?\s+(Jr\.?|Sr\.?|II|III|IV)$", "", name, flags=re.I)
    name = re.sub(r"[^A-Za-z ]", "", name.replace(".", ""))
    return re.sub(r"\s+", " ", name).strip().lower()


def name_variants(name_listed):
    """Normalized forms to try: full name without nickname, nickname + last."""
    out = [norm_name(name_listed)]
    m = re.search(r'"([^"]+)"', name_listed)
    if m:
        last = name_listed.split()[-1]
        out.append(norm_name(f"{m.group(1)} {last}"))
    return [v for v in dict.fromkeys(out) if v]


def load_staff(db_path):
    con = duckdb.connect(str(db_path), read_only=True)
    try:
        persons = con.execute("""
            SELECT person_id, person_name, display_names, wikidata_qid
            FROM staff_persons""").fetchall()
        memberships = con.execute("""
            SELECT DISTINCT person_id, franchise_id, season
            FROM staff_team_season""").fetchall()
    finally:
        con.close()
    names = defaultdict(set)         # normalized name -> person_ids
    last_names = {}
    qids = {}
    for pid, pname, display, qid in persons:
        for n in [pname, *(display or [])]:
            if n:
                names[norm_name(n)].add(pid)
        last_names[pid] = norm_name(pname).split()[-1] if norm_name(pname) else ""
        if qid:
            qids[pid] = qid
    on_staff = defaultdict(set)      # (franchise_id, season) -> person_ids
    for pid, fid, season in memberships:
        on_staff[(fid, int(season))].add(pid)
    return names, last_names, on_staff, qids


def match_person(row, names, last_names, on_staff):
    """(person_id, method) for a listed row; ('', 'unmatched') when no rule
    gives a unique person."""
    fid = to_franchise(row["team_listed"]) if row["team_listed"] else None
    variants = name_variants(row["name_listed"])
    # Event season first, then the season before (a May release may list
    # the club of the season just ended); never both at once, so a person
    # whose staff identity was split across seasons links to the event season
    pools = [on_staff.get((fid, s), set()) for s in (row["season"], row["season"] - 1)] \
        if fid else []
    for pool in pools:
        for v in variants:
            hits = names.get(v, set()) & pool
            if len(hits) == 1:
                return next(iter(hits)), "club_season_exact"
    for pool in pools:
        last = variants[0].split()[-1]
        hits = {p for p in pool if last_names.get(p) == last}
        if len(hits) == 1:
            return next(iter(hits)), "club_season_last_name"
    for v in variants:
        hits = names.get(v, set())
        if len(hits) == 1:
            return next(iter(hits)), "global_unique_exact"
    return "", "unmatched"


# ============================================================================
# Wikidata sex or gender (P21)
# ============================================================================

def gender_for_items(wiki, qids, refresh):
    """person item -> (gender_qid, gender_label) for non-deprecated P21."""
    out = {}
    items = sorted(set(qids))
    for i in range(0, len(items), SPARQL_BATCH):
        values = " ".join(f"wd:{q}" for q in items[i:i + SPARQL_BATCH])
        query = (f"SELECT ?item ?g ?gLabel WHERE {{ VALUES ?item {{ {values} }} "
                 f"?item p:{GENDER_PROPERTY} ?st . ?st ps:{GENDER_PROPERTY} ?g ; "
                 f"wikibase:rank ?rank . FILTER(?rank != wikibase:DeprecatedRank) "
                 f'SERVICE wikibase:label {{ bd:serviceParam wikibase:language "en" . }} }}')
        res = wiki.sparql(query, refresh=refresh)
        for b in res["results"]["bindings"]:
            item = b["item"]["value"].rsplit("/", 1)[-1]
            g = b["g"]["value"].rsplit("/", 1)[-1]
            out.setdefault(item, set()).add((g, b.get("gLabel", {}).get("value", "")))
    return out


# ============================================================================
# Main
# ============================================================================

def write_csv(path, rows, fields):
    path.parent.mkdir(parents=True, exist_ok=True)
    with open(path, "w", newline="", encoding="utf-8") as fh:
        w = csv.DictWriter(fh, fieldnames=fields)
        w.writeheader()
        for r in rows:
            w.writerow({k: r.get(k, "") for k in fields})
    print(f"  -> {path.relative_to(DATA_DIR.parent)}: {len(rows):,} rows")


def main():
    parser = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    parser.add_argument("--refresh", action="store_true",
                        help="re-download every page and SPARQL response")
    args = parser.parse_args()
    t0 = time.time()

    names, last_names, on_staff, qids = load_staff(DB_PATH)
    print(f"staff_persons: {len(last_names):,} persons, {len(qids):,} with a Wikidata item")

    session = http_session()
    participation, registry = [], []
    for ev in ACCELERATOR_EVENTS:
        html = fetch_html(session, ev["url"], args.refresh)
        rows = (parse_nfl_club_list if ev["parser"] == "nfl_club_list" else parse_nfl_table)(
            html, ev["season"])
        n_matched = 0
        for r in rows:
            pid, method = match_person(r, names, last_names, on_staff)
            n_matched += bool(pid)
            participation.append(dict(
                program=ev["program"], event=ev["event"], event_date=ev["event_date"],
                season=r["season"], name_listed=r["name_listed"],
                team_listed=r["team_listed"],
                franchise_id=(to_franchise(r["team_listed"]) or "") if r["team_listed"] else "",
                title_listed=r["title_listed"], returning=r["returning"],
                person_id=pid, match_method=method, source_url=ev["url"],
                eligibility_basis=ev["eligibility_basis"], quote=""))
        registry.append(dict(program=ev["program"], event=ev["event"],
                             event_date=ev["event_date"], season=ev["season"],
                             n_listed=len(rows), n_matched=n_matched,
                             source_url=ev["url"], eligibility_basis=ev["eligibility_basis"]))
        print(f"{ev['event']}: {len(rows)} listed, {n_matched} matched to staff_persons")

    for src in QUOTE_SOURCES:
        html = fetch_html(session, src["url"], args.refresh)
        rows = src["parser"](article_text(html))
        n_matched = 0
        for r in rows:
            pid, method = match_person(r, names, last_names, on_staff)
            n_matched += bool(pid)
            participation.append(dict(
                program=src["program"], event=src["event"], event_date=src["event_date"],
                season=r["season"], name_listed=r["name_listed"],
                team_listed=r["team_listed"], franchise_id=to_franchise(r["team_listed"]),
                title_listed=r["title_listed"], returning=0, person_id=pid,
                match_method=method, source_url=src["url"],
                eligibility_basis=src["eligibility_basis"], quote=r["quote"]))
        registry.append(dict(program=src["program"], event=src["event"],
                             event_date=src["event_date"], season=src["season_label"],
                             n_listed=len(rows), n_matched=n_matched, source_url=src["url"],
                             eligibility_basis=src["eligibility_basis"]))
        print(f"{src['event']}: {len(rows)} evidence rows, {n_matched} matched")

    unmatched = [r for r in participation if not r["person_id"]]
    if unmatched:
        print(f"Unmatched listed names ({len(unmatched)}; kept with empty person_id):")
        for r in unmatched:
            print(f"  {r['event']}: {r['name_listed']} [{r['team_listed'] or 'no club'}]")

    wiki = Wiki()
    gender = gender_for_items(wiki, qids.values(), args.refresh)
    gender_rows = []
    for pid, qid in sorted(qids.items()):
        values = sorted(gender.get(qid, set()))
        gender_rows.append(dict(
            person_id=pid, wikidata_qid=qid,
            gender_qid=";".join(g for g, _ in values),
            gender_label=";".join(l for _, l in values),
            n_statements=len(values),
            source_url=f"https://www.wikidata.org/wiki/{qid}#{GENDER_PROPERTY}"))
    counts = defaultdict(int)
    for r in gender_rows:
        counts[r["gender_label"] or "(no P21)"] += 1
    print("Wikidata P21 by label: " + ", ".join(f"{k}={v}" for k, v in sorted(counts.items())))

    write_csv(OUT_DIR / "program_participation.csv", participation,
              ["program", "event", "event_date", "season", "name_listed", "team_listed",
               "franchise_id", "title_listed", "returning", "person_id", "match_method",
               "source_url", "eligibility_basis", "quote"])
    write_csv(OUT_DIR / "staff_gender_wikidata.csv", gender_rows,
              ["person_id", "wikidata_qid", "gender_qid", "gender_label", "n_statements",
               "source_url"])
    hc_rows = extract_hc_dates(DB_PATH)
    write_csv(OUT_DIR / "hc_hire_dates.csv", hc_rows,
              ["person_id", "wiki_title", "revid", "source_url", "franchise_id", "date",
               "date_kind", "verbs", "interim", "subject", "section", "sentence"])
    write_csv(REGISTRY_PATH, registry,
              ["program", "event", "event_date", "season", "n_listed", "n_matched",
               "source_url", "eligibility_basis"])
    print(f"Done in {time.time() - t0:.0f} s")


if __name__ == "__main__":
    sys.exit(main())
