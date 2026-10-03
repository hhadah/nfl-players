"""Documented (source-stated) race/ethnicity evidence for staff and players.

Purpose: collect what public sources EXPLICITLY state about the race or
ethnicity of every NFL staff person and player, as input to the documented
race measure (notes/race-prediction-design.md, section 1). Nothing here
infers race from a name or a photograph. Two sources:

1. Wikidata "ethnic group" (P172) statements, for staff items (the
   staff_persons.wikidata_qid) and player items found through P3561 (Pro
   Football Reference player ID, values like "B/BradTo00"). Deprecated
   statements are dropped. Every ethnic-group item's English label is mapped
   to a protocol category with the explicit dictionary ETHNIC_GROUP_MAP below;
   labels missing from it stay unmapped and are reported. Each statement keeps
   its rank and references (reference URL P854, stated in P248, imported from
   P143), because a statement imported from a Wikipedia edition is weaker than
   one citing a source.
2. Wikipedia article text (current revision), for every staff article
   (staff_persons.wiki_title) and player article (player_wiki_signals). The
   wikitext is stripped of markup (mwparserfromhell), split into sections and
   sentences, and CANDIDATE sentences containing a race/ethnicity term
   (RACE_TERMS) are kept with the previous and next sentence as context and
   flags for whether the sentence names the subject or starts with a
   pronoun. A candidate is NOT a statement of the subject's race: articles
   mention other people's race and topics such as the Rooney Rule (Bill
   Walsh's article mentions the African-American coaches he mentored). The
   candidates are classified in a later step.

Universe: DuckDB tables staff_persons (person_id, wiki_title, wikidata_qid),
nfl_players (gsis_id, pfr_id) and player_wiki_signals (gsis_id, wiki_title),
read through a READ-ONLY connection. person_uid is 'staff:<person_id>' or
'player:<gsis_id>', as in the race-coding sheets. A player who later joined
a staff appears under both uids (data/hand_coded/race_coding/person_links.csv
links them).

Caching: every response goes through common.Wiki (1 request per second,
maxlag-aware), cached under data/raw/wikipedia/: SPARQL in wikidata/, article
text in race_documented/text/. Article text already cached by
02b_load_nfl_staff.py (staff/person_text/) is reused. The text fetch builds a
title index from every cached response first and requests only the titles
not yet cached, so an interrupted run resumes where it stopped and reruns are
offline. --refresh re-downloads everything.

Writes (data/raw/wikipedia/race_documented/, or --out-dir):
  wikidata_ethnicity.parquet  person x Wikidata item x ethnic group: label,
                              mapped category, rank, references
  text_candidates.parquet     person x candidate sentence: term(s), sentence,
                              context, section, revision id, subject flags
  article_coverage.parquet    one row per person: has_article, fetched,
                              n_sentences, n_candidates, Wikidata item, n_p172
  DuckDB tables race_wikidata_ethnicity, race_text_candidates and
  race_article_coverage (common.write_table), unless --no-db.

Examples:
  .venv/bin/python scripts/04d_race_documented.py --no-db
  .venv/bin/python scripts/04d_race_documented.py --limit 50 --no-db --out-dir /tmp/x
"""
import argparse
import importlib
import json
import re
import time
import unicodedata
from pathlib import Path

import duckdb
import mwparserfromhell as mwp
import pandas as pd

from common import Wiki, connect, write_table
from config import DB_PATH, WIKI_CACHE
from staff_wikitext import EVIDENCE_TERMS

staff_mod = importlib.import_module("02b_load_nfl_staff")   # query_all, chunks

OUT_DIR = WIKI_CACHE / "race_documented"
TEXT_NS = "race_documented/text"                 # cache namespace (article text)
STAFF_TEXT_NS = "staff/person_text"              # 02b's cached leader article text
SPARQL_NS = "wikidata"
PFR_PROPERTY = "P3561"            # Wikidata: Pro Football Reference player ID
ETHNIC_PROPERTY = "P172"          # Wikidata: ethnic group
SPARQL_BATCH = 200                # items or PFR ids per SPARQL query
TEXT_BATCH = 50                   # titles per MediaWiki request (API maximum)
MAX_SENTENCE_CHARS = 1000

# ============================================================================
# Race/ethnicity terms for candidate sentences
# ============================================================================
# (pattern, group, noisy). group = the category the term points to
# (black, white, hispanic, asian, pacific_islander, american_indian,
# multiracial, mena, context); context = race-related topics that say nothing
# by themselves about the subject (Rooney Rule, Fritz Pollard, minority
# hiring). noisy = the term often does not refer to race (a capitalised
# "Black" in "Black Monday", "Mexican" for a place or a league, "white" as a
# colour). Patterns are case-sensitive unless wrapped in (?i:...). Order
# matters: the first alternative that matches at a position wins, so the
# specific forms come before the generic ones.
_PERSON_NOUNS = (r"(?:head coach(?:es)?|coach(?:es|ing)?|coordinators?|general managers?|"
                 r"GMs?|owners?|executives?|assistants?|players?|quarterbacks?|"
                 r"athletes?|candidates?|men|man|women|woman|people|community|"
                 r"family|families|students?|teammates?|officials?|referees?)")
_BLACK_NATIONS = (r"Nigerian|Ghanaian|Liberian|Sierra Leonean|Cameroonian|Congolese|"
                  r"Ethiopian|Eritrean|Kenyan|Somali|Senegalese|Ivorian|Ugandan|"
                  r"Sudanese|Haitian|Jamaican|Bahamian|Barbadian|Trinidadian|"
                  r"Bermudian|Grenadian|Antiguan|Vincentian|Belizean|Guyanese")
_HISPANIC_NATIONS = (r"Mexican|Cuban|Dominican|Colombian|Salvadoran|Guatemalan|Honduran|"
                     r"Nicaraguan|Venezuelan|Peruvian|Ecuadorian|Panamanian|"
                     r"Costa Rican|Argentine|Argentinian|Chilean|Bolivian|Uruguayan|"
                     r"Paraguayan")
_ASIAN_NATIONS = (r"Japanese|Korean|Chinese|Filipino|Vietnamese|Taiwanese|Indian|Thai|"
                  r"Hmong|Cambodian|Laotian|Pakistani|Bangladeshi|Indonesian|Okinawan")
_EUROPEAN_NATIONS = (r"Irish|Italian|German|Polish|Greek|Scottish|Scots-Irish|English|"
                     r"Welsh|Dutch|Norwegian|Swedish|Danish|Finnish|Czech|Slovak|"
                     r"Hungarian|Croatian|Serbian|Slovenian|Ukrainian|Russian|"
                     r"Lithuanian|Portuguese|French|Belgian|Austrian|Swiss|Romanian|"
                     r"Bulgarian|Icelandic|Albanian|Maltese|European")
_MENA_NATIONS = (r"Lebanese|Syrian|Arab|Armenian|Iranian|Persian|Egyptian|Palestinian|"
                 r"Assyrian|Iraqi|Jordanian|Moroccan|Turkish|Israeli")
_ANCESTRY_NOUNS = (r"(?:[- ]Americans?|[- ]Canadians?| descent| ancestry| heritage| origins?|"
                   r" roots| immigrants?| parents?| father| mother| family| background)")
# A term followed by one of these names a place or an institution
# ("Cherokee High School", "Mexican authorities"), not a person's ancestry.
_NOT_PERSON = (r"(?! (?:High School|Middle School|Elementary|Academy|College|County|"
               r"authorities|police|government|League|Winter League|food|restaurants?|"
               r"cuisine|Republic|Grand Prix|national team|Football League|"
               r"Pacific League|Navy|Army|Falls|City|Canyon|Valley|Lake|River|Creek|"
               r"Springs|Park|Street|Avenue|Road|Trail)\b)")
_BLACK_EXCLUDE = (r"(?! (?:Monday|Friday|Tuesday|Thursday|Sunday|Knights?|Hawks?|Hills|"
                  r"Sea|Belt|Box|Ops|Panthers|Bears|Diamonds|Sox|Swamp|Hole|Death|"
                  r"Widow|Mamba|Bird|Mountains?|River|Forest|Warriors?|Rock|Lake|Creek|"
                  r"Jack|Jersey|jerseys?|uniforms?|helmets?|and (?:Gold|Blue|White|Red|Silver)|"
                  r"& Gold|Lightning|Magic|Label|Eyed|Star|Swan|Book|Mesa|Canyon|"
                  r"College|Colleges)\b)")
RACE_TERMS = [
    # Black
    (r"African[- ]Americans?", "black", False),
    (r"(?:" + _BLACK_NATIONS + r"|African|Afro-Caribbean|Caribbean)" + _ANCESTRY_NOUNS,
     "black", False),
    (r"Afro-[A-Z][a-z]+", "black", False),
    (r"(?i:black " + _PERSON_NOUNS + r")\b", "black", False),
    (r"(?<!Historically )(?<!historically )(?<!Silver and )Black" + _BLACK_EXCLUDE,
     "black", True),
    (r"(?:" + _BLACK_NATIONS + r")" + _NOT_PERSON, "black", True),
    # Multiracial
    (r"(?i:bi-?racial|multi-?racial|mixed[- ]race|mixed (?:heritage|ancestry|ethnicity))",
     "multiracial", False),
    # Pacific Islander
    (r"Samoan|Tongan|Native Hawaiian|Pacific Islanders?|Fijian|Chamorro|Guamanian|"
     r"M[aā]ori|Micronesian", "pacific_islander", False),
    (r"Polynesian(?! Bowl)", "pacific_islander", False),
    (r"Polynesian(?= Bowl)", "pacific_islander", True),
    # Hispanic / Latino
    (r"Hispanic|Latino|Latina|Latinx|Chicano|Puerto Rican", "hispanic", False),
    (r"(?:" + _HISPANIC_NATIONS + r"|Spanish)" + _ANCESTRY_NOUNS, "hispanic", False),
    (r"(?:" + _HISPANIC_NATIONS + r")" + _NOT_PERSON, "hispanic", True),
    # Asian
    (r"Asian[- ]Americans?|Asian (?:descent|ancestry|heritage)", "asian", False),
    (r"(?:" + _ASIAN_NATIONS + r")" + _ANCESTRY_NOUNS, "asian", False),
    (r"Filipino|Vietnamese", "asian", True),
    # American Indian / Alaska Native
    (r"Native Americans?|American Indians?|Alaska Natives?|First Nations|"
     r"(?i:enrolled (?:member|citizen) of the)", "american_indian", False),
    (r"(?:Cherokee|Navajo|Lumbee|Lakota|Choctaw|Chickasaw|Muscogee|Creek Nation|"
     r"Seminole Nation|Ojibwe|Comanche|Kiowa|Osage Nation|Blackfeet|Hopi|"
     r"Oneida Nation|Seneca Nation|Crow Nation|Potawatomi|Ho-Chunk|Tlingit|Inupiat|"
     r"Yupik)(?![a-z])" + _NOT_PERSON, "american_indian", False),
    # Tribal names that are also places or teams (Cheyenne, Sioux Falls). Tribal
    # names take no plural: "Chippewas" and "Choctaws" are college teams.
    (r"(?:Apache|Cheyenne|Shoshone|Pueblo|Sioux|Arapaho|Pawnee|Menominee|Mohawk|"
     r"Chippewa)(?![a-z])" + _NOT_PERSON, "american_indian", True),
    # White / European ancestry (noisy: "white" is also a colour)
    (r"(?i:white " + _PERSON_NOUNS + r")\b|White Americans?|Caucasians?|"
     r"European[- ]Americans?", "white", True),
    (r"(?:" + _EUROPEAN_NATIONS + r")" + _ANCESTRY_NOUNS, "white", False),
    (r"Jewish|Jews?\b", "white", True),
    # Middle East / North Africa (no protocol category; kept for review)
    (r"(?:" + _MENA_NATIONS + r")" + _ANCESTRY_NOUNS, "mena", False),
    # Race-related topics (not about the subject by themselves)
    (r"Fritz Pollard|Rooney Rule|(?i:people of color|person of color|coaches of color)",
     "context", False),
    (r"(?i:minority (?:head coach(?:es)?|coach(?:es|ing)?(?: fellowships?)?|candidates?|hiring|"
     r"general managers?|GMs?|executives?|assistants?|owners?|fellowships?)|minorities)",
     "context", False),
    (r"(?i:racial(?:ly)?|racism|racist|segregat(?:ed|ion)|ethnicity|ethnic)", "context", True),
    # Anything else the staff evidence extractor (staff_wikitext.EVIDENCE_TERMS)
    # matches, so the candidates are a superset of its evidence sentences.
    # 'first Black' is dropped: it would consume "first Black head coach"
    # before the specific pattern above could match at "Black".
    (r"(?:" + "|".join(t for t in EVIDENCE_TERMS if not t.startswith("first ")) + r")",
     "legacy", True),
]


def _compile_terms(terms):
    """One alternation with a named group per term (g0, g1, ...), so a single
    finditer per sentence finds every term and m.lastgroup says which one.
    Each term must end a word (an optional plural s/es is allowed), so
    'Black' does not match 'Blackburn' and 'Hopi' does not match 'Hopkins'."""
    parts = [rf"(?P<g{k}>\b(?:{pat})(?:s|es)?\b)" for k, (pat, _, _) in enumerate(terms)]
    return re.compile("|".join(parts))


TERM_RE = _compile_terms(RACE_TERMS)
TERM_GROUP = {f"g{k}": grp for k, (_, grp, _) in enumerate(RACE_TERMS)}
TERM_NOISY = {f"g{k}": noisy for k, (_, _, noisy) in enumerate(RACE_TERMS)}

# ============================================================================
# Wikidata ethnic-group labels -> protocol categories
# ============================================================================
# Explicit dictionary (English label of the P172 value -> category). A value
# with two parts ("black;hispanic") marks a label that states both a race and
# Hispanic ethnicity. Categories follow notes/race-coding-protocol.md: black,
# white, asian, pacific_islander, american_indian, multiracial, and hispanic
# (ethnicity, recorded separately from race). Labels missing here stay
# unmapped and are listed at the end of the run. Left unmapped on purpose:
# nationality-only labels ("Americans"), and Jewish and Middle Eastern /
# North African groups, for which the protocol has no category (the decision
# belongs to the PI). The 2026-10 pull returned 15 distinct labels.
ETHNIC_GROUP_MAP = {
    # Black
    "African Americans": "black",
    "Black people": "black",
    "Black Americans": "black",
    "African diaspora": "black",
    "Black Canadians": "black",
    "Black British people": "black",
    "Afro-Caribbean": "black",
    "Afro-Caribbean Americans": "black",
    "Caribbean Americans": "black",
    "Nigerian Americans": "black",
    "Ghanaian Americans": "black",
    "Liberian Americans": "black",
    "Sierra Leonean Americans": "black",
    "Cameroonian Americans": "black",
    "Congolese Americans": "black",
    "Ethiopian Americans": "black",
    "Eritrean Americans": "black",
    "Kenyan Americans": "black",
    "Somali Americans": "black",
    "Haitian Americans": "black",
    "Jamaican Americans": "black",
    "Bahamian Americans": "black",
    "Trinidadian and Tobagonian Americans": "black",
    "Krahn people": "black",                     # West African (Liberia)
    "Afro-Latin Americans": "black;hispanic",
    "Afro-Puerto Ricans": "black;hispanic",
    "Afro-Cubans": "black;hispanic",
    "Afro-Dominicans": "black;hispanic",
    "Afro-Mexicans": "black;hispanic",
    # White
    "White Americans": "white",
    "white people": "white",
    "White people": "white",
    "European Americans": "white",
    "Irish Americans": "white",
    "Italian Americans": "white",
    "German Americans": "white",
    "Polish Americans": "white",
    "Greek Americans": "white",
    "Scottish Americans": "white",
    "Scotch-Irish Americans": "white",
    "English Americans": "white",
    "Dutch Americans": "white",
    "Norwegian Americans": "white",
    "Swedish Americans": "white",
    "Danish Americans": "white",
    "Finnish Americans": "white",
    "Czech Americans": "white",
    "Slovak Americans": "white",
    "Hungarian Americans": "white",
    "Croatian Americans": "white",
    "Serbian Americans": "white",
    "Slovene Americans": "white",
    "Ukrainian Americans": "white",
    "Russian Americans": "white",
    "Lithuanian Americans": "white",
    "French Americans": "white",
    "Portuguese Americans": "white",
    "Albanian Americans": "white",
    "Hungarians in Romania": "white",
    # Hispanic / Latino (ethnicity)
    "Hispanic and Latino Americans": "hispanic",
    "Hispanic Americans": "hispanic",
    "Latino": "hispanic",
    "Mexican Americans": "hispanic",
    "Puerto Ricans": "hispanic",
    "Stateside Puerto Ricans": "hispanic",
    "Cuban Americans": "hispanic",
    "Dominican Americans": "hispanic",
    "Colombian Americans": "hispanic",
    "Salvadoran Americans": "hispanic",
    "Guatemalan Americans": "hispanic",
    "Honduran Americans": "hispanic",
    "Nicaraguan Americans": "hispanic",
    "Panamanian Americans": "hispanic",
    "Venezuelan Americans": "hispanic",
    "Peruvian Americans": "hispanic",
    "Chilean Americans": "hispanic",
    "Spanish Americans": "hispanic",
    # Asian
    "Asian Americans": "asian",
    "Japanese Americans": "asian",
    "Korean Americans": "asian",
    "Chinese Americans": "asian",
    "Filipino Americans": "asian",
    "Vietnamese Americans": "asian",
    "Taiwanese Americans": "asian",
    "Indian Americans": "asian",
    "Thai Americans": "asian",
    "Hmong Americans": "asian",
    "Cambodian Americans": "asian",
    "Laotian Americans": "asian",
    "Pakistani Americans": "asian",
    # Pacific Islander
    "Samoan Americans": "pacific_islander",
    "Samoans": "pacific_islander",
    "Tongan Americans": "pacific_islander",
    "Tongans": "pacific_islander",
    "Native Hawaiians": "pacific_islander",
    "Pacific Islander Americans": "pacific_islander",
    "Polynesians": "pacific_islander",
    "Fijian Americans": "pacific_islander",
    "Chamorro people": "pacific_islander",
    "Māori": "pacific_islander",
    "Māori people": "pacific_islander",
    # American Indian / Alaska Native
    "Native Americans in the United States": "american_indian",
    "Native Americans": "american_indian",
    "Indigenous peoples of the Americas": "american_indian",
    "Alaska Natives": "american_indian",
    "First Nations in Canada": "american_indian",
    "Cherokee": "american_indian",
    "Navajo": "american_indian",
    "Lumbee": "american_indian",
    "Choctaw": "american_indian",
    "Chickasaw": "american_indian",
    "Muscogee": "american_indian",
    "Lakota people": "american_indian",
    "Sioux": "american_indian",
    # Multiracial
    "Multiracial Americans": "multiracial",
    "multiracial people": "multiracial",
    "Mixed-race people": "multiracial",
}
CATEGORIES = ["black", "white", "hispanic", "asian", "pacific_islander",
              "american_indian", "multiracial"]


# ============================================================================
# Universe (read-only DuckDB)
# ============================================================================

def ascii_fold(text):
    """Accents removed (Tuiasosopo, Ngata stay; Peña -> Pena)."""
    if not isinstance(text, str):
        return ""
    return unicodedata.normalize("NFKD", text).encode("ascii", "ignore").decode()


def title_name(title):
    """Article title without a disambiguator: 'Aaron Curry (American football)'
    -> 'Aaron Curry'."""
    return re.sub(r"\s*\([^)]*\)\s*$", "", title or "").strip()


def load_universe(db_path, limit=None, seed=0):
    """Staff and player persons with their article titles and Wikidata keys.

    Opens the DuckDB READ-ONLY (other processes may be reading it) and closes
    it before any network call. Returns one DataFrame with person_uid, entity,
    person_key, display_name, last_name, wiki_title, wikidata_qid (staff) and
    pfr_id (players). With `limit`, a seeded random sample of `limit` staff
    and `limit` players with an article is kept (for testing).
    """
    try:
        con = duckdb.connect(str(db_path), read_only=True)
    except duckdb.Error as e:
        raise RuntimeError(f"Cannot open {db_path} read-only: {e}") from e
    try:
        staff = con.execute("""
            SELECT person_id AS person_key, person_name AS display_name, last_name,
                   wiki_title, wikidata_qid
            FROM staff_persons""").df()
        players = con.execute("""
            SELECT p.gsis_id AS person_key, p.display_name, p.last_name, p.pfr_id,
                   s.wiki_title
            FROM nfl_players p LEFT JOIN player_wiki_signals s USING (gsis_id)
            WHERE p.gsis_id IS NOT NULL AND p.gsis_id <> ''""").df()
    finally:
        con.close()
    staff["person_uid"] = "staff:" + staff["person_key"]
    staff["entity"] = "staff"
    staff["pfr_id"] = None
    players["person_uid"] = "player:" + players["person_key"]
    players["entity"] = "player"
    players["wikidata_qid"] = None
    if limit:
        staff = staff[staff["wiki_title"].notna()].sample(min(limit, len(staff)),
                                                          random_state=seed)
        players = players[players["wiki_title"].notna()].sample(min(limit, len(players)),
                                                                random_state=seed)
    cols = ["person_uid", "entity", "person_key", "display_name", "last_name",
            "wiki_title", "wikidata_qid", "pfr_id"]
    uni = pd.concat([staff[cols], players[cols]], ignore_index=True)
    if uni["person_uid"].duplicated().any():
        raise RuntimeError("Duplicate person_uid in the universe")
    print(f"Universe: {uni.shape[0]:,} persons x {uni.shape[1]} cols")
    print(uni.groupby("entity").agg(n=("person_uid", "size"),
                                    with_article=("wiki_title", "count"),
                                    with_qid=("wikidata_qid", "count"),
                                    with_pfr=("pfr_id", "count")).to_string())
    return uni


# ============================================================================
# Wikidata ethnic group (P172)
# ============================================================================

def _qid(uri):
    """'http://www.wikidata.org/entity/Q49085' -> 'Q49085'; None for blank
    nodes (an 'unknown value' statement) and literals."""
    tail = uri.rsplit("/", 1)[-1] if isinstance(uri, str) else ""
    return tail if re.fullmatch(r"Q\d+", tail) else None


def sparql_rows(wiki, query, refresh):
    """SPARQL bindings as a list of {variable: value} dicts (cached)."""
    res = wiki.get({"query": query, "format": "json"}, namespace=SPARQL_NS,
                   refresh=refresh, url="https://query.wikidata.org/sparql")
    return [{k: v["value"] for k, v in b.items()} for b in res["results"]["bindings"]]


# Statement, rank and reference parts shared by the staff and player queries
_P172_BLOCK = f"""
    ?item p:{ETHNIC_PROPERTY} ?st . ?st ps:{ETHNIC_PROPERTY} ?eg ; wikibase:rank ?rank .
    FILTER(?rank != wikibase:DeprecatedRank)
    OPTIONAL {{ ?st prov:wasDerivedFrom ?ref .
               OPTIONAL {{ ?ref pr:P854 ?url }}
               OPTIONAL {{ ?ref pr:P248 ?statedin }}
               OPTIONAL {{ ?ref pr:P143 ?imported }} }}"""
_RAW_COLS = ["st", "eg", "rank", "ref", "url", "statedin", "imported"]


def p172_for_items(wiki, qids, refresh):
    """Raw P172 rows for Wikidata items (staff): item, st, eg, rank, ref, ..."""
    rows = []
    for chunk in staff_mod.chunks(sorted(set(qids)), SPARQL_BATCH):
        values = " ".join(f"wd:{q}" for q in chunk)
        q = (f"SELECT ?item ?st ?eg ?rank ?ref ?url ?statedin ?imported WHERE {{ "
             f"VALUES ?item {{ {values} }} {_P172_BLOCK} }}")
        rows += sparql_rows(wiki, q, refresh)
    return pd.DataFrame(rows, columns=["item", *_RAW_COLS])


def p172_for_pfr(wiki, pfr_ids, refresh):
    """Raw rows for PFR player ids: every item carrying P3561 (with or without
    P172), so the result also says which players have a Wikidata item."""
    rows = []
    for chunk in staff_mod.chunks(sorted(set(pfr_ids)), SPARQL_BATCH):
        values = " ".join(f'"{p[0].upper()}/{p}"' for p in chunk)
        q = (f"SELECT ?pfr ?item ?st ?eg ?rank ?ref ?url ?statedin ?imported WHERE {{ "
             f"VALUES ?pfr {{ {values} }} ?item wdt:{PFR_PROPERTY} ?pfr . "
             f"OPTIONAL {{ {_P172_BLOCK} }} }}")
        rows += sparql_rows(wiki, q, refresh)
    df = pd.DataFrame(rows, columns=["pfr", "item", *_RAW_COLS])
    df["pfr_id"] = df["pfr"].str.split("/").str[-1]
    return df


def english_labels(wiki, qids, refresh):
    """{QID: English label} (QIDs without an English label are absent)."""
    out = {}
    for chunk in staff_mod.chunks(sorted(set(q for q in qids if q)), SPARQL_BATCH):
        values = " ".join(f"wd:{q}" for q in chunk)
        q = (f"SELECT ?x ?label WHERE {{ VALUES ?x {{ {values} }} ?x rdfs:label ?label . "
             f'FILTER(LANG(?label) = "en") }}')
        for r in sparql_rows(wiki, q, refresh):
            out[_qid(r["x"])] = r["label"]
    return out


def map_label(label):
    """Protocol category for an ethnic-group label (exact, then case-blind)."""
    if not isinstance(label, str):
        return None
    if label in ETHNIC_GROUP_MAP:
        return ETHNIC_GROUP_MAP[label]
    folded = {k.casefold(): v for k, v in ETHNIC_GROUP_MAP.items()}
    return folded.get(label.casefold())


def _join(values):
    vals = sorted({v for v in values if isinstance(v, str) and v})
    return "; ".join(vals) if vals else None


def build_wikidata(wiki, uni, refresh):
    """(ethnicity, items): one row per person x item x P172 statement, and one
    row per person x Wikidata item (for coverage)."""
    staff = uni[(uni["entity"] == "staff") & uni["wikidata_qid"].notna()]
    players = uni[(uni["entity"] == "player") & uni["pfr_id"].notna()]
    t0 = time.time()
    raw_s = p172_for_items(wiki, staff["wikidata_qid"], refresh)
    raw_p = p172_for_pfr(wiki, players["pfr_id"], refresh)
    print(f"Wikidata: {len(raw_s):,} staff rows, {len(raw_p):,} player rows "
          f"({time.time() - t0:.0f}s)")

    # Person x item keys
    items_s = staff[["person_uid", "wikidata_qid"]].rename(columns={"wikidata_qid": "qid"})
    raw_s["qid"] = raw_s["item"].map(_qid)
    raw_p["qid"] = raw_p["item"].map(_qid)
    items_p = (players[["person_uid", "pfr_id"]]
               .merge(raw_p[["pfr_id", "qid"]].drop_duplicates(), on="pfr_id")
               [["person_uid", "qid"]])
    items = pd.concat([items_s, items_p], ignore_index=True).drop_duplicates()

    # Statements with their references
    raw = pd.concat([raw_s, raw_p], ignore_index=True)
    raw = raw[raw["st"].notna()].copy()
    raw["ethnic_group_qid"] = raw["eg"].map(_qid)
    raw["rank"] = raw["rank"].str.rsplit("#", n=1).str[-1].str.replace("Rank", "")
    raw["statedin_qid"] = raw["statedin"].map(_qid)
    raw["imported_qid"] = raw["imported"].map(_qid)
    stm = (raw.groupby(["qid", "st"], dropna=False)
              .agg(ethnic_group_qid=("ethnic_group_qid", "first"),
                   rank=("rank", "first"),
                   n_references=("ref", "nunique"),
                   reference_urls=("url", _join),
                   stated_in_qids=("statedin_qid", _join),
                   imported_from_qids=("imported_qid", _join))
              .reset_index())
    labels = english_labels(wiki, set(stm["ethnic_group_qid"])
                            | {q for s in stm["stated_in_qids"].dropna() for q in s.split("; ")}
                            | {q for s in stm["imported_from_qids"].dropna()
                               for q in s.split("; ")}, refresh)
    stm["ethnic_group_label"] = stm["ethnic_group_qid"].map(labels)
    stm["mapped_category"] = stm["ethnic_group_label"].map(map_label)
    lab = lambda s: None if not isinstance(s, str) else \
        "; ".join(labels.get(q, q) for q in s.split("; "))
    stm["stated_in"] = stm["stated_in_qids"].map(lab)
    stm["imported_from"] = stm["imported_from_qids"].map(lab)
    # Sourced = at least one reference with a URL or a 'stated in' work
    # (a reference that only says 'imported from Wikimedia project' is not)
    stm["sourced"] = stm["reference_urls"].notna() | stm["stated_in_qids"].notna()

    eth = (items.merge(stm, on="qid", how="inner")
                .merge(uni[["person_uid", "entity", "person_key", "display_name", "pfr_id"]],
                       on="person_uid", how="left"))
    for cat in CATEGORIES:
        eth[f"cat_{cat}"] = eth["mapped_category"].fillna("").str.split(";").map(
            lambda parts, c=cat: c in parts)
    cols = ["person_uid", "entity", "person_key", "display_name", "qid", "pfr_id",
            "st", "ethnic_group_qid", "ethnic_group_label", "mapped_category",
            *[f"cat_{c}" for c in CATEGORIES], "rank", "n_references", "sourced",
            "reference_urls", "stated_in", "imported_from"]
    eth = eth.rename(columns={"st": "statement_id"})
    cols[cols.index("st")] = "statement_id"
    eth["statement_id"] = eth["statement_id"].str.rsplit("/", n=1).str[-1]
    eth = eth[cols].sort_values(["entity", "person_uid", "ethnic_group_qid"])
    return eth.reset_index(drop=True), items.reset_index(drop=True)


# ============================================================================
# Article text: cache index and resumable fetch
# ============================================================================

class TextIndex:
    """Title index over every cached MediaWiki revisions response.

    Responses are cached per request (common.Wiki), so a title can sit in any
    file. The index records, for each page with content, the file holding its
    newest revision, plus the normalization and redirect maps and the titles
    the API reported missing. Only titles the index cannot resolve are
    requested again, so a rerun resumes from the cache and an interrupted
    run loses at most one request.
    """

    def __init__(self, folders):
        self.folders = [Path(f) for f in folders]
        self.scanned = set()
        self.norm, self.redir = {}, {}
        self.pages = {}            # resolved title -> (path, revid)
        self.missing = set()       # resolved titles the API reported missing
        self.n_bad_files = 0

    def scan(self):
        """Read every cache file not read yet. Returns the number of new files."""
        new = [p for f in self.folders if f.exists() for p in sorted(f.glob("*.json"))
               if p not in self.scanned]
        for path in new:
            self.scanned.add(path)
            try:
                d = json.loads(path.read_text())
            except (OSError, json.JSONDecodeError) as e:
                print(f"  WARNING: skipping unreadable cache file {path}: {e}")
                self.n_bad_files += 1
                continue
            q = d.get("query", {})
            self.norm.update({x["from"]: x["to"] for x in q.get("normalized", [])})
            self.redir.update({x["from"]: x["to"] for x in q.get("redirects", [])})
            for page in q.get("pages", []):
                title = page.get("title")
                if title is None:
                    continue
                if page.get("missing") or page.get("invalid"):
                    self.missing.add(title)
                    continue
                for rev in page.get("revisions", []):
                    if "content" not in rev.get("slots", {}).get("main", {}):
                        continue
                    old = self.pages.get(title)
                    if old is None or rev["revid"] > old[1]:
                        self.pages[title] = (path, rev["revid"])
        return len(new)

    def resolve(self, title):
        """Requested title -> resolved title (after normalization/redirects)."""
        n = self.norm.get(title, title)
        return self.redir.get(n, n)

    def status(self, title):
        """'cached', 'missing' or 'unknown' for a requested title."""
        r = self.resolve(title)
        if r in self.pages:
            return "cached"
        if r in self.missing:
            return "missing"
        return "unknown"


def fetch_texts(wiki, titles, index, refresh):
    """Request the current wikitext of every title the index cannot resolve
    (all titles with refresh), TEXT_BATCH titles per request, following
    continuation. Responses are cached by common.Wiki under TEXT_NS; the index
    is rescanned at the end. Returns the number of titles requested."""
    todo = sorted(set(titles) if refresh else
                  {t for t in titles if index.status(t) == "unknown"})
    if not todo:
        return 0
    batches = list(staff_mod.chunks(todo, TEXT_BATCH))
    print(f"Fetching {len(todo):,} articles in {len(batches):,} requests ...", flush=True)
    t0 = time.time()
    for k, chunk in enumerate(batches, 1):
        params = {"action": "query", "prop": "revisions", "titles": "|".join(chunk),
                  "redirects": 1, "rvprop": "ids|timestamp|content", "rvslots": "main"}
        staff_mod.query_all(wiki, params, TEXT_NS, refresh)
        if k % 20 == 0 or k == len(batches):
            rate = (time.time() - t0) / k
            print(f"  {k:,}/{len(batches):,} requests, {time.time() - t0:.0f}s elapsed, "
                  f"~{rate * (len(batches) - k) / 60:.1f} min left", flush=True)
    index.scan()
    return len(todo)


def iter_texts(index, titles):
    """Yield (resolved title, revid, timestamp, wikitext) for every cached
    title, reading each cache file once."""
    by_file = {}
    for t in set(titles):
        if t in index.pages:
            path, revid = index.pages[t]
            by_file.setdefault(path, {})[t] = revid
    for path, wanted in by_file.items():
        try:
            d = json.loads(path.read_text())
        except (OSError, json.JSONDecodeError) as e:
            print(f"  WARNING: cannot reread {path}: {e}")
            continue
        for page in d.get("query", {}).get("pages", []):
            t = page.get("title")
            if t not in wanted:
                continue
            for rev in page.get("revisions", []):
                main = rev.get("slots", {}).get("main", {})
                if rev.get("revid") == wanted[t] and "content" in main:
                    yield t, rev["revid"], rev.get("timestamp"), main["content"]
                    wanted.pop(t)
                    break


# ============================================================================
# Article text: markup -> sections -> sentences -> candidates
# ============================================================================

HEADING_RE = re.compile(r"^(={2,6})[ \t]*(.+?)[ \t]*\1[ \t]*$", re.M)
TABLE_RE = re.compile(r"\{\|(?:(?!\{\|).)*?\n\|\}", re.S)   # innermost wiki table
BLOCK_RES = [re.compile(r"<!--.*?(?:-->|$)", re.S),
             re.compile(r"<ref[^>/]*/>", re.I),
             re.compile(r"<ref[^>]*>.*?</ref\s*>", re.I | re.S),
             re.compile(r"<(gallery|math|timeline|score|syntaxhighlight|graph)[^>]*>.*?"
                        r"</\1\s*>", re.I | re.S)]
# Templates whose text is kept (first positional parameter unless noted)
KEEP_FIRST = {"nowrap", "small", "smaller", "big", "nobr", "nobreak", "sic", "abbr",
              "nobold", "noitalic", "em", "strong", "linktext", "ill",
              "interlanguage link", "proper name", "lang-en", "tooltip"}
QUOTE_TEMPLATES = {"quote", "blockquote", "cquote", "quotation", "pull quote",
                   "quote box", "rquote", "quote frame", "centered pull quote", "gquote"}
LITERAL_TEMPLATES = {"'": "'", "'s": "'s", "mdash": " — ", "emdash": " — ", "ndash": "–",
                     "snd": " – ", "spaced ndash": " – ", "nbsp": " ", "-": " "}
DROP_LINK_PREFIXES = ("file:", "image:", "category:", "media:")
ABBREVIATIONS = {"jr", "sr", "st", "dr", "mr", "mrs", "ms", "no", "nos", "vs", "v", "inc",
                 "co", "corp", "ltd", "mt", "ft", "gen", "gov", "sen", "rep", "rev", "lt",
                 "col", "capt", "sgt", "prof", "jan", "feb", "mar", "apr", "jun", "jul",
                 "aug", "sep", "sept", "oct", "nov", "dec", "approx", "ave", "dept", "est",
                 "fig", "u.s", "e.g", "i.e", "d.c", "u.k", "p.m", "a.m", "pp", "vol", "ed",
                 "univ", "assn", "bros", "ph.d", "etc", "al"}
BOUNDARY_RE = re.compile(r"[.!?][\"”’')\]]*\s+(?=[\"“‘(\[]?[A-Z0-9])")
PRONOUN_RE = re.compile(r"^\W*(?:He|His|She|Her|Him)\b")
NAME_SUFFIX_RE = re.compile(r",?\s+(?:Jr\.?|Sr\.?|II|III|IV|V)$")


def _param_text(param):
    _flatten(param.value)
    return str(param.value).strip()


def _flatten(code):
    """Replace templates in a Wikicode in place: keep the text of formatting,
    language and quotation templates, drop every other template (infoboxes,
    navboxes, citations, dates)."""
    for tpl in code.filter_templates(recursive=False):
        name = str(tpl.name).strip().lower().replace("_", " ")
        positional = [p for p in tpl.params if not p.showkey]
        named = {str(p.name).strip().lower(): p for p in tpl.params if p.showkey}
        repl = ""
        if name in LITERAL_TEMPLATES:
            repl = LITERAL_TEMPLATES[name]
        elif name in KEEP_FIRST and positional:
            repl = _param_text(positional[0])
        elif name.startswith("lang") and positional:
            repl = _param_text(positional[-1])
        elif name in QUOTE_TEMPLATES:
            p = named.get("text") or named.get("quote") or (positional[0] if positional else None)
            repl = (" " + _param_text(p) + " ") if p is not None else ""
        elif name == "sortname" and len(positional) >= 2:
            repl = f"{_param_text(positional[0])} {_param_text(positional[1])}"
        try:
            code.replace(tpl, repl)
        except ValueError:
            pass


def _plain(body):
    """Plain text of one section body, one paragraph or list item per line."""
    code = mwp.parse(body)
    _flatten(code)
    for link in code.filter_wikilinks(recursive=True):
        if str(link.title).strip().lower().startswith(DROP_LINK_PREFIXES):
            try:
                code.remove(link)
            except ValueError:
                pass
    text = code.strip_code(normalize=True, collapse=False)
    return [re.sub(r"\s+", " ", line).strip() for line in text.splitlines()
            if line.strip()]


def article_sections(wikitext):
    """[(section title, [paragraphs])] with 'Lead' for the text before the
    first heading. Comments, footnotes, tables and galleries are removed."""
    text = wikitext
    for rx in BLOCK_RES:
        text = rx.sub(" ", text)
    while True:                                   # nested tables, innermost first
        new = TABLE_RE.sub(" ", text)
        if new == text:
            break
        text = new
    parts = HEADING_RE.split(text)
    out = [("Lead", _plain(parts[0]))]
    for k in range(1, len(parts) - 2, 3):
        title = mwp.parse(parts[k + 1]).strip_code().strip()
        out.append((title, _plain(parts[k + 2])))
    return out


def split_sentences(paragraph):
    """Sentences of a paragraph. A period after an abbreviation (Jr., St.,
    U.S.) or a single-letter initial (A. J.) does not end a sentence."""
    out, start = [], 0
    for m in BOUNDARY_RE.finditer(paragraph):
        before = paragraph[start:m.start()].rsplit(None, 1)
        word = before[-1].lower().lstrip("(\"'“") if before else ""
        if paragraph[m.start()] == "." and (word in ABBREVIATIONS or len(word) == 1
                                             or re.fullmatch(r"(?:[a-z]\.)+[a-z]", word)):
            continue
        out.append(paragraph[start:m.end()].strip())
        start = m.end()
    tail = paragraph[start:].strip()
    if tail:
        out.append(tail)
    return out


def article_candidates(wikitext):
    """(n_sentences, candidates) for one article. A candidate is a sentence
    with at least one race/ethnicity term; it carries its section, index,
    terms, term groups, and the previous and next sentence."""
    sents = []
    for section, paragraphs in article_sections(wikitext):
        for para in paragraphs:
            sents += [(section, s) for s in split_sentences(para)]
    cands = []
    for i, (section, sent) in enumerate(sents):
        hits = [(m.group(0), TERM_GROUP[m.lastgroup], TERM_NOISY[m.lastgroup])
                for m in TERM_RE.finditer(sent)]
        if not hits:
            continue
        cands.append(dict(
            section=section, sentence_index=i,
            term=hits[0][0],
            terms="; ".join(dict.fromkeys(h[0] for h in hits)),
            term_groups="; ".join(dict.fromkeys(h[1] for h in hits)),
            noisy_only=all(h[2] for h in hits),
            sentence=sent[:MAX_SENTENCE_CHARS],
            prev_sentence=sents[i - 1][1][:MAX_SENTENCE_CHARS] if i > 0 else None,
            next_sentence=sents[i + 1][1][:MAX_SENTENCE_CHARS] if i + 1 < len(sents) else None))
    return len(sents), cands


# ============================================================================
# Subject flags, assembly and outputs
# ============================================================================

def name_patterns(display_name, last_name, title):
    """(full-name regex, surname regex, surname set) for a person,
    accent-insensitive and case-sensitive (a surname is capitalised)."""
    fulls = {NAME_SUFFIX_RE.sub("", ascii_fold(x)).strip()
             for x in (display_name, title_name(title)) if isinstance(x, str) and x.strip()}
    surnames = {ascii_fold(last_name).strip()} if isinstance(last_name, str) else set()
    for full in fulls:
        words = full.split()
        if len(words) > 1:
            surnames.add(words[-1])
    fulls, surnames = sorted(f for f in fulls if f), sorted(s for s in surnames if len(s) > 1)
    rx = lambda xs: re.compile(r"\b(?:" + "|".join(map(re.escape, xs)) + r")\b") if xs else None
    return rx(fulls), rx(surnames), set(surnames)


def subject_flags(sentence, full_rx, surname_rx):
    """mentions_full_name, mentions_surname, starts_with_pronoun."""
    s = ascii_fold(sentence)
    return (bool(full_rx and full_rx.search(s)), bool(surname_rx and surname_rx.search(s)),
            bool(PRONOUN_RE.match(s)))


def _batched(iterable, n):
    batch = []
    for x in iterable:
        batch.append(x)
        if len(batch) == n:
            yield batch
            batch = []
    if batch:
        yield batch


def build_text(wiki, uni, refresh, workers):
    """(candidates, per-title results) for every article in the universe."""
    titles = sorted(set(uni["wiki_title"].dropna()))
    index = TextIndex([WIKI_CACHE / STAFF_TEXT_NS, WIKI_CACHE / TEXT_NS])
    index.scan()
    staff_titles = set(uni.loc[uni["entity"] == "staff", "wiki_title"].dropna())
    reused = sum(index.status(t) == "cached" and index.pages[index.resolve(t)][0].parent.name
                 == "person_text" for t in staff_titles)
    status = pd.Series({t: index.status(t) for t in titles})
    print(f"Articles: {len(titles):,} unique titles; already cached {int((status == 'cached').sum()):,} "
          f"({reused:,} staff texts reused from {STAFF_TEXT_NS}), known missing "
          f"{int((status == 'missing').sum()):,}, to fetch {int((status == 'unknown').sum()):,}")
    t0 = time.time()
    n_req = fetch_texts(wiki, titles, index, refresh)
    print(f"Fetched {n_req:,} titles in {time.time() - t0:.0f}s; "
          f"unreadable cache files: {index.n_bad_files}")

    resolved = {t: index.resolve(t) for t in titles}
    t0 = time.time()
    results = {}                  # resolved title -> (revid, timestamp, n_sentences, cands)
    from concurrent.futures import ProcessPoolExecutor
    with ProcessPoolExecutor(max_workers=workers) as pool:
        for batch in _batched(iter_texts(index, set(resolved.values())), 400):
            outs = pool.map(article_candidates, [b[3] for b in batch], chunksize=8)
            for (title, revid, ts, _), (n, cands) in zip(batch, outs):
                results[title] = (revid, ts, n, cands)
            print(f"  processed {len(results):,} articles ({time.time() - t0:.0f}s)", flush=True)
    return resolved, index, results


def assemble(uni, resolved, index, results, items, eth):
    """(candidates, coverage) DataFrames."""
    rows, cov, n_own_name = [], [], 0
    n_p172 = eth.groupby("person_uid").size()
    qids = items.groupby("person_uid")["qid"].agg(lambda s: "; ".join(sorted(set(s))))
    for r in uni.itertuples(index=False):
        title = r.wiki_title if isinstance(r.wiki_title, str) else None
        res_title = resolved.get(title) if title else None
        res = results.get(res_title)
        cov.append(dict(person_uid=r.person_uid, entity=r.entity, person_key=r.person_key,
                        display_name=r.display_name, wiki_title=title,
                        resolved_title=res_title if res else None,
                        has_article=title is not None, fetched=res is not None,
                        article_missing=bool(title) and index.status(title) == "missing",
                        revid=res[0] if res else None, rev_timestamp=res[1] if res else None,
                        n_sentences=res[2] if res else None,
                        n_candidates=0 if res else None,
                        wikidata_qid=qids.get(r.person_uid),
                        n_p172=int(n_p172.get(r.person_uid, 0))))
        if not res or not res[3]:
            continue
        full_rx, surname_rx, surnames = name_patterns(r.display_name, r.last_name, title)
        for c in res[3]:
            # Every term is the subject's own surname (Quincy Black): not a candidate
            if all(ascii_fold(t) in surnames for t in c["terms"].split("; ")):
                n_own_name += 1
                continue
            cov[-1]["n_candidates"] += 1
            full, sur, pron = subject_flags(c["sentence"], full_rx, surname_rx)
            rows.append(dict(person_uid=r.person_uid, entity=r.entity, person_key=r.person_key,
                             display_name=r.display_name, wiki_title=res_title,
                             revid=res[0], rev_timestamp=res[1], **c,
                             mentions_full_name=full, mentions_surname=sur,
                             starts_with_pronoun=pron))
    print(f"Dropped {n_own_name:,} sentences whose only term is the subject's surname")
    cand = pd.DataFrame(rows)
    cov = pd.DataFrame(cov)
    for col in ("revid", "n_sentences", "n_candidates"):
        cov[col] = cov[col].astype("Int64")
    if len(cand):
        cols = ["person_uid", "entity", "person_key", "display_name", "wiki_title", "revid",
                "rev_timestamp", "section", "sentence_index", "term", "terms", "term_groups",
                "noisy_only", "mentions_full_name", "mentions_surname",
                "starts_with_pronoun", "sentence", "prev_sentence", "next_sentence"]
        cand = cand[cols].sort_values(["entity", "person_uid", "sentence_index"])
    return cand.reset_index(drop=True), cov


def describe(name, df):
    """Print shape and per-column missingness."""
    print(f"\n{name}: {df.shape[0]:,} rows x {df.shape[1]} cols")
    miss = df.isna().mean().round(3)
    parts = [f"{c}={v}" for c, v in miss.items() if v > 0]
    print("  missing share: " + ", ".join(parts) if parts else "  no missing values")


def write_parquet(df, path):
    try:
        path.parent.mkdir(parents=True, exist_ok=True)
        df.to_parquet(path, index=False)
    except (OSError, ValueError, ImportError) as e:
        raise RuntimeError(f"Could not write {path}: {e}") from e
    print(f"  wrote {path} ({len(df):,} rows)")


def report(eth, cand, cov):
    """Counts by entity: Wikidata P172 by mapped category, unmapped labels,
    article coverage and candidate sentences."""
    print("\nWikidata P172 statements (persons) by mapped category and entity:")
    e = eth.assign(mapped_category=eth["mapped_category"].fillna("(unmapped)"))
    print(e.groupby(["mapped_category", "entity"])["person_uid"].nunique()
           .unstack(fill_value=0).to_string())
    print(f"Persons with any P172: {eth.groupby('entity')['person_uid'].nunique().to_dict()}; "
          f"sourced statements: {int(eth['sourced'].sum())} of {len(eth)}")
    unmapped = eth.loc[eth["mapped_category"].isna(), "ethnic_group_label"].fillna("(no label)")
    print("Unmapped ethnic-group labels: "
          + (", ".join(f"{k} ({v})" for k, v in unmapped.value_counts().items()) or "none"))

    print("\nArticle coverage by entity:")
    g = cov.groupby("entity")
    print(pd.DataFrame({"persons": g.size(), "has_article": g["has_article"].sum(),
                        "fetched": g["fetched"].sum(),
                        "article_missing": g["article_missing"].sum(),
                        "sentences": g["n_sentences"].sum(),
                        "with_candidate": g["n_candidates"].apply(lambda s: int((s > 0).sum())),
                        "with_p172": g["n_p172"].apply(lambda s: int((s > 0).sum()))}).to_string())
    if len(cand):
        about = cand["mentions_full_name"] | cand["mentions_surname"] | cand["starts_with_pronoun"]
        c = cand.assign(about_subject=about, specific=~cand["noisy_only"])
        print("\nCandidate sentences by entity:")
        g = c.groupby("entity")
        print(pd.DataFrame({"candidates": g.size(), "persons": g["person_uid"].nunique(),
                            "specific_term": g["specific"].sum(),
                            "names_subject_or_pronoun": g["about_subject"].sum(),
                            "specific_and_subject": g.apply(
                                lambda x: int((x["specific"] & x["about_subject"]).sum()),
                                include_groups=False)}).to_string())
        print("\nCandidates by first term group (a sentence can match several):")
        print(c["term_groups"].str.split("; ").str[0].value_counts().to_string())


def main():
    parser = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    parser.add_argument("--refresh", action="store_true",
                        help="re-download every Wikidata and Wikipedia response")
    parser.add_argument("--no-db", action="store_true",
                        help="write the parquet files only (no DuckDB tables)")
    parser.add_argument("--limit", type=int, default=None,
                        help="test on a seeded sample of N staff and N players with articles")
    parser.add_argument("--out-dir", default=str(OUT_DIR),
                        help="folder for the parquet files (default data/raw/wikipedia/race_documented)")
    parser.add_argument("--workers", type=int, default=6,
                        help="processes for parsing article text (default 6)")
    args = parser.parse_args()
    out_dir = Path(args.out_dir)
    t_start = time.time()

    uni = load_universe(DB_PATH, limit=args.limit)
    wiki = Wiki()
    eth, items = build_wikidata(wiki, uni, args.refresh)
    resolved, index, results = build_text(wiki, uni, args.refresh, args.workers)
    cand, cov = assemble(uni, resolved, index, results, items, eth)

    for name, df in [("wikidata_ethnicity", eth), ("text_candidates", cand),
                     ("article_coverage", cov)]:
        describe(name, df)
        write_parquet(df, out_dir / f"{name}.parquet")
    report(eth, cand, cov)

    if args.no_db:
        print("\n--no-db: DuckDB not touched")
    elif args.limit:
        print("\n--limit: DuckDB not touched (test run)")
    else:
        con = connect()
        try:
            src = "Wikidata P172 (via staff QID / P3561) + Wikipedia article text"
            write_table(con, "race_wikidata_ethnicity", eth, source=src,
                        note="P172 statements mapped with ETHNIC_GROUP_MAP; non-deprecated")
            write_table(con, "race_text_candidates", cand, source=src,
                        note="keyword candidate sentences; NOT classified")
            write_table(con, "race_article_coverage", cov, source=src,
                        note="one row per staff person and player")
        finally:
            con.close()
    print(f"\nDone in {(time.time() - t_start) / 60:.1f} min")


if __name__ == "__main__":
    main()
