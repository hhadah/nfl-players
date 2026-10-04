"""Name-based race/ethnicity probabilities (BIFSG), optionally with county.

This is the SECONDARY race measure. The primary measure is human hand-coding
(data/hand_coded/). Names carry limited information about race, so these
probabilities are noisy proxies; use them for misclassification checks, not
as a drop-in race variable.

Categories (as in the Census surname file): white, black, api, aian and
multi are non-Hispanic single races / two or more races; hispanic is
Hispanic of any race.

Model
-----
Surname only (BISG without geography; Elliott et al. 2009):

    P(r | s) = share of race r among Census 2010 bearers of surname s.

Surname + first name (BIFSG; Voicu 2018, in the Bayes-rule framework of
Imai and Khanna 2016). Assuming the first name f is independent of the
surname s conditional on race, and that P(f | r) is the same in the target
population as among mortgage applicants,

    P(r | f, s)  ∝  P(r | s) · P(f | r),
    P(f | r)      = N_H(f, r) / N_H(r)  ∝  P_H(r | f) / P_H(r),

where H is the Tzioumis (2018) mortgage-application sample: P_H(r | f) is
the file's race share for first name f and P_H(r) is the file's OWN race
marginal (sum over every row, including 'ALL OTHER FIRST NAMES', of
obs x share): 82.3% white, 4.2% Black, 6.9% Hispanic, 6.3% API. Dividing by
the U.S. population marginal instead (the pre-2026-09 code) understates the
Black:white odds about four-fold in every combined prediction.

With no surname, P(r | s) is replaced by the Census marginal P_C(r) (all
surnames, including 'ALL OTHER NAMES').

Optional geography variant (BIFSG with county; Elliott et al. 2009, Voicu
2018), for people with a known home county g (CFBD hometown FIPS):

    P(r | f, s, g)  ∝  P(r | s) · P(f | r) · P(g | r),
    P(g | r)        ∝  P_G(r | g) / P_G(r),

from 2010 Census county counts (CC-EST2019, modified race; API = Asian +
NHPI). This assumes the home county is independent of the names given race
and that the person's race odds follow the county's resident population.
Recruits are not a random draw from their county, so this sharpens but does
not calibrate the posterior. County is much coarser than the tract or block
group used by Voicu (2018).

Data handling
-------------
* Census '(S)' cells (small positive counts suppressed for confidentiality):
  the row residual 100 - sum(reported shares) is spread equally over the
  suppressed cells, then the row is rescaled to sum to 1.
* Smoothing: all tables are shrunk toward their own marginal with Dirichlet
  pseudo-counts, P~(r | name) = (n p(r | name) + a m_r) / (n + a), where n is
  the name's count (Census 'count', Tzioumis 'obs', county population) and m
  the table marginal. The concentration a is estimated by maximum marginal
  likelihood of a Dirichlet-multinomial across the listed names (3.3
  pseudo-observations for surnames, 3.5 for first names, 7.5 for counties). A zero cell therefore lowers a race's
  probability but never vetoes it (JEROD: 0% Black in HMDA, P~(Black) = 0.003).
* Unmatched names are not treated as uninformative: an unmatched surname uses
  the Census 'ALL OTHER NAMES' row and an unmatched first name the Tzioumis
  'ALL OTHER FIRST NAMES' row. Among mortgage applicants, unlisted first names
  are disproportionately API and Black (likelihood ratios 4.5 and 2.7 vs the
  file marginal). Whether that carries over to a younger population with
  different naming patterns (JALEN, CONNOR, KALEB are all unlisted) is an
  assumption, not a fact. It also applies to initials (AJ, DJ) and to
  nicknames missing from the list (ZAC, NORV, BUTCH), which pushes white
  coaches known by a nickname toward Black (Zac Taylor, Norv Turner and
  Butch Davis are labelled Black in the 2026-09 build).
* Normalization: ASCII-fold, upper-case, drop apostrophes and periods, drop
  generational suffixes (JR, SR, II, III, IV, V). Lookup candidates, in
  order: all letters concatenated (Census stores O'NEILL as ONEILL and
  DE LA CRUZ as DELACRUZ); then the first and the last hyphen component;
  then the last word for surnames (ST. BROWN -> BROWN) or the first word for
  first names. The first candidate found in the table is used.
* Probabilities are on a 0-1 scale and sum to 1.

Known limits: the Census and HMDA reference populations (U.S. residents,
mortgage applicants) are not NFL players or coaches, so P(r | s) carries the
U.S. race mix and the posterior is not calibrated for football. In the
2026-09 build, NFL players active 2010-2025 have mean P(Black) = 0.32 and
34% are labelled Black (for the players linked to a CFBD roster, mean
P(Black) is 0.33 from names and 0.38 with the home county), against an
approximate external benchmark of 55-60% Black. Adding an NFL-specific prior would make the posterior mostly prior
(the 2026-09 audit found it labels Joe Burrow Black), so none is used. The
county variant exists only for people with a CFBD hometown county. The
argmax label is a misclassified binary proxy with non-classical error;
do not use it as the race regressor without a validation sample.

References
----------
Elliott, M. N., et al. (2009). Using the Census Bureau's surname list to
  improve estimates of race/ethnicity and associated disparities. Health
  Services and Outcomes Research Methodology 9(2): 69-83.
Imai, K., and K. Khanna (2016). Improving ecological inference by predicting
  individual ethnicity from voter registration records. Political Analysis
  24(2): 263-272.
Tzioumis, K. (2018). Demographic aspects of first names. Scientific Data 5:
  180025. doi:10.7910/DVN/TYJKEZ.
Voicu, I. (2018). Using first name information to improve race and ethnicity
  classification. Statistics and Public Policy 5(1): 1-13.
"""
import hashlib
import json
import re
import unicodedata
from functools import lru_cache

import duckdb
import numpy as np
import pandas as pd

from config import REFERENCE_DIR

CENSUS_CSV = REFERENCE_DIR / "census_2010_surnames.csv"
TZIOUMIS_CSV = REFERENCE_DIR / "tzioumis_firstnames.csv"
COUNTY_CSV = REFERENCE_DIR / "census_2010_county_race.csv"
SOURCES_JSON = REFERENCE_DIR / "SOURCES.json"

RACES = ["white", "black", "hispanic", "api", "aian", "multi"]
PROB_COLS = [f"p_{r}" for r in RACES]
_PCT_COLS = ["pctwhite", "pctblack", "pcthispanic", "pctapi", "pctaian", "pct2prace"]

CENSUS_OTHER = "ALL OTHER NAMES"
TZIOUMIS_OTHER = "ALL OTHER FIRST NAMES"
SUFFIXES = {"JR", "SR", "II", "III", "IV", "V"}

# County FIPS codes retired between 2010 and 2019 -> the code CC-EST2019 uses
# (Bedford city VA merged into Bedford County, 2013; Wade Hampton AK renamed
# Kusilvak, 2015; Shannon SD renamed Oglala Lakota, 2015).
FIPS_SUCCESSORS = {"51515": "51019", "02270": "02158", "46113": "46102"}


# ============================================================================
# Reference tables
# ============================================================================

def _check_sources(path):
    """Fail loudly if a reference file is missing or differs from the SHA-256
    recorded by 00_fetch_reference.py; warn if it is an archive fallback."""
    if not path.exists() or not SOURCES_JSON.exists():
        raise FileNotFoundError(
            f"{path} (or {SOURCES_JSON}) is missing: run scripts/00_fetch_reference.py")
    rec = json.loads(SOURCES_JSON.read_text()).get(path.name)
    digest = hashlib.sha256(path.read_bytes()).hexdigest()
    if rec is None or rec["sha256"] != digest:
        raise RuntimeError(f"{path.name} does not match SOURCES.json: "
                           "re-run scripts/00_fetch_reference.py --refresh")
    if rec.get("fallback_from_archive"):
        print(f"  WARNING: {path.name} is the ARCHIVE FALLBACK copy, not a fresh download")
    return rec


def _eb_concentration(counts, marginal):
    """Dirichlet concentration a maximizing the Dirichlet-multinomial marginal
    likelihood of the (names x races) count matrix, with the mean fixed at
    `marginal`. Grid search on log a (DuckDB supplies lgamma)."""
    long = pd.DataFrame({"x": counts.ravel(), "m": np.tile(marginal, len(counts))})
    totals = pd.DataFrame({"n": counts.sum(axis=1)})
    grid = pd.DataFrame({"a": np.exp(np.linspace(np.log(0.01), np.log(1000), 241))})
    con = duckdb.connect()
    for name, df in [("cells", long), ("totals", totals), ("grid", grid)]:
        con.register(name, df)
    best = con.execute("""
        WITH c AS (SELECT a, sum(lgamma(x + a * m) - lgamma(a * m)) AS ll
                   FROM cells, grid GROUP BY a),
             t AS (SELECT a, sum(lgamma(a) - lgamma(n + a)) AS ll
                   FROM totals, grid GROUP BY a)
        SELECT a FROM c JOIN t USING (a) ORDER BY c.ll + t.ll DESC LIMIT 1
    """).fetchone()[0]
    con.close()
    return float(best)


def _prepare(names, counts, shares, other):
    """Smoothed P(r | name) table (index = name) plus the table marginal.

    shares: (n, 6) array of shares on a 0-1 scale that already sum to 1.
    """
    cell_counts = counts[:, None] * shares
    marginal = cell_counts.sum(axis=0) / cell_counts.sum()
    listed = names != other if other else np.ones(len(names), dtype=bool)
    alpha = _eb_concentration(cell_counts[listed], marginal)
    smoothed = (cell_counts + alpha * marginal) / (counts[:, None] + alpha)
    table = pd.DataFrame(smoothed, index=names, columns=RACES)
    return table, marginal, alpha


@lru_cache(maxsize=1)
def load_reference():
    """Load, clean and smooth both reference tables (cached per process).

    Returns a dict with: census (P(r|s), index = surname), census_marginal,
    census_alpha, first (P_H(r|f), index = first name), first_marginal
    (P_H(r)), first_alpha, sources (SOURCES.json records).
    """
    sources = {p.name: _check_sources(p) for p in (CENSUS_CSV, TZIOUMIS_CSV)}

    # Census: keep 'NULL' and 'NA' as surnames; '(S)' marks suppressed cells.
    census = pd.read_csv(CENSUS_CSV, dtype={"name": str}, keep_default_na=False,
                         na_values=["(S)"])
    pct = census[_PCT_COLS].to_numpy(dtype=float)
    suppressed = np.isnan(pct)
    residual = np.clip(100.0 - np.nansum(pct, axis=1), 0.0, None)
    fill = residual / np.maximum(suppressed.sum(axis=1), 1)
    pct = np.where(suppressed, fill[:, None], pct)
    shares = pct / pct.sum(axis=1, keepdims=True)
    c_table, c_marg, c_alpha = _prepare(census["name"].to_numpy(),
                                        census["count"].to_numpy(dtype=float),
                                        shares, CENSUS_OTHER)

    # Tzioumis first names (no suppression; rows sum to 100 up to rounding).
    first = pd.read_csv(TZIOUMIS_CSV, dtype={"firstname": str}, keep_default_na=False)
    pct = first[_PCT_COLS].to_numpy(dtype=float)
    shares = pct / pct.sum(axis=1, keepdims=True)
    f_table, f_marg, f_alpha = _prepare(first["firstname"].to_numpy(),
                                        first["obs"].to_numpy(dtype=float),
                                        shares, TZIOUMIS_OTHER)

    return dict(census=c_table, census_marginal=c_marg, census_alpha=c_alpha,
                first=f_table, first_marginal=f_marg, first_alpha=f_alpha,
                sources=sources)


@lru_cache(maxsize=1)
def load_county():
    """Smoothed P_G(r | county) (index = 5-digit FIPS), the national marginal
    P_G(r) and the smoothing concentration, from the 2010 Census counts."""
    rec = _check_sources(COUNTY_CSV)
    county = pd.read_csv(COUNTY_CSV, dtype={"county_fips": str})
    counts = county[RACES].to_numpy(dtype=float)
    total = counts.sum(axis=1)
    table, marginal, alpha = _prepare(county["county_fips"].to_numpy(), total,
                                      counts / total[:, None], other=None)
    return dict(county=table, county_marginal=marginal, county_alpha=alpha,
                source=rec)


# ============================================================================
# Name normalization
# ============================================================================

def _missing(x):
    return x is None or x is pd.NA or (np.isscalar(x) and pd.isna(x))


def name_tokens(name):
    """Upper-case ASCII word tokens of a name, generational suffixes removed.

    Hyphens and spaces separate tokens; apostrophes and periods are dropped
    inside a token (D'ANDRE -> DANDRE, A.J. -> AJ). Hyphen boundaries are
    kept as a '-' token so callers can recover the hyphen components.
    """
    if _missing(name):
        return []
    text = unicodedata.normalize("NFKD", str(name)).encode("ascii", "ignore").decode()
    text = re.sub(r"['.`]", "", text.upper())
    text = re.sub(r"\s*-\s*", " - ", text)
    tokens = [t for t in re.split(r"[^A-Z-]+", text) if t]
    words = [t for t in tokens if t != "-"]
    if len(words) > 1:
        tokens = [t for t in tokens if t not in SUFFIXES]
    # Trim dangling hyphen markers left by removed tokens.
    while tokens and tokens[0] == "-":
        tokens.pop(0)
    while tokens and tokens[-1] == "-":
        tokens.pop()
    return tokens


def lookup_candidates(name, word="last"):
    """Ordered lookup keys for one name (see module docstring).

    word='last' (surnames) or 'first' (first names) picks which single word
    is the final fallback for multi-word names.
    """
    tokens = name_tokens(name)
    words = [t for t in tokens if t != "-"]
    if not words:
        return []
    components = "".join(t if t != "-" else " " for t in tokens).split()
    candidates = ["".join(words), components[0], components[-1],
                  words[-1] if word == "last" else words[0]]
    seen = []
    for c in candidates:
        if len(c) >= 2 and c not in seen:
            seen.append(c)
    return seen


def county_fips5(value, state_fips=None):
    """5-digit county FIPS string from a code stored as text or number.

    4-5 digits are state + county (1001 -> '01001'); longer tract/block codes
    keep their county prefix. 1-3 digits are a county code without its state
    (CFBD stores e.g. '053' for Pierce County, WA): the 2-digit state_fips is
    prefixed when known, else None. Codes retired before 2019 map to the
    CC-EST2019 county that absorbed or renamed them (FIPS_SUCCESSORS).
    """
    if _missing(value):
        return None
    match = re.fullmatch(r"(\d+)(?:\.0*)?", str(value).strip())
    if not match:
        return None
    digits = match.group(1)
    if len(digits) <= 3:
        fips = state_fips + digits.zfill(3) if state_fips else None
    else:
        fips = digits.zfill(5)[:5]
    return FIPS_SUCCESSORS.get(fips, fips)


def split_full_name(full):
    """(first, last) from a 'First [Middle] Last [Suffix]' string."""
    if _missing(full):
        return None, None
    words = [w for w in str(full).split() if w.strip(".,").upper() not in SUFFIXES]
    if not words:
        return None, None
    if len(words) == 1:
        return None, words[0]
    return words[0], words[-1]


def _match(values, table, word):
    """For each input (a tuple of alternative spellings), the first lookup key
    found in `table`; if none matches, the first candidate of the first
    non-empty alternative (matched=False). Empty inputs give (None, False)."""
    cache = {}
    keys, matched = [], []
    for alternatives in values:
        if alternatives not in cache:
            fallback, hit = None, None
            for name in alternatives:
                cands = lookup_candidates(name, word)
                if cands and fallback is None:
                    fallback = cands[0]
                hit = next((c for c in cands if c in table.index), None)
                if hit:
                    break
            cache[alternatives] = (hit, True) if hit else (fallback, False)
        key, ok = cache[alternatives]
        keys.append(key)
        matched.append(ok)
    return keys, np.array(matched)


# ============================================================================
# Prediction
# ============================================================================

def predict_race(last_names, first_names, first_names_alt=None, county_fips=None):
    """BIFSG probabilities for aligned sequences of names.

    first_names_alt (optional) is a second first-name spelling (e.g. the
    preferred name when first_names holds the legal name); it is used only
    when first_names does not match the Tzioumis list. county_fips (optional)
    adds the county likelihood P(g | r) where the county is known.

    Returns a DataFrame (same length and order as the inputs) with columns
    first_name_used, last_name_used, p_white ... p_multi, race_bifsg,
    race_surname_only, first_name_matched, surname_matched, method; with
    county_fips also county_fips and county_matched (method gets '+county'
    where the county likelihood was applied).
    """
    ref = load_reference()
    census, first = ref["census"], ref["first"]
    n = len(last_names)
    alt = first_names_alt if first_names_alt is not None else [None] * n

    last_key, last_ok = _match([(s,) for s in last_names], census, "last")
    first_key, first_ok = _match(list(zip(first_names, alt)), first, "first")
    has_last = np.array([k is not None for k in last_key])
    has_first = np.array([k is not None for k in first_key])

    # P(r | s): matched surname, else 'ALL OTHER NAMES', else Census marginal.
    s_rows = [k if ok else CENSUS_OTHER for k, ok in zip(last_key, last_ok)]
    p_s = census.loc[s_rows].to_numpy(copy=True)
    p_s[~has_last] = ref["census_marginal"]

    # P(f | r) up to a constant: P_H(r | f) / P_H(r); 1 when no first name.
    f_rows = [k if ok else TZIOUMIS_OTHER for k, ok in zip(first_key, first_ok)]
    lik_f = first.loc[f_rows].to_numpy() / ref["first_marginal"]
    lik_f[~has_first] = 1.0

    post = p_s * lik_f
    none = ~has_last & ~has_first

    # P(g | r) up to a constant: P_G(r | g) / P_G(r); 1 when county unknown.
    if county_fips is not None:
        geo = load_county()
        fips = [county_fips5(v) for v in county_fips]
        county_ok = np.array([f in geo["county"].index for f in fips])
        lik_g = np.ones_like(post)
        lik_g[county_ok] = (geo["county"].loc[[f for f, ok in zip(fips, county_ok) if ok]]
                            .to_numpy() / geo["county_marginal"])
        county_ok &= ~none
        post = post * lik_g

    post = post / post.sum(axis=1, keepdims=True)
    post[none] = np.nan

    out = pd.DataFrame(post, columns=PROB_COLS)
    labels = np.array(RACES, dtype=object)
    out["race_bifsg"] = np.where(none, None, labels[np.nan_to_num(post).argmax(axis=1)])
    out["race_surname_only"] = np.where(has_last, labels[p_s.argmax(axis=1)], None)
    out.insert(0, "first_name_used", first_key)
    out.insert(1, "last_name_used", last_key)
    out["first_name_matched"] = first_ok
    out["surname_matched"] = last_ok
    out["method"] = np.select([has_last & has_first, has_last, has_first],
                              ["bifsg", "surname_only", "first_name_only"], "no_name")
    if county_fips is not None:
        out["county_fips"] = fips
        out["county_matched"] = county_ok
        out.loc[county_ok, "method"] = out.loc[county_ok, "method"] + "+county"
    return out
