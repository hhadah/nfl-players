"""Predicted race for NFL staff and players: a model-only posterior from names
(and hometown county) under an NFL-specific prior (primary), a documented-race
variant (sensitivity), and the documented evidence itself (validation).

Implements notes/race-prediction-design.md, sections 1-2, as revised on
2026-10-02 for the pay and team-performance estimands. Race enters the
analysis as a probability; argmax labels are descriptive only. Categories
follow the Census surname file (race_utils.RACES): white, black, api, aian and
multi are non-Hispanic single races / two or more races; hispanic is Hispanic
of any race.

Variants
--------
pred     PRIMARY. Model-only posterior for EVERY person. The prior is
         estimated by EM on the full population of each entity: documented
         persons enter through their name/county likelihood only, and their
         documented labels are not inputs. The prior conditions only on
         covariates fixed before the outcomes:
           players  position at NFL entry (pos_group: fine position, K/P/LS
                    pooled as ST), rookie-season era, draft bucket, college
                    type, county_available (a home-county likelihood exists)
           staff    role group and unit at FIRST appearance in the staff data,
                    first-season era
         (Revised 2026-10-02 after review: former_player left the primary
         staff prior, because it is coded 'yes' only through a Wikidata link,
         i.e. only for persons with an article; county_available entered the
         player prior because county coverage follows CFBD recruit coverage,
         which varies with draft status and college type within the cells.)
preddoc  SENSITIVITY (fame-dependent documentation). Documented one-hot where
         a public source states race; otherwise the posterior under a prior
         estimated on undocumented persons only. X adds fame proxies that
         calibrate the selected undocumented pool: players add has_wiki and
         career_bucket; staff use the most senior role ever held, the modal
         coaching unit, former_player and has_wiki.
Player side variants of the primary (two columns each, p_white / p_black,
plus the prior P(Black)):
  *_pred_nodraft   prior without draft_bucket. The primary prior conditions on
                   draft round, so its P(Black) is partly a function of draft
                   outcomes; draft and draft-selection regressions, and team
                   shares that should not proxy draft capital, use this one.
  *_pred_nocounty  names-only likelihood, prior re-estimated without
                   county_available: the leave-county-out check.
Raked sensitivity (p_white_pred_raked, p_black_pred_raked): see "TIDES".

Why the primary variant ignores documentation. Documentation depends on
fame, and fame is correlated with pay and team quality. If documented persons
get P(Black) = 1 while equally Black but less famous persons get a shrunken
posterior, the measurement error is differential with respect to the
outcomes: the Black pay coefficient picks up the pay of famous players, and
team diversity tracks talent. In the primary variant every person has the
same kind of information (names, county where available, predetermined X).
County availability differs across persons; it is in X, so the prior is
fitted separately for the two information sets. The error is non-differential
given X if names and county are independent of the outcome given race and X.
Regression calibration also needs p calibrated given the regression's full
conditioning set and no unmodelled race x control interactions; see the
design note, section 4.

Why only predetermined covariates. Career length and article existence are
partly outcomes of success after a contract or season, and the most senior
role ever held leaks later promotions into team-season composition. The
primary prior excludes them. Staff data start in 1999 (partial before 2007),
so "first appearance" is the first season observed, not the career start.

Documented race (validation; preddoc)
-------------------------------------
Sources, each optional (a missing input is skipped and reported):
  wikidata  Wikidata "ethnic group" (P172) statements mapped to protocol
            categories by 04d_race_documented.py
            (data/raw/wikipedia/race_documented/wikidata_ethnicity.parquet).
  category  Wikipedia category flags (staff_person_wiki_signals,
            player_wiki_signals): cat_black, cat_hispanic_latino, cat_asian,
            cat_pacific_islander, cat_native_american.
  text      data/derived/race_text_labels.csv: classified article sentences
            stating the subject's own race (has_statement TRUE rows only;
            rows with confidence >= 2 used). If a staff person and his linked
            player uid both have a text label, the staff label wins.
The keyword evidence columns of staff_person_wiki_signals are NOT used.
Combination, per person (a staff person who played, Wikidata link in
person_links.csv, pools the evidence of both uids):
  * Races stated = union over sources of black, white, asian,
    pacific_islander, american_indian. documented_black_any = 1 if Black is
    stated, alone or in combination.
  * Two or more races from one source, or an explicit multiracial statement:
    'multi' (documented_components).
  * Sources that each state a single, different race, with no multiracial
    statement: documented_conflict; preddoc falls back to the model and
    leaves the person out of its prior fit.
  * Hispanic is ethnicity: a person stated to be Hispanic is 'hispanic'
    (Census scheme), whatever race is stated with it; a Black statement still
    sets documented_black_any = 1. No Hispanic statement = non-Hispanic.
  * Positive-only: no statement is never evidence of race.

Name and hometown likelihood
----------------------------
race_bifsg's posterior is P(r | f, s) ∝ P_C(r) P(s | r) P(f | r), with P_C the
Census surname-file marginal. Dividing by P_C gives
    L_i(r) ∝ P(s_i | r) · P(f_i | r) [· P(g_i | r)],   normalized over r.
For a player linked to a CFBD recruit (player_xwalk.recruit_id, else
player_recruit_xwalk) whose home county matched, P(g | r) ∝ P_G(r | g) / P_G(r)
(race_utils.load_county) multiplies the name likelihood; failing that, the
county of a linked CFBD roster row. The county comes from race_bifsg_geo; the
names are nfl_players' (the recruit spelling differs for ~15% of links). A
link with two counties is not used. No usable name: L = 1, posterior = prior
(pred_method 'prior_only').
First-name factor: for aian and multi, whose Tzioumis marginals are ~0.16%,
race_utils' common Dirichlet smoothing (a = 3.5 pseudo-applicants) leaves
likelihood ratios that rest on one or two applicants (MEL: LR_multi = 10 from
one applicant of 61; DOUG: 0.01 from zero applicants), and the EM prior turns
that noise into probability. For these categories (FIRST_NAME_NEUTRAL) the
first-name factor is set to 1; surname and county still inform them
(Polynesian surnames carry 'multi' legitimately). A count-based shrinkage,
LR = (observed + 5) / (expected + 5) applicants, was also tried: documented
Black vs non-Black AUC 0.895 vs 0.890, but the EM gave 'multi' an even larger
prior (17% for 2021+ rookies) and lowered P(Black), so it is not used. Either
way 'multi' is weakly identified against 'black' (the Census surname file
separates them little): its prior rose from 3-7% to 8-12% for post-2005
rookies, which is why p_black_or_multi_pred is written for robustness.
A staff person who played (Wikidata link) uses the likelihood of his PLAYER
record (legal and common first name, home county) when it has a usable name
(likelihood_uid); the staff and player name likelihoods are never multiplied,
which would count the same name twice.

Prior, estimated by EM (both variants)
--------------------------------------
pi(r | X_i) = softmax(X_i gamma_r), main effects of categorical covariates
(dummies; the first level listed in PRIMARY_LEVELS / PREDDOC_LEVELS is the
reference), maximizing sum_i log sum_r pi(r | X_i) L_i(r) over the fit set:
  E-step  w_ir = pi(r | X_i) L_i(r) / sum_r' pi(r' | X_i) L_i(r')
  M-step  weighted multinomial logit (sklearn LogisticRegression, lbfgs, L2,
          C = 10) of the race index on X, the data stacked once per race with
          weights w_ir; persons with identical X are collapsed into one cell
          with summed weights (same weighted likelihood, faster).
Start from the BIFSG posterior (Census prior); stop when the log-likelihood
changes by < 1e-6 (relative), at most 500 iterations.
Fit sets: pred (and its side variants) = every person with a usable name;
preddoc = undocumented, non-conflict persons with a usable name. A staff
person who played has two rows: the player row keeps the PLAYER-prior
posterior (its covariates are predetermined relative to the coaching career),
the staff row combines the staff prior with the player-record likelihood;
linked_uid names the other record. Documentation is pooled over both uids
and reported on both rows. (Until the 2026-10-02 review the player row
inherited the staff posterior, which replaced the player prior and county for
a group selected on a later coaching career.)

Columns: p_*_pred, p_black_any_pred (= p_black_pred, i.e. NON-HISPANIC BLACK
ALONE despite the name, kept for the R loaders: the Black share of 'multi'
and 'hispanic' is not modelled, so Black Hispanic and multiracial Black
persons are in OtherRace), p_black_or_multi_pred (p_black + p_multi, an upper
variant for robustness), prior_*_pred, race_pred_argmax, pred_method,
p_{white,black}_pred_{nodraft,nocounty}, prior_black_pred_{nodraft,nocounty},
p_{white,black}_pred_raked. preddoc: p_*_preddoc,
p_black_any_preddoc (1 for documented Black alone or in combination, 0 for
other documented persons, = p_black_preddoc otherwise), prior_*_preddoc,
p_*_preddoc_model (model-only posterior under the undocumented prior),
race_preddoc_argmax, pred_method_preddoc ('documented' / 'model' /
'prior_only'). Also: documented_*, likelihoods L_*, likelihood_uid,
linked_uid, p_black_bifsg, covariates of both variants.

Validation (printed; stored in the diagnostics file), primary next to preddoc:
TIDES by season; AUC of the model-only P(Black) on documented persons (Black
vs non-Black, Black vs white); named examples; distribution and entropy; the
share of Var(P(Black)) explained by the prior covariates (R^2 on the dummies
and on the covariate-cell means; the rest is within-cell variation from names
and county); the calibration-implied E[p | Black] = E[p^2]/E[p] next to the
mean p of documented Black persons; role holders 2010-2025 (head coaches,
coordinators, GMs): documentation coverage, mean p by documented race, AUC
(model and names only) and the reliability Var(p)/(Var(p) + E[p(1-p)]);
player vs staff record of staff persons who played; sensitivity of P(Black)
to the L2 penalty (C = 10 vs 1e4).

Assumptions and limits
----------------------
* Names and county are independent of X given race, and P(s | r), P(f | r),
  P(g | r) are as in the Census / HMDA reference populations; the second
  fails in known ways (unlisted first names, nicknames; race_utils). The
  prior absorbs what the likelihood cannot explain.
* Main effects only (position x era interactions raised the player
  log-likelihood by ~125 for ~200 parameters in a review refit and left the
  pay coefficient unchanged, so they are not used). Position at entry: the
  draft position, else the modal position in the first season-roster season
  (1999+), else the current nfl_players position; the unit is that of the
  first source, and a generic level (OL, DL, LB, DB) is refined only within
  the unit. Draft bucket R5+ = rounds 5-7, plus rounds 8-12 of the
  drafts before 1994. College type: last CFBD college team and season for
  players linked to a CFBD roster, else nfl_players.college_name through
  school_xwalk at the CFBD season nearest to rookie season - 1 (CFBD starts
  in 2004: earlier players get the 2004 conference). Power = SEC, Big Ten,
  Big 12, ACC, Pac-10/12, Big East through 2012, Notre Dame. HBCU = ever in
  the SWAC, MEAC, CIAA or SIAC (all-HBCU conferences, minus Chowan) or in
  HBCU_EXTRA, or an nflverse college name in HBCU_NFLVERSE (HBCUs absent from
  or outside those conferences in CFBD: Central State OH, Langston, Albany
  State, Morris Brown, ...). No CFBD match (mostly junior colleges): 'unknown'.
* Degenerate cells (ST, HBCU) are held off the boundary by the L2 penalty;
  K, P and LS are pooled, and the diagnostics report the change in P(Black)
  under a 1,000 times weaker penalty.
* preddoc only: career length counts game-day seasons (REG weekly ACT/INA
  rows from 2002, ACT/INA season rows 1999-2001); has_wiki = article found
  through the PFR id on Wikidata, looked up only for race-coding tiers 5-7
  and linked players ('not_checked' otherwise).

TIDES comparison and raking
---------------------------
With data/reference/tides_nfl_race_shares.csv, season means are compared
with TIDES: players weighted by game-day weeks (the average weekly roster) and
as an unweighted headcount of distinct players; head coaches = non-interim
head coaches (32 a season); coordinators = OC, DC, STC; assistant coaches =
coordinators, position, assistant and strength coaches below the head coach
(support_staff excluded), by person-season. Roster regime: the weekly rosters
list ~1,690 distinct players a season in 2008-2015, ~2,650 in 2016 and
2,100-2,400 from 2017, so 2016 is reported separately. TIDES "African-American"
through 2016 put each player in one category (multiracial players inside a
single race); from 2019 "Black" is self-identified and excludes two or more
races (~10%) and non-disclosure, so 2019+ is bracketed between Black alone and
Black + two or more races. The primary posterior is NOT calibrated in levels:
it is below TIDES for players and assistant coaches (at the EM fixed point the
mean posterior equals the mean prior, so the gap reflects the reference
likelihoods, e.g. the HMDA 'ALL OTHER FIRST NAMES' row and nicknames, not an
EM failure). p_{white,black}_pred_raked (SENSITIVITY; the raking uses TIDES,
so out-of-window seasons are the check): the primary prior's Black odds,
against every other category, are multiplied by exp(delta), one delta per
entity. Players: the 2010-2015 mean of the week-weighted season means matches
TIDES African-American (2016 out: roster regime); validated 2019-2023 against
[Black, Black + 2+]. Staff: the 2010-2016 mean of the assistant-coach season
means matches TIDES; applied to every staff person; validated on 2017-2023
assistants and on head coaches. A uniform odds shift changes the slope of
E[R | p, X] in p, so estimates should be shown with the raked columns too.

Inputs: DuckDB (read-only) race_bifsg, race_bifsg_geo, nfl_players,
nfl_rosters_weekly, nfl_rosters_season, nfl_draft_picks, nfl_contract_history,
player_xwalk,
player_recruit_xwalk, player_college_xwalk, college_teams, school_xwalk,
staff_persons, staff_team_season, staff_person_wiki_signals,
player_wiki_signals; person_links.csv; the documented-race files above.
Writes (data/raw/derived_race/, or --out-dir): race_predicted.parquet (one
row per person, OUT_COLS), race_predicted_priors.parquet (gamma by variant x
entity x race x level), race_predicted_diagnostics.parquet (EM paths, level
means, checks, TIDES by season); DuckDB tables race_predicted and
race_predicted_priors (common.write_table) unless --no-db.

Example:  .venv/bin/python scripts/04e_predict_race.py --no-db
"""
import argparse
import re
import time
from pathlib import Path

import duckdb
import numpy as np
import pandas as pd
from sklearn.linear_model import LogisticRegression
from sklearn.metrics import roc_auc_score

from common import connect, write_table
from config import DATA_DIR, DB_PATH, HAND_CODED_DIR, RAW_DIR, WIKI_CACHE
from race_utils import PROB_COLS, RACES, TZIOUMIS_OTHER, load_county, load_reference

OUT_DIR = RAW_DIR / "derived_race"
WIKIDATA_PATH = WIKI_CACHE / "race_documented" / "wikidata_ethnicity.parquet"
TEXT_LABELS_PATH = DATA_DIR / "derived" / "race_text_labels.csv"
TIDES_PATHS = [DATA_DIR / "reference" / "tides_nfl_race_shares.csv",
               RAW_DIR / "reference" / "tides_nfl_race_shares.csv"]
LINKS_PATH = HAND_CODED_DIR / "race_coding" / "person_links.csv"

K = len(RACES)
BLACK = RACES.index("black")
LIK_COLS = [f"L_{r}" for r in RACES]
LIKN_COLS = [f"LN_{r}" for r in RACES]      # names only (no county); not written
VARIANTS = ("pred", "preddoc")


def pcols(v, kind="p"):
    """Column names of one variant: kind 'p' (posterior), 'prior', or 'model'
    (preddoc's model-only posterior)."""
    if kind == "model":
        return [f"p_{r}_{v}_model" for r in RACES]
    return [f"{kind}_{r}_{v}" for r in RACES]


def model_black(v):
    """Model-only P(Black) of a variant (the primary posterior is model-only)."""
    return "p_black_pred" if v == "pred" else "p_black_preddoc_model"


METHOD_COL = {"pred": "pred_method", "preddoc": "pred_method_preddoc"}
TEXT_COLS = ["person_uid", "display_name", "entity", "wiki_title", "revid",
             "has_statement", "race", "multiracial_components", "hispanic",
             "confidence", "quote", "sentence_index", "source_url", "classifier", "note"]
TEXT_RACES = {"black", "white", "asian", "pacific_islander", "american_indian",
              "multiracial", "unknown"}
TEXT_MIN_CONFIDENCE = 2

# Protocol races -> Census categories (Asian and Pacific Islander -> api)
PROTOCOL_RACES = {"black": "black", "white": "white", "asian": "api",
                  "pacific_islander": "api", "american_indian": "aian"}
# Free-text spellings -> protocol tokens (text labels; Wikidata categories)
TOKEN_SYNONYMS = {"african_american": "black", "black_american": "black",
                  "caucasian": "white", "asian_american": "asian",
                  "native_hawaiian": "pacific_islander", "nhpi": "pacific_islander",
                  "native_american": "american_indian", "aian": "american_indian",
                  "alaska_native": "american_indian", "latino": "hispanic",
                  "latina": "hispanic", "hispanic_latino": "hispanic",
                  "multi": "multiracial", "mixed": "multiracial",
                  "mixed_race": "multiracial", "biracial": "multiracial",
                  "two_or_more": "multiracial"}
CATEGORY_FLAGS = {"cat_black": "black", "cat_asian": "asian",
                  "cat_pacific_islander": "pacific_islander",
                  "cat_native_american": "american_indian",
                  "cat_hispanic_latino": "hispanic"}
SOURCE_ORDER = ["wikidata", "category", "text"]

# Era bins (rookie season for players, first staff season for staff)
ERA_BREAKS = [(2005, "<=2005"), (2010, "2006-10"), (2015, "2011-15"),
              (2020, "2016-20"), (9999, "2021+")]

# Covariate levels by variant and entity; the first level is the reference
_ROLES = ["head_coach", "coordinator", "position_coach", "assistant_qc",
          "strength_support", "gm_personnel", "owner_executive"]
_UNITS = ["none", "offense", "defense", "special_teams"]
_ERAS = [lab for _, lab in ERA_BREAKS]
# Position at NFL entry (pos_group): fine position; OL / DL / LB / DB are the
# generic levels of players listed only by unit; K, P and LS are pooled (ST)
_POSITIONS = ["QB", "RB", "FB", "WR", "TE", "OT", "G", "C", "OL", "DE", "DT", "DL",
              "OLB", "ILB", "LB", "CB", "S", "DB", "ST"]
_PLAYER_BASE = {
    "pos_group": _POSITIONS,
    "rookie_era": _ERAS,
    "draft_bucket": ["R1-2", "R3-4", "R5+", "undrafted"],
    "college_type": ["power", "other_fbs", "fcs_or_lower", "hbcu", "unknown"],
    "county_available": ["no", "yes"],
}
_PLAYER_NODRAFT = {k: v for k, v in _PLAYER_BASE.items() if k != "draft_bucket"}
PRIMARY_LEVELS = {        # predetermined covariates only
    "player": _PLAYER_BASE,
    "staff": {"role_group_first": _ROLES, "unit_first": _UNITS, "first_era": _ERAS},
}
PREDDOC_LEVELS = {        # adds fame proxies (sensitivity variant)
    "player": {**_PLAYER_BASE, "has_wiki": ["no", "yes", "not_checked"],
               "career_bucket": ["0", "1", "2-3", "4-6", "7+"]},
    "staff": {"role_group_senior": _ROLES, "unit_modal": _UNITS, "first_era": _ERAS,
              "former_player": ["no", "yes"], "has_wiki": ["no", "yes"]},
}
LEVELS = {"pred": PRIMARY_LEVELS, "preddoc": PREDDOC_LEVELS}
# Player-only side variants of the primary prior (written as two columns each):
#   nodraft   prior without draft_bucket, for draft and draft-selection
#             outcomes and for team shares that should not proxy draft capital
#   nocounty  names-only likelihood (no county factor), prior re-estimated
#             without county_available: the leave-county-out check
SIDE_LEVELS = {"nodraft": _PLAYER_NODRAFT,
               "nocounty": {k: v for k, v in _PLAYER_BASE.items() if k != "county_available"}}
COVARIATE_COLS = list(dict.fromkeys(var for lv in LEVELS.values()
                                    for ent in lv.values() for var in ent))

# staff_team_season.role_group -> (seniority rank, role group for the prior).
# role_group_first: most senior role in the first season observed;
# role_group_senior: most senior role ever held.
ROLE_SENIORITY = {
    "head_coach": (1, "head_coach"),
    "owner_executive": (2, "owner_executive"),
    "general_manager": (3, "gm_personnel"),
    "coordinator": (4, "coordinator"),
    "position_coach": (5, "position_coach"),
    "assistant_coach": (6, "assistant_qc"),
    "strength_conditioning": (7, "strength_support"),
    "support_staff": (8, "strength_support"),
    "personnel_scouting": (9, "gm_personnel"),
    "other_front_office": (10, "gm_personnel"),
}
COACH_GROUPS = ("head_coach", "coordinator", "position_coach", "assistant_coach",
                "strength_conditioning", "support_staff")

# College type
POWER_CONFS = {"SEC", "Big Ten", "Big 12", "ACC", "Pac-12", "Pac-10"}
BIG_EAST_LAST_FBS = 2012          # Big East football became the AAC in 2013
HBCU_CONFS = {"SWAC", "MEAC", "CIAA", "SIAC"}
HBCU_EXTRA = {"Tennessee State", "Lincoln (MO)", "West Virginia State"}
NOT_HBCU = {"Chowan"}             # CIAA member, not an HBCU
# HBCUs (U.S. Department of Education list) as spelled in nfl_players.college_name
# (first ';' component), matched independently of CFBD coverage: schools that
# left football or are absent from college_teams (Central State OH, Langston,
# Albany State, Morris Brown, Knoxville, Cheyney, UMES, Fisk, Bishop College,
# Saint Augustine's, Saint Paul's, Concordia AL) plus the spellings of the
# conference-matched HBCUs
HBCU_NFLVERSE = {
    "Central State University, Oh", "Langston University", "Albany State (GA)",
    "Morris Brown College", "Knoxville College", "University of Cheyney",
    "Maryland Univ. (Eastern Shore)", "Fisk University", "Bishop",
    "Saint Augustine's College", "St. Paul's College", "Concordia College (AL)",
    "Tennessee State", "Grambling State", "Jackson State University", "Southern",
    "South Carolina State", "Florida A&M", "Texas Southern", "Alcorn State", "Hampton",
    "Bethune-Cookman College", "North Carolina A&T", "Howard",
    "University of Arkansas at Pine Bluff", "Alabama State", "Morgan State",
    "Mississippi Valley State University", "Prairie View A&M", "Alabama A&M",
    "North Carolina Central", "Winston Salem State University", "Norfolk State",
    "Delaware State", "Fort Valley State College", "Tuskegee University",
    "Virginia Union", "Lane College", "Johnson C. Smith University",
    "Kentucky State University", "Fayetteville State University",
    "Elizabeth City State Univ", "Clark Atlanta University", "Bowie State",
    "Morehouse College", "Lincoln University (MO)", "Virginia State",
    "Savannah State College", "Stillman College", "West Virginia State",
    "Livingstone College", "Shaw University", "Allen University", "Benedict College"}
FIRST_CFBD_SEASON, LAST_CFBD_SEASON = 2004, 2025

# Position at entry: roster / draft position codes -> fine position (_POSITIONS)
FINE_POSITION = {"QB": "QB", "RB": "RB", "HB": "RB", "FB": "FB", "WR": "WR", "TE": "TE",
                 "T": "OT", "OT": "OT", "G": "G", "OG": "G", "C": "C", "OL": "OL",
                 "DE": "DE", "DT": "DT", "NT": "DT", "DL": "DL", "OLB": "OLB",
                 "ILB": "ILB", "MLB": "ILB", "LB": "LB", "CB": "CB", "S": "S", "SS": "S",
                 "FS": "S", "SAF": "S", "DB": "DB", "K": "ST", "P": "ST", "LS": "ST",
                 "SPEC": "ST"}
BROAD_POSITION = {"QB": "QB", "RB": "RB", "FB": "RB", "WR": "WR", "TE": "TE",
                  "OT": "OL", "G": "OL", "C": "OL", "OL": "OL", "DE": "DL", "DT": "DL",
                  "DL": "DL", "OLB": "LB", "ILB": "LB", "LB": "LB", "CB": "DB", "S": "DB",
                  "DB": "DB", "ST": "ST"}
GENERIC_POSITIONS = {"OL", "DL", "LB", "DB"}
# nfl_players.position_group fallback when no position code maps
GROUP_TO_FINE = {"QB": "QB", "RB": "RB", "WR": "WR", "TE": "TE", "OL": "OL", "DL": "DL",
                 "LB": "LB", "DB": "DB", "K": "ST", "P": "ST", "LS": "ST"}

# Players: contract windows of the race-coding tiers (has_wiki lookup scope)
VETERAN_TYPES = ("UFA", "RFA", "ERFA", "SFA", "Extension", "Franchise", "Transition")
ROOKIE_TYPES = ("Drafted", "UDFA")

# EM
C_PRIOR = 10.0
C_SENSITIVITY = 1e4               # weak-penalty refit reported in the diagnostics
EM_TOL = 1e-6
EM_MAX_ITER = 500

# First-name likelihood: categories whose Tzioumis marginal is ~0.16%, so a
# name's likelihood ratio is driven by one or two applicants (MEL: one
# two-or-more-races applicant out of 61 gives LR_multi = 10). Their ratio is
# set to 1; surname and county still inform them.
FIRST_NAME_NEUTRAL = ("aian", "multi")

# Quality checks
ACTIVE_SEASONS = (2010, 2025)
# Raking windows: TIDES 'African-American' (players) before the 2019 break,
# without 2016, when the weekly rosters list ~2,650 distinct players (about
# 1,690 in 2008-2015 and 2,100-2,400 from 2017); staff: assistant coaches
# 2010-2016, validated 2017-2023
RAKE_SEASONS = (2010, 2015)
STAFF_RAKE_SEASONS = (2010, 2016)
ROSTER_REGIME_BREAK = 2016
EXAMPLE_PLAYERS = ["Joe Burrow", "Tom Brady", "Peyton Manning", "Patrick Mahomes",
                   "Lamar Jackson", "Jalen Hurts", "Russell Wilson", "Aaron Donald",
                   "J.J. Watt", "Travis Kelce", "Justin Jefferson",
                   "Christian McCaffrey", "Cooper Kupp", "Tyreek Hill"]
EXAMPLE_STAFF = ["Mike Tomlin", "Bill Belichick", "Andy Reid", "Sean McVay",
                 "Todd Bowles", "DeMeco Ryans", "Raheem Morris", "Brian Flores",
                 "Kyle Shanahan", "Ron Rivera", "Tony Dungy", "Zac Taylor",
                 "Norv Turner", "Eric Bieniemy", "Jim Caldwell", "Romeo Crennel",
                 "David Culley", "Dennis Green", "Dom Capers", "Kellen Moore",
                 "Doug Williams", "Mel Phillips"]
# Staff persons who played: player row (own posterior) next to the staff row
EXAMPLE_LINKED = ["Doug Williams", "Mel Phillips", "Kellen Moore", "Charlie Frye",
                  "T.J. Yates", "Thad Lewis", "Pat White", "Aaron Glenn", "DeMeco Ryans"]


# ============================================================================
# Helpers
# ============================================================================

def era(year):
    """Era label for a season (ERA_BREAKS); None if missing."""
    if pd.isna(year):
        return None
    return next(lab for upper, lab in ERA_BREAKS if year <= upper)


def table_names(con):
    return {t for (t,) in con.execute("SHOW TABLES").fetchall()}


def describe_frame(name, df, cols=None):
    """Print shape and missingness (share NA) of the listed columns."""
    cols = cols or list(df.columns)
    miss = df[cols].isna().mean()
    miss = miss[miss > 0]
    print(f"  {name}: {df.shape[0]:,} rows x {df.shape[1]} cols; missing: " +
          (", ".join(f"{c} {100 * v:.1f}%" for c, v in miss.items()) if len(miss) else "none"))


def read_links(path=LINKS_PATH):
    """Staff person -> gsis_id links verified through Wikidata: the staff
    record takes the player record's likelihood, and documentation is pooled
    over both uids (name_timing candidates are not used)."""
    try:
        links = pd.read_csv(path, dtype=str)
    except FileNotFoundError:
        print(f"  {path} not found: no staff-player links (run 09_race_coding_sheets.py)")
        return pd.DataFrame(columns=["person_id", "gsis_id"])
    links = links[links["link_method"] == "wikidata"][["person_id", "gsis_id"]]
    links = links.dropna().drop_duplicates()
    if links["gsis_id"].duplicated().any() or links["person_id"].duplicated().any():
        raise RuntimeError(f"{path}: a staff person or gsis_id appears in two Wikidata links")
    print(f"  staff-player links (Wikidata): {len(links):,}")
    return links


# ============================================================================
# Likelihood L_i(r) from names (and home county)
# ============================================================================

def player_counties(con):
    """One home county FIPS per NFL player: the linked recruit's county
    (player_xwalk.recruit_id, then player_recruit_xwalk), else the county of a
    linked CFBD roster row; only race_bifsg_geo rows whose county matched the
    Census table. At each level a player with two different counties gets
    none from that level."""
    geo = con.execute("""
        WITH g AS (SELECT entity, entity_id, county_fips FROM race_bifsg_geo
                   WHERE county_matched AND county_fips IS NOT NULL)
        SELECT x.gsis_id, g.county_fips, 1 AS level, 'recruit' AS county_source
        FROM player_xwalk x JOIN g ON g.entity = 'recruits'
             AND g.entity_id = CAST(x.recruit_id AS VARCHAR)
        UNION ALL
        SELECT r.gsis_id, g.county_fips, 2, 'recruit'
        FROM player_recruit_xwalk r JOIN g ON g.entity = 'recruits'
             AND g.entity_id = CAST(r.recruit_id AS VARCHAR)
        UNION ALL
        SELECT c.gsis_id, g.county_fips, 3, 'college_roster'
        FROM player_college_xwalk c JOIN g ON g.entity = 'college_players'
             AND g.entity_id = CAST(c.player_id AS VARCHAR)""").df().drop_duplicates()
    n_cty = geo.groupby(["gsis_id", "level"])["county_fips"].transform("nunique")
    geo = (geo[n_cty == 1].sort_values(["gsis_id", "level"])
              .drop_duplicates("gsis_id")[["gsis_id", "county_fips", "county_source"]])
    return geo.set_index("gsis_id")


def first_name_factor(b, ref):
    """P_H(r | f) / P_H(r) of each race_bifsg row as race_utils.predict_race
    applied it: the matched first name, else 'ALL OTHER FIRST NAMES'; 1
    without a first name."""
    first = ref["first"]
    key = b["first_name_used"]
    has_first = key.notna().to_numpy()
    rows = np.where(b["first_name_matched"].fillna(False).astype(bool).to_numpy(), key,
                    TZIOUMIS_OTHER)
    lik_f = np.ones((len(b), K))
    lik_f[has_first] = (first.loc[rows[has_first]].to_numpy()
                        / np.asarray(ref["first_marginal"], dtype=float))
    return lik_f


def load_likelihoods(con, links):
    """One row per person_uid ('player:<gsis_id>' / 'staff:<person_id>'):
    L_white ... L_multi (normalized; names x county), LN_white ... LN_multi
    (names only, for the leave-county-out variant), likelihood_source (bifsg /
    bifsg_geo / none), county_source, no_usable_name, bifsg_method,
    p_black_bifsg (the name-only race_bifsg posterior) and likelihood_uid (the
    record whose names and county give L).

    The first-name factor of FIRST_NAME_NEUTRAL categories is set to 1. A
    staff person who played (Wikidata link) takes the likelihood of his player
    record (legal and common first name, plus the home county) when that
    record has a usable name; the two name likelihoods are never multiplied,
    which would count the same name twice."""
    ref = load_reference()
    pc = np.asarray(ref["census_marginal"], dtype=float)
    print("  Census marginal P_C(r): " +
          ", ".join(f"{r} {100 * p:.2f}%" for r, p in zip(RACES, pc)))
    b = con.execute(f"""
        SELECT entity, entity_id, {', '.join(PROB_COLS)}, method, first_name_used,
               first_name_matched FROM race_bifsg
        WHERE entity IN ('nfl_players', 'staff_persons')""").df()
    if b.duplicated(["entity", "entity_id"]).any():
        raise RuntimeError("race_bifsg is not unique by (entity, entity_id)")
    is_player = (b["entity"] == "nfl_players").to_numpy()
    b["person_uid"] = np.where(is_player, "player:", "staff:") + b["entity_id"]

    post = b[PROB_COLS].to_numpy(dtype=float)
    usable = ~np.isnan(post).any(axis=1)
    lik = np.ones_like(post)
    lik[usable] = post[usable] / pc

    # Neutral first-name factor for the rare categories
    lik_f = first_name_factor(b, ref)
    neutral = [RACES.index(r) for r in FIRST_NAME_NEUTRAL]
    lik[np.ix_(usable, neutral)] /= lik_f[np.ix_(usable, neutral)]
    lik /= lik.sum(axis=1, keepdims=True)
    lik_names = lik.copy()

    # Home county likelihood for players with a usable name
    counties = player_counties(con)
    cty = load_county()
    fips = b["entity_id"].map(counties["county_fips"]).where(is_player)
    has_cty = (fips.notna() & fips.isin(cty["county"].index)).to_numpy() & usable
    lik[has_cty] *= (cty["county"].loc[fips[has_cty]].to_numpy()
                     / np.asarray(cty["county_marginal"], dtype=float))
    lik /= lik.sum(axis=1, keepdims=True)

    out = pd.DataFrame(lik, columns=LIK_COLS)
    out[LIKN_COLS] = lik_names
    out.insert(0, "person_uid", b["person_uid"].to_numpy())
    out["likelihood_source"] = np.select([has_cty, usable], ["bifsg_geo", "bifsg"], "none")
    out["county_source"] = b["entity_id"].map(counties["county_source"]).where(has_cty).to_numpy()
    out["no_usable_name"] = ~usable
    out["bifsg_method"] = b["method"].to_numpy()
    out["p_black_bifsg"] = b["p_black"].to_numpy()
    out["likelihood_uid"] = out["person_uid"]
    out = out.set_index("person_uid")

    # Staff persons who played: the player record's likelihood
    st_uid = "staff:" + links["person_id"]
    pl_uid = "player:" + links["gsis_id"]
    ok = (pl_uid.isin(out.index) & st_uid.isin(out.index)).to_numpy().copy()
    ok[ok] = ~out.loc[pl_uid[ok], "no_usable_name"].to_numpy(dtype=bool)
    num = LIK_COLS + LIKN_COLS
    out.loc[st_uid[ok], num] = out.loc[pl_uid[ok], num].to_numpy(dtype=float)
    for c in ("likelihood_source", "county_source", "no_usable_name"):
        out.loc[st_uid[ok], c] = out.loc[pl_uid[ok], c].to_numpy()
    out.loc[st_uid[ok], "likelihood_uid"] = pl_uid[ok].to_numpy()
    print(f"  staff persons who played: {int(ok.sum()):,} of {len(links):,} take the "
          "player record's likelihood")
    print("  likelihood source by entity:")
    print(pd.crosstab(b["entity"].to_numpy(),
                      out.loc[b["person_uid"], "likelihood_source"].to_numpy()).to_string())
    return out


# ============================================================================
# Documented race
# ============================================================================

def parse_tokens(*values):
    """Protocol tokens from free-text race fields: 'Black; White',
    'black and white', 'Pacific Islander' -> {'black', 'white'},
    {'pacific_islander'}. Unknown words are kept (and ignored downstream)."""
    out = set()
    for value in values:
        if value is None or (not isinstance(value, str) and pd.isna(value)):
            continue
        for piece in re.split(r"[;,/|+&]|\band\b", str(value).lower()):
            tok = re.sub(r"[\s\-]+", "_", piece.strip()).strip("_")
            if tok:
                out.add(TOKEN_SYNONYMS.get(tok, tok))
    return out


def evidence_row(person_uid, source, tokens):
    """One evidence record: races stated, multiracial statement, Hispanic."""
    return dict(person_uid=person_uid, source=source,
                races=frozenset(t for t in tokens if t in PROTOCOL_RACES),
                multi="multiracial" in tokens, hispanic="hispanic" in tokens)


def wikidata_evidence(path=WIKIDATA_PATH):
    """Evidence from Wikidata P172 statements (04d_race_documented.py output),
    or None if the file does not exist yet."""
    try:
        df = pd.read_parquet(path)
    except FileNotFoundError:
        print(f"  wikidata: {path} not found -- source skipped")
        return None
    except Exception as exc:                       # corrupt or partial file
        print(f"  wikidata: could not read {path} ({exc}) -- source skipped")
        return None
    cat = next((c for c in ("mapped_category", "category", "race_category")
                if c in df.columns), None)
    if "person_uid" not in df.columns or cat is None:
        print(f"  wikidata: {path} lacks person_uid / mapped_category -- source skipped")
        return None
    if "rank" in df.columns:
        df = df[df["rank"].fillna("").str.lower() != "deprecated"]
    n_unmapped = int(df[cat].isna().sum())
    df = df[df[cat].notna()]
    rows = [evidence_row(u, "wikidata", parse_tokens(c))
            for u, c in zip(df["person_uid"], df[cat])]
    print(f"  wikidata: {len(df):,} mapped statements for {df['person_uid'].nunique():,} "
          f"persons ({n_unmapped:,} unmapped statements ignored)")
    return pd.DataFrame(rows)


def category_evidence(con):
    """Evidence from the Wikipedia category flags (staff and player tables);
    tables that do not exist are skipped."""
    tables, rows = table_names(con), []
    for table, id_col, prefix in [("staff_person_wiki_signals", "person_id", "staff:"),
                                  ("player_wiki_signals", "gsis_id", "player:")]:
        if table not in tables:
            print(f"  category: table {table} not in DB -- skipped")
            continue
        df = con.execute(f'SELECT {id_col}, {", ".join(CATEGORY_FLAGS)} FROM "{table}"').df()
        flags = df[list(CATEGORY_FLAGS)].fillna(False).astype(bool)
        hit = flags.any(axis=1)
        for pid, (_, f) in zip(df.loc[hit, id_col], flags[hit].iterrows()):
            tokens = {CATEGORY_FLAGS[c] for c in CATEGORY_FLAGS if f[c]}
            rows.append(evidence_row(prefix + pid, "category", tokens))
        print(f"  category: {table}: {int(hit.sum()):,} of {len(df):,} articles flagged "
              f"({', '.join(f'{c} {int(flags[c].sum())}' for c in CATEGORY_FLAGS)})")
    return pd.DataFrame(rows) if rows else None


def text_evidence(links, path=TEXT_LABELS_PATH, min_confidence=TEXT_MIN_CONFIDENCE):
    """Evidence from classified article sentences (race_text_labels.csv, schema
    TEXT_COLS; extra columns such as 'basis' are ignored), or None if the file
    does not exist or lacks a column. Keeps has_statement TRUE rows with
    confidence >= min_confidence. A player uid linked to a staff person is
    dropped when the staff uid has its own label (the staff code wins)."""
    try:
        df = pd.read_csv(path, dtype=str, keep_default_na=False, encoding="utf-8-sig")
    except FileNotFoundError:
        print(f"  text: {path} not found -- source skipped")
        return None
    except Exception as exc:
        print(f"  text: could not read {path} ({exc}) -- source skipped")
        return None
    missing = [c for c in TEXT_COLS if c not in df.columns]
    if missing:
        print(f"  text: WARNING {path} lacks columns {missing} -- source skipped")
        return None
    n_in = len(df)
    df["race"] = df["race"].str.strip().str.lower()
    bad = ~df["race"].isin(TEXT_RACES)
    if bad.any():
        print(f"  text: WARNING {int(bad.sum())} rows with an unknown race value dropped: "
              f"{df.loc[bad, 'race'].value_counts().to_dict()}")
    conf = pd.to_numeric(df["confidence"], errors="coerce")
    keep = (df["has_statement"].str.strip().str.lower() == "true") & ~bad \
        & (conf >= min_confidence)
    df = df[keep]
    # Staff code wins over the linked player uid's code
    to_staff = dict(zip("player:" + links["gsis_id"], "staff:" + links["person_id"]))
    staff_uids = set(df.loc[df["person_uid"].str.startswith("staff:"), "person_uid"])
    dup = df["person_uid"].map(to_staff).isin(staff_uids)
    df = df[~dup]
    rows, n_bad_comp = [], 0
    for u, race, comp, hisp in zip(df["person_uid"], df["race"],
                                   df["multiracial_components"], df["hispanic"]):
        tokens = set()
        if race == "multiracial":
            parts = {t.strip() for t in comp.lower().split(";") if t.strip()}
            n_bad_comp += bool(parts - set(PROTOCOL_RACES))
            tokens = {"multiracial"} | (parts & set(PROTOCOL_RACES))
        elif race != "unknown":
            tokens = {race}
        if hisp.strip().lower() == "yes":
            tokens.add("hispanic")
        if tokens:
            rows.append(evidence_row(u, "text", tokens))
    print(f"  text: {n_in:,} rows; {int(keep.sum()):,} with has_statement and confidence "
          f">= {min_confidence}; {int(dup.sum()):,} linked player labels dropped (staff "
          f"label wins); {len(rows):,} usable" +
          (f"; {n_bad_comp} rows with unlisted multiracial components" if n_bad_comp else ""))
    return pd.DataFrame(rows) if rows else None


def combine_documented(evidence, links):
    """One row per modelled person (person_uid; a linked player's evidence is
    moved to his staff uid) with documented_race (Census category or None),
    documented_components, documented_black_any, documented_hispanic,
    documented_sources, documented_conflict, documented_races_stated and
    is_documented. Rules: module docstring."""
    cols = ["documented_race", "documented_components", "documented_black_any",
            "documented_hispanic", "documented_sources", "documented_conflict",
            "documented_races_stated", "is_documented"]
    if evidence is None or evidence.empty:
        return pd.DataFrame(columns=cols, index=pd.Index([], name="person_uid"))
    to_staff = dict(zip("player:" + links["gsis_id"], "staff:" + links["person_id"]))
    ev = evidence.assign(person_uid=evidence["person_uid"].map(to_staff)
                         .fillna(evidence["person_uid"]))
    # Per person x source: union of races, any multiracial / Hispanic statement
    per_src = (ev.groupby(["person_uid", "source"])
                 .agg(races=("races", lambda s: frozenset().union(*s)),
                      multi=("multi", "any"), hispanic=("hispanic", "any"))
                 .reset_index())
    records = {}
    for uid, g in per_src.groupby("person_uid", sort=False):
        races = frozenset().union(*g["races"])
        within_multi = bool(g["multi"].any() or (g["races"].map(len) >= 2).any())
        hisp = bool(g["hispanic"].any())
        conflict = len(races) >= 2 and not within_multi
        sources = ";".join(s for s in SOURCE_ORDER if s in set(g["source"]))
        stated = ";".join(sorted(races)) or None
        if not (races or within_multi or hisp):
            continue
        if conflict:
            records[uid] = dict(documented_race=None, documented_components=None,
                                documented_black_any=pd.NA, documented_hispanic=pd.NA,
                                documented_sources=sources, documented_conflict=True,
                                documented_races_stated=stated, is_documented=False)
            continue
        multi = within_multi or len(races) >= 2
        if hisp:
            race = "hispanic"
        elif multi:
            race = "multi"
        else:
            race = PROTOCOL_RACES[next(iter(races))]
        records[uid] = dict(documented_race=race,
                            documented_components=stated if multi else None,
                            documented_black_any=int("black" in races),
                            documented_hispanic=int(hisp), documented_sources=sources,
                            documented_conflict=False, documented_races_stated=stated,
                            is_documented=True)
    out = pd.DataFrame.from_dict(records, orient="index")[cols]
    out.index.name = "person_uid"
    for c in ("documented_black_any", "documented_hispanic"):
        out[c] = out[c].astype("Int64")
    out["documented_conflict"] = out["documented_conflict"].astype(bool)
    out["is_documented"] = out["is_documented"].astype(bool)
    return out


def load_documented(con, links):
    """Read the three documented-race sources (each optional) and combine them.
    Returns (documented frame, {source: present?})."""
    parts = {"wikidata": wikidata_evidence(), "category": category_evidence(con),
             "text": text_evidence(links)}
    present = {k: v is not None for k, v in parts.items()}
    frames = [v for v in parts.values() if v is not None and not v.empty]
    evidence = pd.concat(frames, ignore_index=True) if frames else None
    doc = combine_documented(evidence, links)
    print(f"  documented persons: {int(doc['is_documented'].sum()):,}; "
          f"conflicts (fall back to the model): {int(doc['documented_conflict'].sum()):,}")
    if len(doc):
        print("  documented race x Black-any (after pooling linked uids):")
        print(pd.crosstab(doc["documented_race"].fillna("(conflict)"),
                          doc["documented_black_any"].astype(str)).to_string())
    return doc, present


# ============================================================================
# Covariates
# ============================================================================

def college_type(con):
    """gsis_id -> college_type (power / other_fbs / fcs_or_lower / hbcu /
    unknown). Linked CFBD roster: last college team and season. Otherwise the
    first ';'-separated nfl_players.college_name component that school_xwalk
    maps to a CFBD school, at the CFBD season nearest to rookie season - 1."""
    teams = con.execute("""SELECT team, season, conference, classification
                           FROM college_teams""").df()
    ever = teams.groupby("team")["conference"].apply(lambda s: bool(set(s) & HBCU_CONFS))
    hbcu = (set(ever[ever].index) | HBCU_EXTRA) - NOT_HBCU

    linked = con.execute("""
        SELECT gsis_id, last_college_team AS team, last_college_season AS season
        FROM player_college_xwalk WHERE last_college_team IS NOT NULL
        ORDER BY gsis_id, placeholder_id, last_college_season DESC, team""").df()
    linked = linked.drop_duplicates("gsis_id").assign(how="cfbd_roster")

    players = con.execute("SELECT gsis_id, college_name, rookie_season FROM nfl_players").df()
    xw = con.execute("""SELECT nflverse_college, cfbd_school FROM school_xwalk
                        WHERE cfbd_school IS NOT NULL""").df()
    xw = dict(zip(xw["nflverse_college"], xw["cfbd_school"]))
    known = set(teams["team"])
    def first_school(name):
        if not isinstance(name, str):
            return None
        for part in [name.strip()] + [p.strip() for p in name.split(";")]:
            school = xw.get(part)
            if school in known:
                return school
        return None
    rest = players[~players["gsis_id"].isin(linked["gsis_id"])].copy()
    rest["team"] = rest["college_name"].map(first_school)
    rest["season"] = (rest["rookie_season"] - 1).clip(FIRST_CFBD_SEASON, LAST_CFBD_SEASON)
    rest = rest.dropna(subset=["team"]).assign(how="school_xwalk")
    both = pd.concat([linked, rest[["gsis_id", "team", "season", "how"]]], ignore_index=True)

    # Nearest CFBD season of the school (exact for linked players)
    m = both.merge(teams, on="team", how="left", suffixes=("", "_ct"))
    m["gap"] = (m["season_ct"] - m["season"]).abs()
    m = m.sort_values(["gsis_id", "gap", "season_ct"]).drop_duplicates("gsis_id")
    power = (m["conference"].isin(POWER_CONFS)
             | ((m["conference"] == "Big East") & (m["season_ct"] <= BIG_EAST_LAST_FBS))
             | (m["team"] == "Notre Dame"))
    fbs = m["classification"] == "fbs"
    m["college_type"] = np.select(
        [m["team"].isin(hbcu), fbs & power, fbs,
         m["classification"].isin(["fcs", "ii", "iii"])],
        ["hbcu", "power", "other_fbs", "fcs_or_lower"], "unknown")
    out = players[["gsis_id", "college_name"]].merge(m[["gsis_id", "college_type", "how"]],
                                                     on="gsis_id", how="left")
    out["college_type"] = out["college_type"].fillna("unknown")
    # HBCU from the nflverse college name, whatever CFBD covers
    first_name = out["college_name"].str.split(";").str[0].str.strip()
    to_hbcu = first_name.isin(HBCU_NFLVERSE) & (out["college_type"] != "hbcu")
    print(f"  college type: {int(to_hbcu.sum()):,} players set to hbcu from the nflverse "
          "college name (was: " +
          str(out.loc[to_hbcu, "college_type"].value_counts().to_dict()) + ")")
    out.loc[to_hbcu, "college_type"] = "hbcu"
    out.loc[to_hbcu, "how"] = "hbcu_list"
    print("  college type by source: " +
          str(out.groupby(out["how"].fillna("none"))["college_type"]
              .value_counts().unstack(fill_value=0).to_dict("index")))
    return out.set_index("gsis_id")["college_type"]


def entry_position(con):
    """gsis_id -> fine position at NFL entry (_POSITIONS). Sources, in order:
    the draft position (nfl_draft_picks), the modal position in the player's
    first season on a season roster (from 1999), the current nfl_players
    position. The unit (BROAD_POSITION) is that of the first source that
    maps; a generic level (OL, DL, LB, DB) is refined by a later source only
    within the same unit (S, CB within DB), never across units."""
    df = con.execute("""
        WITH dr AS (SELECT gsis_id, any_value(position) AS draft_pos FROM nfl_draft_picks
                    WHERE gsis_id IS NOT NULL GROUP BY 1),
        rs AS (SELECT gsis_id, season, position, count(*) AS n FROM nfl_rosters_season
               WHERE gsis_id IS NOT NULL AND position IS NOT NULL GROUP BY ALL),
        first AS (SELECT gsis_id, position AS roster_pos FROM rs
                  QUALIFY row_number() OVER (PARTITION BY gsis_id
                                             ORDER BY season, n DESC, position) = 1)
        SELECT p.gsis_id, dr.draft_pos, first.roster_pos, p.position AS current_pos,
               p.position_group
        FROM nfl_players p LEFT JOIN dr USING (gsis_id) LEFT JOIN first USING (gsis_id)
        """).df()
    srcs = [df[c].map(FINE_POSITION) for c in ("draft_pos", "roster_pos", "current_pos")]
    srcs.append(df["position_group"].map(GROUP_TO_FINE))
    fine = pd.Series(None, index=df.index, dtype=object)
    unit = pd.Series(None, index=df.index, dtype=object)
    for s in srcs:                       # unit of the first source that maps
        take = unit.isna() & s.notna()
        fine[take], unit[take] = s[take], s[take].map(BROAD_POSITION)
    for s in srcs[1:]:                   # refine generic levels within the unit
        take = (fine.isin(GENERIC_POSITIONS) & s.notna() & ~s.isin(GENERIC_POSITIONS)
                & (s.map(BROAD_POSITION) == unit))
        fine[take] = s[take]
    if fine.isna().any():
        raise RuntimeError(f"entry_position: {int(fine.isna().sum())} players without a "
                           "position")
    changed = df["position_group"].map(GROUP_TO_FINE).map(BROAD_POSITION) != unit
    print(f"  position at entry: unit differs from the current position group for "
          f"{int(changed.sum()):,} players; levels: {fine.value_counts().to_dict()}")
    return pd.Series(fine.to_numpy(), index=df["gsis_id"])


def player_covariates(con, links):
    """One row per nfl_players gsis_id: display_name, pos_group (fine position
    at entry), rookie_era, draft_bucket, has_wiki, career_bucket, college_type,
    n_gameday_seasons. county_available is added from the likelihood in
    prepare_entity()."""
    df = con.execute(f"""
        WITH gd AS (
            SELECT gsis_id, season FROM nfl_rosters_weekly
            WHERE season_type = 'REG' AND is_key_primary AND status IN ('ACT', 'INA')
              AND gsis_id IS NOT NULL AND gsis_id <> ''
            UNION
            SELECT gsis_id, season FROM nfl_rosters_season
            WHERE season < 2002 AND status IN ('ACT', 'INA') AND gsis_id IS NOT NULL),
        car AS (SELECT gsis_id, count(DISTINCT season) AS n_seasons FROM gd GROUP BY 1),
        scope AS (   -- players 09_race_coding_sheets.py looked up on Wikidata
            SELECT gsis_id FROM nfl_contract_history
            WHERE (contract_type IN {VETERAN_TYPES} AND year_signed BETWEEN 2011 AND 2025)
               OR (contract_type IN {ROOKIE_TYPES} AND year_signed BETWEEN 2011 AND 2026)
            UNION SELECT gsis_id FROM nfl_rosters_season WHERE season >= 2002)
        SELECT p.gsis_id, p.display_name, p.position_group, p.rookie_season,
               p.draft_round, p.pfr_id, coalesce(car.n_seasons, 0) AS n_gameday_seasons,
               coalesce(p.gsis_id IN (SELECT gsis_id FROM scope
                                      WHERE gsis_id IS NOT NULL), false) AS in_scope,
               coalesce(p.gsis_id IN (SELECT gsis_id FROM player_wiki_signals
                                      WHERE gsis_id IS NOT NULL), false) AS has_article
        FROM nfl_players p LEFT JOIN car USING (gsis_id)""").df()
    describe_frame("nfl_players", df)
    try:                     # every link (also name_timing) widened the lookup scope
        all_links = pd.read_csv(LINKS_PATH, dtype=str)
    except FileNotFoundError:
        all_links = links
    checked = ((df["in_scope"].astype(bool) | df["gsis_id"].isin(all_links["gsis_id"]))
               & df["pfr_id"].notna()).to_numpy()
    out = pd.DataFrame({"gsis_id": df["gsis_id"], "display_name": df["display_name"]})
    out["pos_group"] = df["gsis_id"].map(entry_position(con)).to_numpy()
    out["rookie_era"] = df["rookie_season"].map(era)
    rnd = df["draft_round"].astype(float)
    out["draft_bucket"] = np.select([rnd.isna(), rnd <= 2, rnd <= 4],
                                    ["undrafted", "R1-2", "R3-4"], "R5+")
    out["has_wiki"] = np.select([df["has_article"].astype(bool).to_numpy(), checked],
                                ["yes", "no"], "not_checked")
    n = df["n_gameday_seasons"].astype(int)
    out["career_bucket"] = np.select([n == 0, n == 1, n <= 3, n <= 6],
                                     ["0", "1", "2-3", "4-6"], "7+")
    out["n_gameday_seasons"] = n.astype(int)
    out = out.set_index("gsis_id")
    out["college_type"] = college_type(con).reindex(out.index).fillna("unknown")
    return out


def staff_covariates(con, links):
    """One row per staff_persons person_id: display_name; role_group_first and
    unit_first (most senior role, and the unit of the most senior coaching
    role, in the first season observed; primary); role_group_senior and
    unit_modal (most senior role ever held; modal coaching unit; preddoc);
    first_era (first season observed); former_player (Wikidata link to an NFL
    player: every 'yes' has an article, so it is a fame proxy and enters the
    preddoc prior only); has_wiki (staff_persons.wiki_title). Unit is 'none'
    without a unit-specific coaching role."""
    persons = con.execute("""SELECT person_id, person_name, first_season, wiki_title
                             FROM staff_persons""").df()
    ts = con.execute("""SELECT person_id, season, role_group, unit FROM staff_team_season
                        WHERE person_id IS NOT NULL""").df()
    describe_frame("staff_persons", persons)
    unmapped = set(ts["role_group"]) - set(ROLE_SENIORITY)
    if unmapped:
        raise RuntimeError(f"Unmapped staff role groups: {unmapped}")
    ts["rank"] = ts["role_group"].map(lambda g: ROLE_SENIORITY[g][0])
    group7 = lambda s: s.map(lambda g: ROLE_SENIORITY[g][1])
    ts["first"] = ts.groupby("person_id")["season"].transform("min")
    first = ts[ts["season"] == ts["first"]]
    unit_ok = ts["role_group"].isin(COACH_GROUPS) & ts["unit"].isin(_UNITS[1:])

    def top(rows):
        return rows.sort_values(["person_id", "rank", "unit"]).drop_duplicates("person_id") \
                   .set_index("person_id")
    coach = ts[unit_ok]
    unit_modal = (coach.groupby(["person_id", "unit"])["season"].nunique().rename("n")
                       .reset_index().sort_values(["person_id", "n", "unit"],
                                                  ascending=[True, False, True])
                       .drop_duplicates("person_id").set_index("person_id")["unit"])

    out = pd.DataFrame({"person_id": persons["person_id"],
                        "display_name": persons["person_name"]}).set_index("person_id")
    out["role_group_first"] = group7(top(first)["role_group"]).reindex(out.index)
    out["unit_first"] = top(first[unit_ok.loc[first.index]])["unit"].reindex(out.index) \
        .fillna("none")
    out["role_group_senior"] = group7(top(ts)["role_group"]).reindex(out.index)
    out["unit_modal"] = unit_modal.reindex(out.index).fillna("none")
    first_season = ts.groupby("person_id")["season"].min().reindex(out.index)
    n_diff = int((first_season != persons.set_index("person_id")["first_season"]
                  .reindex(out.index)).sum())
    if n_diff:
        print(f"  staff: {n_diff} persons whose staff_persons.first_season differs from "
              "their first staff_team_season season (the latter is used)")
    out["first_era"] = first_season.map(era)
    out["former_player"] = np.where(out.index.isin(links["person_id"]), "yes", "no")
    out["has_wiki"] = np.where(persons.set_index("person_id")["wiki_title"]
                               .reindex(out.index).notna(), "yes", "no")
    print("  staff role group, first season observed vs most senior ever:")
    print(pd.crosstab(out["role_group_first"], out["role_group_senior"]).to_string())
    return out


# ============================================================================
# Prior by EM
# ============================================================================

def design_matrix(cov, levels):
    """Dummy-coded X (first level = reference) and the (covariate, level) of
    each column. Fails on a level not listed in `levels`."""
    cols, names = [], []
    for var, lv in levels.items():
        vals = cov[var].astype(object)
        bad = set(vals.dropna()) - set(lv)
        if bad or vals.isna().any():
            raise ValueError(f"{var}: unlisted or missing levels {bad or 'NA'}")
        for level in lv[1:]:
            cols.append((vals == level).to_numpy(dtype=float))
            names.append((var, level))
    return np.column_stack(cols), names


def fit_prior_em(X, L, init_prior, label, C=C_PRIOR, tol=EM_TOL, max_iter=EM_MAX_ITER):
    """EM for pi(r | X) = softmax(X gamma) given known likelihoods L (n x K).

    Persons with identical X rows are collapsed into cells: the M-step's
    weighted log-likelihood sum_i sum_r w_ir log pi(r | X_i) equals
    sum_cells sum_r (sum_{i in cell} w_ir) log pi(r | X_cell). Returns the
    fitted LogisticRegression, the log-likelihood path and a convergence flag.
    """
    cells, cell_of = np.unique(X, axis=0, return_inverse=True)
    cell_of = cell_of.ravel()
    Xs = np.repeat(cells, K, axis=0)            # cell 0 x races 0..K-1, cell 1 ...
    ys = np.tile(np.arange(K), len(cells))
    # M-step tolerance (gradient of the weight-averaged loss) well below the
    # EM stopping rule; warm starts make later M-steps cheap
    model = LogisticRegression(C=C, solver="lbfgs", tol=1e-8, max_iter=10000,
                               warm_start=True)
    w = init_prior * L
    w /= w.sum(axis=1, keepdims=True)
    path, prev, converged, t0 = [], None, False, time.time()
    for it in range(1, max_iter + 1):
        cw = np.zeros((len(cells), K))
        np.add.at(cw, cell_of, w)
        model.fit(Xs, ys, sample_weight=cw.ravel())
        pi = model.predict_proba(cells)[cell_of]
        num = pi * L
        tot = num.sum(axis=1)
        ll = float(np.log(tot).sum())
        pen = ll - float((model.coef_ ** 2).sum()) / (2 * C)
        w = num / tot[:, None]
        rel = abs(ll - prev) / abs(prev) if prev is not None else np.nan
        path.append(dict(iteration=it, loglik=ll, loglik_penalized=pen, rel_change=rel))
        if prev is not None and rel < tol:
            converged = True
            break
        prev = ll
    pens = np.array([p["loglik_penalized"] for p in path])
    n_drop = int((np.diff(pens) < -1e-6 * np.abs(pens[1:])).sum())
    print(f"  {label}: EM {'converged' if converged else 'stopped (max_iter)'} after "
          f"{len(path)} iterations in {time.time() - t0:.1f}s; n = {len(L):,} persons in "
          f"{len(cells):,} covariate cells; loglik {path[0]['loglik']:.2f} -> "
          f"{path[-1]['loglik']:.2f}" + (f"; WARNING penalized loglik fell {n_drop}x"
                                        if n_drop else ""))
    return model, pd.DataFrame(path), converged



def gamma_table(model, names, levels, entity, variant):
    """Long table of gamma: variant, entity, race, covariate, level,
    coefficient, is_reference (reference levels carry 0)."""
    rows = []
    for k, race in enumerate(RACES):
        rows.append(dict(covariate="(intercept)", level=None, race=race,
                         coefficient=float(model.intercept_[k]), is_reference=False))
        rows += [dict(covariate=var, level=lv[0], race=race, coefficient=0.0,
                      is_reference=True) for var, lv in levels.items()]
        rows += [dict(covariate=var, level=level, race=race,
                      coefficient=float(model.coef_[k, j]), is_reference=False)
                 for j, (var, level) in enumerate(names)]
    return pd.DataFrame(rows).assign(variant=variant, entity=entity)[
        ["variant", "entity", "race", "covariate", "level", "coefficient", "is_reference"]]


def prepare_entity(entity, cov, lik, doc):
    """Covariates joined with the likelihood and the documented columns; a
    person missing from race_bifsg gets L = 1 (no usable name)."""
    df = cov.copy()
    df["entity"] = entity
    df["entity_id"] = df.index
    df["person_uid"] = f"{entity}:" + df.index.to_series()
    df = df.join(lik, on="person_uid").join(doc, on="person_uid")
    miss = df["likelihood_source"].isna()
    if miss.any():
        print(f"  {entity}: {int(miss.sum()):,} persons missing from race_bifsg (L = 1)")
        df.loc[miss, LIK_COLS + LIKN_COLS] = 1.0 / K
        df.loc[miss, "likelihood_source"] = "none"
        df.loc[miss, "no_usable_name"] = True
        df.loc[miss, "likelihood_uid"] = df.loc[miss, "person_uid"]
    df["no_usable_name"] = df["no_usable_name"].astype(bool)
    df["is_documented"] = df["is_documented"].fillna(False).astype(bool)
    df["documented_conflict"] = df["documented_conflict"].fillna(False).astype(bool)
    if entity == "player":
        df["county_available"] = np.where(df["likelihood_source"] == "bifsg_geo", "yes", "no")
    return df.reset_index(drop=True)


def fit_variant(df, levels, fit, entity, variant, lik_cols=LIK_COLS, C=C_PRIOR, init=None):
    """EM prior on the rows in `fit`; prior and posterior for every row. The
    EM starts from the Census marginal, or from `init` (n x K prior)."""
    X, names = design_matrix(df, levels)
    L = df[lik_cols].to_numpy(dtype=float)
    pc = np.asarray(load_reference()["census_marginal"], dtype=float)
    start = pc if init is None else init[fit]
    model, path, converged = fit_prior_em(X[fit], L[fit], start, f"{entity} [{variant}]", C=C)
    prior = model.predict_proba(X)
    post = prior * L
    post /= post.sum(axis=1, keepdims=True)
    path = path.assign(entity=entity, variant=variant, converged=converged)
    return prior, post, gamma_table(model, names, levels, entity, variant), path


def model_entity(entity, cov, lik, doc, links):
    """All variants for one entity ('player' or 'staff'). Returns (person
    frame, gamma tables, EM paths, check rows)."""
    df = prepare_entity(entity, cov, lik, doc)
    usable = ~df["no_usable_name"].to_numpy()
    is_doc = df["is_documented"].to_numpy()
    if entity == "player":
        df["linked_uid"] = df["entity_id"].map(links.set_index("gsis_id")["person_id"])
        df["linked_uid"] = "staff:" + df["linked_uid"]
    else:
        df["linked_uid"] = "player:" + df["entity_id"].map(links.set_index("person_id")["gsis_id"])
    labels = np.array(RACES, dtype=object)
    gammas, paths, checks = [], [], []

    # Primary: model-only, full population, predetermined covariates
    fit = usable
    prior, post, g1, p1 = fit_variant(df, PRIMARY_LEVELS[entity], fit, entity, "pred")
    df[pcols("pred", "prior")] = prior
    df[pcols("pred")] = post
    df["p_black_any_pred"] = post[:, BLACK]
    df["p_black_or_multi_pred"] = post[:, BLACK] + post[:, RACES.index("multi")]
    df["race_pred_argmax"] = labels[post.argmax(axis=1)]
    df["pred_method"] = np.where(usable, "model", "prior_only")
    df["in_fit_pred"] = fit
    gammas.append(g1)
    paths.append(p1)

    # Primary prior under a weak penalty: how much the L2 penalty matters
    _, post_c, _, p_c = fit_variant(df, PRIMARY_LEVELS[entity], fit, entity,
                                    f"pred, C = {C_SENSITIVITY:g}", C=C_SENSITIVITY,
                                    init=prior)
    diff = np.abs(post_c[:, BLACK] - post[:, BLACK])
    sec = f"sensitivity of P(Black) to the prior's L2 penalty (C = {C_PRIOR:g} vs {C_SENSITIVITY:g})"
    for metric, val in [("mean |change|", diff.mean()), ("max |change|", diff.max()),
                        ("share |change| > 0.05", (diff > 0.05).mean())]:
        checks.append(dict(table="check", variant="pred", section=sec, covariate=metric,
                           entity=entity, value=float(val), n=len(diff)))
    paths.append(p_c)

    # Player side variants of the primary prior (no draft bucket; no county)
    if entity == "player":
        for v, levels in SIDE_LEVELS.items():
            lik_cols = LIKN_COLS if v == "nocounty" else LIK_COLS
            prior_v, post_v, g_v, p_v = fit_variant(df, levels, fit, entity, f"pred_{v}",
                                                    lik_cols=lik_cols)
            df[f"p_white_pred_{v}"] = post_v[:, RACES.index("white")]
            df[f"p_black_pred_{v}"] = post_v[:, BLACK]
            df[f"prior_black_pred_{v}"] = prior_v[:, BLACK]
            gammas.append(g_v)
            paths.append(p_v)

    # Sensitivity: documented one-hot, else prior fitted on undocumented persons
    fit = usable & ~is_doc & ~df["documented_conflict"].to_numpy()
    prior, post, g2, p2 = fit_variant(df, PREDDOC_LEVELS[entity], fit, entity, "preddoc")
    gammas.append(g2)
    paths.append(p2)
    df[pcols("preddoc", "prior")] = prior
    df[pcols("preddoc", "model")] = post
    pred = post.copy()
    if is_doc.any():
        idx = df.loc[is_doc, "documented_race"].map({r: k for k, r in enumerate(RACES)})
        pred[is_doc] = np.eye(K)[idx.to_numpy(dtype=int)]
    df[pcols("preddoc")] = pred
    doc_any = df["documented_black_any"].astype("Float64").fillna(0).to_numpy(dtype=float)
    df["p_black_any_preddoc"] = np.where(is_doc, doc_any, pred[:, BLACK])
    df["race_preddoc_argmax"] = labels[pred.argmax(axis=1)]
    df["pred_method_preddoc"] = np.select([is_doc, ~usable], ["documented", "prior_only"],
                                          "model")
    df["in_fit_preddoc"] = fit
    return df, pd.concat(gammas), pd.concat(paths), checks


def documented_with_links(doc, links):
    """The documented frame with each linked player uid carrying the pooled
    evidence of his staff uid (documentation is a property of the person;
    each record keeps its own model posterior)."""
    if doc.empty or links.empty:
        return doc
    st_uid = "staff:" + links["person_id"]
    have = st_uid.isin(doc.index).to_numpy()
    extra = doc.loc[st_uid[have]].copy()
    extra.index = pd.Index("player:" + links.loc[have, "gsis_id"].to_numpy(), name="person_uid")
    return pd.concat([doc[~doc.index.isin(extra.index)], extra])


def linked_comparison(players, staff, links):
    """Staff persons who played: P(Black) of the player record (own player
    prior) vs the staff record (staff prior; player-record likelihood). Check
    rows plus a printed table of EXAMPLE_LINKED."""
    pl = players.set_index("entity_id")
    st = staff.set_index("entity_id")
    m = links[links["gsis_id"].isin(pl.index) & links["person_id"].isin(st.index)]
    d = pd.DataFrame({"name": pl.loc[m["gsis_id"], "display_name"].to_numpy(),
                      "documented_black_any": pl.loc[m["gsis_id"], "documented_black_any"]
                      .astype("Float64").to_numpy(),
                      "p_player": pl.loc[m["gsis_id"], "p_black_pred"].to_numpy(),
                      "p_staff": st.loc[m["person_id"], "p_black_pred"].to_numpy(),
                      "prior_player": pl.loc[m["gsis_id"], "prior_black_pred"].to_numpy(),
                      "prior_staff": st.loc[m["person_id"], "prior_black_pred"].to_numpy()})
    diff = (d["p_player"] - d["p_staff"]).abs()
    sec = "staff persons who played: player-record vs staff-record P(Black)"
    rows = [dict(table="check", variant="pred", section=sec, covariate=metric,
                 entity="player", value=float(val), n=len(d))
            for metric, val in [("mean |player - staff|", diff.mean()),
                                ("share |player - staff| > 0.2", (diff > 0.2).mean())]]
    blk = d["documented_black_any"] == 1
    for col, lab in [("p_player", "player record"), ("p_staff", "staff record")]:
        rows.append(dict(table="check", variant="pred", section=sec,
                         covariate=f"mean, documented Black: {lab}", entity="player",
                         value=float(d.loc[blk, col].mean()), n=int(blk.sum())))
    print("\n  Staff persons who played (player record = player prior; staff record = staff "
          "prior x player-record likelihood):")
    print(d[d["name"].isin(EXAMPLE_LINKED)].drop_duplicates("name").round(3)
          .to_string(index=False))
    return rows


# ============================================================================
# Diagnostics and quality checks
# ============================================================================

def level_means(df, entity, variant):
    """Per level of the variant's prior covariates: n, n in the fit, n
    documented, mean prior (fit persons), mean posterior (all persons), mean
    model-only P(Black) of documented persons, mean BIFSG P(Black)."""
    rows = []
    fit, doc = df[f"in_fit_{variant}"], df["is_documented"]
    for var in LEVELS[variant][entity]:
        for level, g in df.groupby(var, sort=False):
            gf, gd = g[fit.loc[g.index]], g[doc.loc[g.index]]
            row = dict(variant=variant, entity=entity, table="covariate_level",
                       covariate=var, level=level, n=len(g), n_fit=len(gf),
                       n_documented=len(gd),
                       mean_p_black_model_documented=gd[model_black(variant)].mean(),
                       mean_p_black_bifsg=g["p_black_bifsg"].mean())
            row.update({f"mean_prior_{r}_fit": gf[c].mean()
                        for r, c in zip(RACES, pcols(variant, "prior"))})
            row.update({f"mean_p_{r}": g[c].mean() for r, c in zip(RACES, pcols(variant))})
            rows.append(row)
    return pd.DataFrame(rows)


def activity(con):
    """Game-day player weeks and staff seasons, ACTIVE_SEASONS only."""
    lo, hi = ACTIVE_SEASONS
    players = con.execute(f"""
        SELECT gsis_id, season, count(DISTINCT week) AS n_weeks FROM nfl_rosters_weekly
        WHERE season_type = 'REG' AND is_key_primary AND status IN ('ACT', 'INA')
          AND gsis_id IS NOT NULL AND gsis_id <> '' AND season BETWEEN {lo} AND {hi}
        GROUP BY ALL""").df()
    staff = con.execute(f"""
        SELECT DISTINCT person_id, franchise_id, season, role_std, role_group, interim_any
        FROM staff_team_season WHERE person_id IS NOT NULL AND season BETWEEN {lo} AND {hi}
        """).df()
    return players, staff


def auc(y, score):
    y, score = np.asarray(y, dtype=float), np.asarray(score, dtype=float)
    ok = ~np.isnan(y) & ~np.isnan(score)
    return roc_auc_score(y[ok], score[ok]) if len(set(y[ok])) == 2 else np.nan


def r2_decomposition(d, y, levels, key):
    """R^2 of y on the prior covariates: OLS on the main-effect dummies, on the
    covariate-cell means (between-cell share), and on `key` alone."""
    y = d[y].to_numpy(dtype=float)
    sst = ((y - y.mean()) ** 2).sum()
    X, _ = design_matrix(d, levels)
    X = np.column_stack([np.ones(len(y)), X])
    beta = np.linalg.lstsq(X, y, rcond=None)[0]
    r2_main = 1 - ((y - X @ beta) ** 2).sum() / sst
    def r2_means(cols):
        m = pd.Series(y, index=d.index).groupby([d[c] for c in cols]).transform("mean")
        return 1 - ((y - m.to_numpy()) ** 2).sum() / sst
    return r2_main, r2_means(list(levels)), r2_means([key])


def variant_metrics(out, active, staff_seasons, v):
    """Quality checks for one variant, as rows (table = 'check')."""
    rows = []
    def add(section, metric, entity, value, n):
        rows.append(dict(table="check", variant=v, section=section, covariate=metric,
                         entity=entity, value=float(value), n=int(n)))
    pb, pa, pr = f"p_black_{v}", f"p_black_any_{v}", f"prior_black_{v}"
    pl = out[out["entity"] == "player"].set_index("entity_id")
    st = out[out["entity"] == "staff"].set_index("entity_id")
    lo, hi = ACTIVE_SEASONS
    sec = f"players on game-day rosters {lo}-{hi}"
    a = pl.loc[np.intersect1d(active["gsis_id"].unique(), pl.index)]
    w = active.join(pl[[pb]], on="gsis_id").dropna(subset=[pb])
    add(sec, "mean P(Black), persons", "player", a[pb].mean(), len(a))
    add(sec, "mean P(Black any), persons", "player", a[pa].mean(), len(a))
    add(sec, "mean P(Black), player-weeks", "player", np.average(w[pb], weights=w["n_weeks"]), len(w))
    add(sec, "mean prior P(Black), persons", "player", a[pr].mean(), len(a))
    add(sec, "share P(Black) > 0.5", "player", (a[pb] > 0.5).mean(), len(a))
    for label, mask in [("head coaches", staff_seasons["role_group"] == "head_coach"),
                        ("on-field coaches", staff_seasons["role_group"].isin(COACH_GROUPS))]:
        ss = staff_seasons[mask]
        s = st.reindex(ss["person_id"].unique())
        ps = ss[["person_id", "season"]].drop_duplicates().join(st[pb], on="person_id")
        add(f"coaches {lo}-{hi}", f"{label}: mean P(Black), persons", "staff", s[pb].mean(), len(s))
        add(f"coaches {lo}-{hi}", f"{label}: mean P(Black), person-seasons", "staff",
            ps[pb].mean(), len(ps))
        add(f"coaches {lo}-{hi}", f"{label}: mean prior P(Black)", "staff", s[pr].mean(), len(s))

    own = out[out["entity"] == "player"]
    for scope, d in [("all players", own),
                     ("undocumented players", own[own["documented_race"].isna()])]:
        sec = f"distribution: {scope}"
        for q, val in d[pb].quantile([0.05, 0.25, 0.50, 0.75, 0.95]).items():
            add(sec, f"P(Black) q{int(100 * q):02d}", "player", val, len(d))
        p = d[pcols(v)].to_numpy(dtype=float)
        add(sec, f"mean entropy (nats; max {np.log(K):.2f})", "player",
            (-(np.where(p > 0, p * np.log(np.clip(p, 1e-300, None)), 0)).sum(axis=1)).mean(), len(d))
        add(sec, "share |P(Black) - prior| > 0.1", "player", ((d[pb] - d[pr]).abs() > 0.1).mean(), len(d))
        add(sec, "share P(Black) > 0.5", "player", (d[pb] > 0.5).mean(), len(d))

    score = model_black(v)
    doc = out[out["documented_race"].notna()]
    # 'all' counts a staff person who played once (his staff record)
    once = doc[~((doc["entity"] == "player") & doc["linked_uid"].notna())]
    for ent, d in [("player", doc[doc["entity"] == "player"]),
                   ("staff", doc[doc["entity"] == "staff"]), ("all", once)]:
        sec = "documented persons: model-only P(Black)"
        black = d["documented_black_any"] == 1
        white = d["documented_race"] == "white"
        for grp, m in [("Black", black), ("non-Black", ~black), ("white", white)]:
            add(sec, f"mean, documented {grp}", ent, d.loc[m, score].mean(), int(m.sum()))
        bw = d[black | white]
        for name, col in [("model", score), ("names only (BIFSG)", "p_black_bifsg"),
                          ("prior only", pr)]:
            add(sec, f"AUC Black vs non-Black, {name}", ent, auc(black, d[col]), len(d))
            add(sec, f"AUC Black vs white, {name}", ent,
                auc(bw["documented_black_any"] == 1, bw[col]), len(bw))

    # Calibration implies E[p | D = 1] = E[p^2] / E[p] over the population;
    # documented Black persons are fame-selected, so the comparison is indicative
    sec = "calibration check: E[p^2]/E[p] (implied mean P(Black) of Black persons)"
    for ent, d, dd in [("player", own, doc[doc["entity"] == "player"]),
                       ("staff", out[out["entity"] == "staff"], doc[doc["entity"] == "staff"])]:
        p = d[score].to_numpy(dtype=float)
        add(sec, "implied E[p | Black] = E[p^2]/E[p]", ent, (p ** 2).mean() / p.mean(), len(p))
        blk = dd["documented_black_any"] == 1
        add(sec, "observed mean p, documented Black", ent, dd.loc[blk, score].mean(),
            int(blk.sum()))

    # Role holders 2010-2025: documentation coverage and accuracy
    sec = f"role holders {lo}-{hi}: model-only P(Black) by documented race"
    for label, groups in [("head coaches", ["head_coach"]), ("coordinators", ["coordinator"]),
                          ("general managers", ["general_manager"])]:
        ss = staff_seasons[staff_seasons["role_group"].isin(groups)]
        s = st.reindex(ss["person_id"].unique())
        add(sec, f"{label}: share with documented race", "staff",
            s["documented_race"].notna().mean(), len(s))
        dd = s[s["documented_race"].notna()]
        blk = dd["documented_black_any"] == 1
        add(sec, f"{label}: mean p, documented Black", "staff", dd.loc[blk, score].mean(),
            int(blk.sum()))
        add(sec, f"{label}: mean p, undocumented", "staff",
            s.loc[s["documented_race"].isna(), score].mean(),
            int(s["documented_race"].isna().sum()))
        for name, col in [("model", score), ("names only (BIFSG)", "p_black_bifsg")]:
            add(sec, f"{label}: AUC documented Black vs rest (positive-only), {name}", "staff",
                auc(s["documented_black_any"].astype("Float64").fillna(0) == 1, s[col]), len(s))
        ps = ss[["person_id", "season"]].drop_duplicates().join(st[score], on="person_id")[score]
        rel = ps.var() / (ps.var() + (ps * (1 - ps)).mean())
        add(sec, f"{label}: reliability Var(p)/(Var(p) + E[p(1-p)]), person-seasons",
            "staff", rel, len(ps))

    staff_rows = out[out["entity"] == "staff"]
    for ent, d in [("player", own), ("staff", staff_rows)]:
        lv = LEVELS[v][ent]
        key = next(iter(lv))
        r2m, r2c, r2k = r2_decomposition(d.reset_index(drop=True), score, lv, key)
        sec = "variance of model-only P(Black) explained by the prior covariates"
        add(sec, "R2 on covariate dummies (main effects)", ent, r2m, len(d))
        add(sec, "R2 on covariate-cell means", ent, r2c, len(d))
        add(sec, f"R2 on {key} alone", ent, r2k, len(d))
        add(sec, "within-cell share (names, county)", ent, 1 - r2c, len(d))
    return rows


def print_side_by_side(rows):
    """Print check rows with the primary and the sensitivity variant side by
    side, in the order the checks were computed."""
    df = pd.DataFrame(rows)
    df = df[df["variant"].isin(VARIANTS)]
    cell = {(r.section, r.covariate, r.entity, r.variant): (r.value, r.n)
            for r in df.itertuples()}
    current = None
    for sec, metric, ent in dict.fromkeys(zip(df["section"], df["covariate"], df["entity"])):
        if sec != current:
            current = sec
            print(f"\n  {sec}:")
            print(f"    {'':<60s} {'entity':>7s} {'pred':>8s} {'preddoc':>8s}   n (pred / preddoc)")
        vals = [cell.get((sec, metric, ent, v), (np.nan, np.nan)) for v in VARIANTS]
        ns = " / ".join("-" if pd.isna(n) else f"{int(n):,}" for _, n in vals)
        print(f"    {metric:<60s} {ent:>7s} {vals[0][0]:8.3f} {vals[1][0]:8.3f}   {ns}")


def named_examples(out):
    """Named players and coaches: primary posterior next to the sensitivity
    variant's model-only and final posteriors (descriptive sanity check)."""
    cols = ["display_name", "documented_race", "prior_black_pred", "p_black_pred",
            "prior_black_preddoc", "p_black_preddoc_model", "p_black_preddoc", "p_black_bifsg"]
    print("\n  Named examples (pred = primary, model-only; preddoc_model = as if "
          "undocumented under the undocumented prior):")
    for names, ent in [(EXAMPLE_PLAYERS, "player"), (EXAMPLE_STAFF, "staff")]:
        ex = out[(out["entity"] == ent) & out["display_name"].isin(names)]
        if ent == "player":          # namesakes: keep the longest career
            ex = ex.sort_values("n_gameday_seasons", ascending=False).drop_duplicates("display_name")
        with pd.option_context("display.width", 220, "display.max_columns", 20):
            print(ex[cols].round(3).to_string(index=False))


# ============================================================================
# TIDES comparison and raked sensitivity column (players, primary variant)
# ============================================================================

def load_tides():
    """TIDES shares (0-1) indexed by (group, season), one column per category:
    the median of the contemporaneous primary percent values; retrospective
    restatements only for a season without a contemporaneous value. None if
    the reference file does not exist."""
    path = next((p for p in TIDES_PATHS if p.exists()), None)
    if path is None:
        print("  TIDES: tides_nfl_race_shares.csv not found -- comparison and raking skipped")
        return None
    try:
        t = pd.read_csv(path)
    except Exception as exc:
        print(f"  TIDES: could not read {path} ({exc}) -- skipped")
        return None
    t = t[(t["unit"] == "percent") & (t["source_type"] == "primary")].copy()
    t["retro"] = t["notes"].fillna("").str.contains("Retrospective")
    t = t[t["retro"] == t.groupby(["group", "season", "category"])["retro"].transform("min")]
    print(f"  TIDES: {path} ({len(t):,} values used)")
    return (t.groupby(["group", "season", "category"])["value"].median() / 100).unstack("category")


TIDES_ASSISTANT_GROUPS = ("coordinator", "position_coach", "assistant_coach",
                          "strength_conditioning")
TIDES_COORDINATOR_ROLES = ("OC", "DC", "STC")


def staff_group_seasons(staff_seasons):
    """Person-seasons by TIDES staff group: head_coaches (non-interim head
    coaches: 32 per season, the start-of-season count TIDES reports),
    coordinators (OC, DC, STC) and assistant_coaches (coordinators, position,
    assistant and strength coaches below the head coach; support_staff
    'OTHER_COACH' rows excluded)."""
    ss = staff_seasons
    hc_all = ss[ss["role_group"] == "head_coach"][["person_id", "season"]].drop_duplicates()
    hc = ss[(ss["role_group"] == "head_coach") & ~ss["interim_any"].fillna(False).astype(bool)]
    coord = ss[ss["role_std"].isin(TIDES_COORDINATOR_ROLES)]
    asst = ss[ss["role_group"].isin(TIDES_ASSISTANT_GROUPS)][["person_id", "season"]]
    asst = asst.drop_duplicates().merge(hc_all, how="left", indicator=True)
    asst = asst[asst["_merge"] == "left_only"].drop(columns="_merge")
    return {g: d[["person_id", "season"]].drop_duplicates()
            for g, d in [("head_coaches", hc), ("coordinators", coord),
                         ("assistant_coaches", asst)]}


def season_means(out, active, staff_seasons, cols):
    """Model means by season: players weighted by game-day weeks ('players',
    the average weekly roster) and unweighted over distinct players
    ('players_headcount'); staff groups (staff_group_seasons) by
    person-season. Before 2016 the weekly rosters list about 1,690 distinct
    players a season, in 2016 about 2,650, and 2,100-2,400 from 2017."""
    pl = out[out["entity"] == "player"].set_index("entity_id")
    st = out[out["entity"] == "staff"].set_index("entity_id")
    w = active.join(pl[cols], on="gsis_id").dropna(subset=cols)
    frames = [w.groupby("season").apply(
        lambda g: pd.Series({**{c: np.average(g[c], weights=g["n_weeks"]) for c in cols},
                             "n": len(g)}), include_groups=False).assign(group="players"),
              w.groupby("season").agg(**{c: (c, "mean") for c in cols},
                                      n=("gsis_id", "size")).assign(group="players_headcount")]
    for group, ps in staff_group_seasons(staff_seasons).items():
        ps = ps.join(st[cols], on="person_id")
        frames.append(ps.groupby("season").agg(**{c: (c, "mean") for c in cols},
                                               n=("person_id", "size")).assign(group=group))
    return pd.concat(frames).reset_index().set_index(["group", "season"])


def tides_report(out, active, staff_seasons, tides):
    """Print model vs TIDES by season (primary, raked primary, preddoc) and
    summary gaps; return diagnostics rows (tables 'tides_season', 'check')."""
    cols = ["p_black_any_pred", "p_black_pred_raked", "p_black_any_preddoc"]
    sm = season_means(out, active, staff_seasons, cols)
    tid = tides.reindex(columns=["black", "two_or_more_races", "not_disclosed"])
    tid = pd.concat([tid, tid.loc[["players"]].rename(index={"players": "players_headcount"})])
    comp = sm.join(tid.rename(columns=lambda c: f"tides_{c}"), how="left")
    comp["tides_black_plus_2plus"] = comp["tides_black"] + comp["tides_two_or_more_races"]
    # Upper bound among those who disclosed: (Black + 2+) / (1 - not disclosed)
    comp["tides_upper_disclosed"] = (comp["tides_black_plus_2plus"]
                                     / (1 - comp["tides_not_disclosed"].fillna(0)))
    print("\n  Model P(Black) vs TIDES by season. TIDES 'black' = African-American "
          "(one category per player) through 2016;\n  self-identified Black alone from "
          "2019 (2018 for assistant coaches), when 2019+ is bracketed by black and black + "
          f"two or more races.\n  Roster regime: {ROSTER_REGIME_BREAK} lists ~2,650 distinct "
          "players (about 1,690 before, 2,100-2,400 after):")
    with pd.option_context("display.width", 220, "display.max_rows", 200):
        print(comp.round(3).to_string())
    rows = [dict(table="tides_season", variant="both", entity=g, covariate="season",
                 level=str(s), n=int(r["n"]), value=r["p_black_any_pred"],
                 value_raked=r["p_black_pred_raked"],
                 value_preddoc=r["p_black_any_preddoc"], tides_black=r["tides_black"],
                 tides_two_or_more=r["tides_two_or_more_races"])
            for (g, s), r in comp.iterrows()]
    sec = "TIDES (like-for-like caveats: see docstring)"
    def add(v, metric, entity, value, n):
        rows.append(dict(table="check", variant=v, section=sec, covariate=metric,
                         entity=entity, value=float(value), n=int(n)))
    for v, col, tag in [("pred", "p_black_any_pred", ""),
                        ("pred", "p_black_pred_raked", " [raked]"),
                        ("preddoc", "p_black_any_preddoc", "")]:
        for grp in ("players", "players_headcount"):
            p = comp.loc[grp]
            lab = "players" if grp == "players" else "players (headcount)"
            for nm, sel in [("2010-2015", (p.index >= 2010) & (p.index <= 2015)),
                            ("2016", p.index == 2016)]:
                pre = p[sel & p["tides_black"].notna()]
                add(v, f"{lab} {nm}: mean model - TIDES African-American{tag}", "player",
                    (pre[col] - pre["tides_black"]).mean(), len(pre))
            post = p[(p.index >= 2019) & p["tides_black_plus_2plus"].notna()]
            inside = (post[col] >= post["tides_black"]) & (post[col] <= post["tides_black_plus_2plus"])
            add(v, f"{lab} 2019+: share of seasons inside [Black, Black + 2+]{tag}", "player",
                inside.mean(), len(post))
            add(v, f"{lab} 2019+: mean model - TIDES Black + two or more{tag}", "player",
                (post[col] - post["tides_black_plus_2plus"]).mean(), len(post))
            add(v, f"{lab} 2019+: mean model - TIDES (Black + 2+)/(1 - not disclosed){tag}",
                "player", (post[col] - post["tides_upper_disclosed"]).mean(), len(post))
        for g in ("head_coaches", "coordinators", "assistant_coaches"):
            q = comp.loc[g]
            for nm, sel in [("2010-2016", q.index <= 2016), ("2017+", q.index >= 2017)]:
                qq = q[sel & q["tides_black"].notna()]
                if len(qq):
                    add(v, f"{g.replace('_', ' ')} {nm}: mean model - TIDES Black{tag}",
                        "staff", (qq[col] - qq["tides_black"]).mean(), len(qq))
    print_side_by_side([r for r in rows if r["table"] == "check"])
    return rows


def rake_entity(rows_df, target, season_mean_of):
    """Raked primary posterior of one entity: the prior's Black odds (against
    every other category) times exp(delta), delta set by bisection so that
    season_mean_of(p_black + p_multi) equals `target`. The raking windows
    (RAKE_SEASONS, STAFF_RAKE_SEASONS) end before 2017, when TIDES coded one
    race per person and folded multiracial persons into a single category
    (mostly 'African-American'), so the matching model quantity is
    P(Black alone) + P(multiracial), not P(Black alone) (2026-10-03 fix; the
    earlier target over-raked by matching Black alone to that series).
    Returns (raked posterior array, delta, unraked value of the matched
    quantity)."""
    prior = rows_df[pcols("pred", "prior")].to_numpy(dtype=float)
    L = rows_df[LIK_COLS].to_numpy(dtype=float)
    def posterior(delta):
        pr = prior.copy()
        pr[:, BLACK] *= np.exp(delta)
        post = pr * L
        return post / post.sum(axis=1, keepdims=True)
    multi = RACES.index("multi")
    matched = lambda post: post[:, BLACK] + post[:, multi]
    lo, hi = -6.0, 6.0
    for _ in range(60):
        mid = (lo + hi) / 2
        lo, hi = (mid, hi) if season_mean_of(matched(posterior(mid))) < target else (lo, mid)
    delta = (lo + hi) / 2
    return posterior(delta), delta, season_mean_of(matched(posterior(0.0)))


def rake(out, active, staff_seasons, tides):
    """p_black_pred_raked and p_white_pred_raked (SENSITIVITY, so that TIDES
    stays an independent check), one delta per entity:
      players  mean over RAKE_SEASONS of the week-weighted season mean equals
               the mean TIDES African-American share (2016 left out: roster
               regime); validated on 2019-2023 against [Black, Black + 2+]
      staff    mean over STAFF_RAKE_SEASONS of the assistant-coach season mean
               (staff_group_seasons) equals the mean TIDES assistant-coach
               Black share; applied to every staff person; validated on
               2017-2023 assistants and on head coaches
    Returns (frame indexed by person_uid, check rows)."""
    res, checks = [], []
    for ent, group, seasons in [("player", "players", RAKE_SEASONS),
                                ("staff", "assistant_coaches", STAFF_RAKE_SEASONS)]:
        d = out[out["entity"] == ent].reset_index(drop=True)
        tgt = tides.loc[group, "black"].reindex(range(seasons[0], seasons[1] + 1)).dropna()
        idx = pd.Series(d.index, index=d["entity_id"])
        if ent == "player":
            a = active[active["season"].isin(tgt.index) & active["gsis_id"].isin(idx.index)]
            pos, wts, seas = idx[a["gsis_id"]].to_numpy(), a["n_weeks"].to_numpy(), a["season"]
        else:
            a = staff_group_seasons(staff_seasons)[group]
            a = a[a["season"].isin(tgt.index) & a["person_id"].isin(idx.index)]
            pos, wts, seas = idx[a["person_id"]].to_numpy(), np.ones(len(a)), a["season"]
        def season_mean_of(pb):
            s = pd.DataFrame({"p": pb[pos] * wts, "w": wts,
                              "season": seas.to_numpy()}).groupby("season").sum()
            return float((s["p"] / s["w"]).mean())
        post, delta, base = rake_entity(d, float(tgt.mean()), season_mean_of)
        res.append(pd.DataFrame({"p_white_pred_raked": post[:, RACES.index("white")],
                                 "p_black_pred_raked": post[:, BLACK]},
                                index=d["person_uid"].to_numpy()))
        print(f"\n  raked {ent}s (sensitivity; matched P(Black) + P(multi)): target = mean TIDES {group} Black share "
              f"{seasons[0]}-{seasons[1]} ({len(tgt)} seasons) = {tgt.mean():.3f}; unraked "
              f"{base:.3f}; prior Black odds x {np.exp(delta):.2f} (delta {delta:+.3f})")
        checks += [dict(table="check", variant="pred", section="TIDES raking",
                        covariate=k, entity=ent, value=float(val), n=len(tgt))
                   for k, val in [("target", tgt.mean()), ("unraked mean", base),
                                  ("delta", delta)]]
    return pd.concat(res), checks


# ============================================================================
# Main
# ============================================================================

DOC_COLS = ["documented_race", "documented_components", "documented_black_any",
            "documented_hispanic", "documented_sources", "documented_conflict",
            "documented_races_stated"]
SIDE_COLS = [f"{k}_{v}" for v in SIDE_LEVELS
             for k in ("p_white_pred", "p_black_pred", "prior_black_pred")]
OUT_COLS = (["entity", "entity_id", "person_uid", "display_name",
             *pcols("pred"), "p_black_any_pred", "p_black_or_multi_pred", "race_pred_argmax",
             "pred_method", *pcols("pred", "prior"), "p_white_pred_raked",
             "p_black_pred_raked", *SIDE_COLS, "in_fit_pred",
             *DOC_COLS,
             *pcols("preddoc"), "p_black_any_preddoc", "race_preddoc_argmax",
             "pred_method_preddoc", *pcols("preddoc", "model"), *pcols("preddoc", "prior"),
             "in_fit_preddoc",
             "likelihood_source", "likelihood_uid", "county_source", "no_usable_name",
             "bifsg_method", "p_black_bifsg", *LIK_COLS, "linked_uid"]
            + COVARIATE_COLS + ["n_gameday_seasons"])


def write_parquet(df, path):
    try:
        df.to_parquet(path, index=False)
    except OSError as exc:
        raise RuntimeError(f"Could not write {path}: {exc}") from exc
    print(f"  -> {path} ({len(df):,} rows x {df.shape[1]} cols)")


def print_priors_by_level(levels):
    """Mean estimated prior P(Black) over each variant's fit set, by level."""
    print("\n  Estimated prior P(Black) by covariate level (mean over the variant's fit set):")
    for (v, ent), g in levels.groupby(["variant", "entity"], sort=False):
        print(f"    [{v}] {ent}")
        for var, h in g.groupby("covariate", sort=False):
            print(f"      {var:<18s}" + "  ".join(f"{l} {p:.2f}" for l, p in
                                               zip(h["level"], h["mean_prior_black_fit"])))


def main():
    ap = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    ap.add_argument("--no-db", action="store_true",
                    help="do not write the DuckDB tables (parquet files only)")
    ap.add_argument("--out-dir", default=str(OUT_DIR),
                    help="folder for the parquet files (default data/raw/derived_race)")
    args = ap.parse_args()
    out_dir = Path(args.out_dir)
    out_dir.mkdir(parents=True, exist_ok=True)
    t0 = time.time()

    print(f"Reading {DB_PATH} (read-only)")
    con = duckdb.connect(str(DB_PATH), read_only=True)
    links = read_links()
    print("Likelihoods:")
    lik = load_likelihoods(con, links)
    print("Documented race:")
    doc, present = load_documented(con, links)
    doc = documented_with_links(doc, links)
    print("Covariates:")
    pcov = player_covariates(con, links)
    scov = staff_covariates(con, links)
    active, staff_seasons = activity(con)
    con.close()

    print("Priors (EM; pred = primary, preddoc = sensitivity):")
    players, g_p, path_p, chk_p = model_entity("player", pcov, lik, doc, links)
    staff, g_s, path_s, chk_s = model_entity("staff", scov, lik, doc, links)
    out = pd.concat([staff, players], ignore_index=True)
    for c in OUT_COLS:
        if c not in out.columns:
            out[c] = np.nan
    out = out[OUT_COLS].copy()
    if out["person_uid"].duplicated().any():
        raise RuntimeError("race_predicted: person_uid is not unique")
    if len(out) != len(pcov) + len(scov):
        raise RuntimeError(f"race_predicted has {len(out):,} rows, expected "
                           f"{len(pcov) + len(scov):,}")
    for c in ("documented_black_any", "documented_hispanic"):
        out[c] = out[c].astype("Int64")
    for c in ("documented_conflict", "no_usable_name", "in_fit_pred", "in_fit_preddoc"):
        out[c] = out[c].fillna(False).astype(bool)
    for col in ("pred_method", "pred_method_preddoc"):
        print(f"  {col} by entity:")
        print(pd.crosstab(out["entity"], out[col]).to_string())

    print("\nQuality checks (pred = PRIMARY, model-only; preddoc = sensitivity):")
    checks = [r for v in VARIANTS for r in variant_metrics(out, active, staff_seasons, v)]
    checks += chk_p + chk_s + linked_comparison(players, staff, links)
    print_side_by_side(checks)
    named_examples(out)
    tides = load_tides()
    if tides is not None:
        raked, rake_checks = rake(out, active, staff_seasons, tides)
        for c in raked.columns:
            out[c] = out["person_uid"].map(raked[c])
        checks += rake_checks
        checks += tides_report(out, active, staff_seasons, tides)
    levels = pd.concat([level_means(df, ent, v) for ent, df in (("player", players),
                                                                 ("staff", staff))
                        for v in VARIANTS], ignore_index=True)
    print_priors_by_level(levels)

    priors = pd.concat([g_p, g_s], ignore_index=True)
    em_path = pd.concat([path_p, path_s], ignore_index=True).assign(table="em_path")
    sources = pd.DataFrame([dict(table="input", variant="both", entity="all",
                                 covariate=f"source present: {k}", value=float(val), n=0)
                            for k, val in present.items()])
    diagnostics = pd.concat([em_path, levels, pd.DataFrame(checks), sources],
                            ignore_index=True)

    print("\nWriting:")
    describe_frame("race_predicted", out, [c for c in OUT_COLS if c not in COVARIATE_COLS])
    write_parquet(out, out_dir / "race_predicted.parquet")
    write_parquet(priors, out_dir / "race_predicted_priors.parquet")
    write_parquet(diagnostics, out_dir / "race_predicted_diagnostics.parquet")
    if args.no_db:
        print("  --no-db: DuckDB tables race_predicted / race_predicted_priors not written")
    else:
        con = connect()
        note = ("primary = pred (model-only); sensitivity = preddoc; documented sources "
                "present: " + ", ".join(k for k, val in present.items() if val))
        write_table(con, "race_predicted", out,
                    source="BIFSG likelihood x EM-estimated NFL prior; documented race "
                           "(scripts/04e_predict_race.py)", note=note)
        write_table(con, "race_predicted_priors", priors,
                    source="multinomial-logit prior coefficients by EM "
                           "(scripts/04e_predict_race.py)", note=note)
        con.close()
    print("Documented sources present: " +
          ", ".join(f"{k}={'yes' if val else 'no'}" for k, val in present.items()))
    print(f"Done in {time.time() - t0:.1f}s")


if __name__ == "__main__":
    main()
