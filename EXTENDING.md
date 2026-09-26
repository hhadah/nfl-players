# Extending the data

This file records why the project exists, the research questions the data can
support, and what the data still lack.

## Premise of the study

I started this project to answer two questions about the NFL.

1. **Does racial diversity in the organization improve team performance?**
   "Organization" means the full staff: head coach, coordinators, position and
   assistant coaches, strength and conditioning, GM, personnel and scouting
   staff, and owners and executives. The head coach alone is not enough.
2. **Is there racial discrimination in the contracts and pay players
   receive?** If there is a gap, is it taste-based (Becker 1957) or
   statistical, meaning employers use race as a proxy for productivity they
   cannot yet observe (Altonji and Pierret 2001, *QJE*)?

The league is a good laboratory for both (Kahn 2000, *Journal of Economic
Perspectives*):

- Pay is public at the contract level.
- Productivity is measured play by play: EPA, success rate, snaps.
- A betting market prices every game, which benchmarks expected team quality.
- The players are majority Black, while the people who hire, coach and pay
  them are majority white (see Lapchick's annual *TIDES Racial and Gender
  Report Card: NFL* for exact shares).
- The Rooney Rule targets minority hiring directly. It covered head-coach
  searches from the 2003 cycle, was extended to GM and senior
  football-operations searches in 2009, and was amended in 2020 and 2022.

To separate statistical from taste-based discrimination, I condition on every
signal the market could see at each margin:

- **High school:** 247 composite recruiting ratings.
- **College:** CFBD box scores and PPA.
- **Draft:** combine results, the CFBD pre-draft grade and draft position.
- **NFL:** snaps, box scores, PFR advanced defense, cap history.

Two features of the market shape the designs:

- Rookie pay is slotted by pick, so for rookies the wage margin is the draft
  itself.
- Pay is freely bargained only at the veteran margin: second contracts,
  extensions, tags and free agency.

**Status (2026-09-26):** hand coding of race has not started. Every `*Hand`
variable and `race`, `black_any` and `nonwhite` are 0% non-missing in every
sample, so every question below is a design until the coding in
`notes/race-coding-protocol.md` is done.

- `black_provisional` is a Wikipedia-category flag. It is positive-only (1 or
  NA), so it is a lower bound.
- In the team files, `*BlackProv = 0` means "not flagged", not "non-Black".
- BIFSG (`p_black_bifsg`) misclassifies most Black people in this population.

The samples are in `data/datasets/analysis/`:

| Sample | Rows | Coverage |
|---|---|---|
| `team_season` | 861 | 32 franchises, 1999-2025 |
| `team_game` | 14,552 | |
| `staff_person_season` | 25,767 | 3,596 people |
| `player_season` | 58,991 | 14,608 players |
| `contracts` | 52,944 | |
| `draft_prospects` | 9,975 | |

## Research questions

Each question lists the design and its identifying assumption, the main
threat, and the variables that support it.

### A. Coaching staff and front office

**A1. Does hiring a Black head coach change team performance?**
- *Design:* stacked event study of between-season hires, comparing Black and
  non-Black hires on `WinPct`, `PointDiffPerGame`, `OffEPAPerPlay`/`DefEPAPerPlay`
  and `WinsOverExpected`, with interim spells treated separately. Identifying
  assumption: conditional on pre-hire performance and expectations
  (`LagWinPct`, `LagExpectedWins`), the race of the hire is unrelated to the
  counterfactual trajectory.
- *Threat:* selection on the hiring situation. Teams that change coaches are
  negatively selected: mean `LagWinPct` is 0.352 against 0.538 for teams that
  keep their coach, a visible Ashenfelter dip. `ExpectedWins` for the hire
  season is post-treatment, and `SeasonOpenerImpliedWinProb` is set after the
  hiring cycle, so a true preseason win total is still missing.
- *Data:* 170 between-season changes (`HCChange`), 54 team-seasons with an
  in-season change, 162 distinct season head coaches, and 226 franchise x HC
  runs (31 left-censored in 1999). The spell and hire file with interim flags
  and firing reasons is not built yet.

**A2. Does diversity beyond the head coach (coordinators, position coaches,
assistants, front office) affect performance?**
- *Design:* team outcomes on the Black share (`ShareBlackHand*`) or Blau index
  (`BlauHand*`) of each staff group, with team x head-coach-spell FE, so that
  identification comes from assistant turnover within a head coach's tenure.
  Identifying assumption: within-spell composition changes are not timed to
  shocks in team quality.
- *Threat:* coordinators are fired after bad seasons and replaced in bulk.
  Mitigations: control for lagged outcomes and for
  `ShareCoachesNewToFranchise`/`ShareCoachesPromoted`; placebo on leads of
  composition changes; use `FullStaffObserved` (608 of 861 team-seasons,
  2007+) or era FE, because boxes before 2007 are partial.
- *Data:* `OCChange` (mean 0.408), `DCChange` (0.379), `GMChange`, `CodedShare*`.
  On the provisional flag, the within-franchise SD of the coach Black share is
  0.094 against a total SD of 0.100. That may partly reflect Wikipedia
  coverage, so recompute it on hand codes.

**A3. Within a team-season, does the race of the OC versus the DC predict unit
performance?**
- *Design:* stack offense and defense units with team x season FE, which
  absorbs the head coach, GM, budget and schedule. Identifying assumption:
  nothing unit-specific is correlated with the coordinator's race.
- *Threat:* unit-specific talent allocation. Control for unit roster quality
  from `player_season`. On the provisional flags, Black coordinators are
  concentrated on defense (mean `DCBlackProv` 0.168 against `OCBlackProv`
  0.058), so power is asymmetric.
- *Data:* `Off/DefEPAPerPlay`, `Off/DefSuccessRate` and pass/rush splits;
  `OCBlackHand`/`DCBlackHand`; `ShareBlackHandOffenseCoaches`/`...DefenseCoaches`.
  `team_game` assigns the staff snapshot in force on game day.

**A4. Are minority coaches hired into worse situations, fired sooner, and
rehired less ("glass cliff"; "last hired, first fired")?**
- *Design:* (a) compare the inherited performance and expectations of teams
  hiring Black versus non-Black HCs, OCs, DCs and GMs; (b) a discrete-time
  hazard of separation on race x `WinsOverExpected` and tenure; (c) a model of
  rehiring as head coach conditional on `PriorNFLHCSeasons`. Identifying
  assumption for (b): conditional on performance over expectation, race is
  not correlated with the quality owners see and I do not.
- *Threat:* minority coaches who clear a higher hiring bar may be better, so
  treat the separation gap as a bound. Tenure is left-censored in 1999.
- *Data:* 170 between-season and 54 in-season HC changes, 305 OC, 295 DC and
  97 GM changes; `HCTenure`, `GMTenure`, `SeasonsWithFranchise`.
- *Literature:* Madden (2004, *Journal of Sports Economics*) on head-coach
  performance by race, 1990-2002; Ryan and Haslam (2005, *British Journal of
  Management*) define the glass cliff (on gender).

**A5. Do promotion pipelines and hiring networks differ by race?**
- *Design:* person-season transition models of promotion from position coach
  to coordinator to head coach at any franchise, with season FE, conditioning
  on role, unit, `SeasonsOnStaffAnyTeam` and unit performance. Add
  network position from a directed hiring graph: coaches new to a franchise
  (`NewToFranchise`) link to the current head coach and to their previous head
  coach (`PrevFranchise`).
- *Threat:* sorting into units (offense feeds head-coach jobs differently), so
  report results with and without unit controls; network ties are endogenous
  to ability.
- *Data:* 351 position coach -> coordinator transitions (6,893 at-risk
  person-seasons), 124 coordinator -> head coach (2,419 at risk), 277
  within-franchise promotions to coordinator, 6,914 `NewToFranchise`
  person-seasons.

**A6. Did the Rooney Rule and its amendments change the race of hires?**
- *Design:* interrupted time series over hiring cycles (2003 head coaches;
  2009 GM and front office, `RooneyFrontOffice2009` from season 2010;
  `RooneyAmend2020`; `RooneyAmend2022`), using roles the rule did not yet
  cover as comparisons (coordinators before 2020, position coaches).
- *Threat:* every club is treated at once, so this is a before-after
  comparison, not a difference-in-differences across teams. With 3-10 HC
  hires per cycle (mean 6.5), use randomization or permutation inference.
- *Data:* head-coach hires by `RooneyEra`: 19 before the rule, 115 under the
  2003 rule (2003-2020), 7 in 2021 and 29 from 2022.
- *Literature:* Solow, Solow and Walker (2011, *Labour Economics*); Madden and
  Ruther (2011, *Journal of Sports Economics*).

**A7. Does front-office diversity shape roster construction and the pay of
Black players?**
- *Design:* link GM and personnel-staff race to draft capital by position,
  veteran APY and the conditional race gap in pay, using within-franchise GM
  changes. Separate GM-only changes from joint GM + HC changes.
- *Data:* `GMPersonId` (755 team-seasons), `GMChange` (97), `GMTenure`,
  `ShareBlackHandFrontOffice`, `ShareBlackHandPersonnel`, linked to
  `contracts`/`player_season` via `PayFranchise` or `PrimaryFranchise`. Pick
  one: they differ in 4.2% of paid player-seasons.

### B. Player pay and statistical discrimination

**B1. Conditional on pre-draft signals, are Black prospects drafted later or
less often?** (the rookie wage margin)
- *Design:* `LogPick` (and `Drafted` for the combine-invitee pool) on race,
  combine results, `AgeAtDraft`, `RecruitRating`, `FinalCollegePower`,
  `FinalCollegeSRS`, the `CollFinal*` production vector and class x position
  FE. Identifying assumption: selection on observables.
- *Threat:* omitted signals (interviews, medicals). `PreDraftGrade` is made by
  scouts and may embed the bias itself, so report with and without it; it
  exists for drafted players only.
- *Data:* `draft_prospects` 2011-22: 4,522 prospects (1,457 undrafted
  invitees). Of the 3,065 drafted, 64.8% have `Forty`, `PreDraftGrade` and
  `RecruitRating` jointly.

**B2. Is the draft efficient by race and by pipeline (FCS, HBCU)?**
- *Design:* regress realized output on `LogPick` x race (and x origin) with
  class x position FE, following Massey and Thaler (2013, *Management
  Science*). If race or origin predicts output conditional on the pick, the
  market misprices it.
- *Threat:* infra-marginality; output depends on coach-allocated playing time;
  later classes are right-censored. The prospect universe misses undrafted
  players without a combine invitation.
- *Data:* drafted 2011-22 (N = 3,065): `PfrWeightedAV`,
  `NFLGamesStartedFirst3`, `VeteranContract` (use it rather than
  `SecondContract`, which counts practice-squad and SFA deals).
  `FinalCollegeHBCU` flags only 22 prospects, and FCS box scores are complete
  only from 2022, so an HBCU school list and pre-2022 FCS statistics are
  needed first.

**B3. Among veterans, is there a race gap in pay conditional on prior NFL
productivity?**
- *Design:* in `contracts` with `VeteranMarket == 1`, regress `LogAPYCapPct`,
  `GuaranteeShare` and `years` on race + `Prior*` + `Career*` + `ExperienceBin`
  + `AgeAtSigning`, with year x position and `ContractType` FE. `NewTeam` and
  `UFAResign` separate the open market from re-signings.
- *Threat:* no quality measure for OL and much of the defense (PFR advanced
  stats only from 2018; no PFF). Snaps and starts are chosen by coaches, so
  conditioning on them absorbs usage discrimination (a bad control).
  `GuaranteedZero` mixes zero with unknown guarantees.
- *Data:* 7,161 veteran-market contracts (UFA 4,578; tags and tenders 1,609;
  re-sign/extension 974). Of the 6,512 signed 2014+, 99.8% have prior-season
  snaps.

**B4. Does the conditional race gap change as NFL output is revealed
(employer learning)?**
- *Design:* in `player_season`, interact race and pre-NFL signals
  (`DraftPick`, `PreDraftGrade`, `RecruitRating`, combine) with `Experience` in
  regressions of log `GoverningAPY` or `CapPercent`, with and without realized
  output (Farber and Gibbons 1996, *QJE*; Altonji and Pierret 2001, *QJE*).
  Under statistical discrimination with learning, the gap converges to the
  gap implied by realized output. A gap that persists conditional on output
  points toward taste.
- *Threat:* rookie pay is slotted, so learning shows up only after the rookie
  deal (experience 3-5+). Survivors are selected (B6). NFL teams observe
  pre-market signals richly.
- *Data:* 35,765 paid player-seasons for 8,138 players; for 2013+ at
  experience 0-12, 30,205 rows, of which 63.9% have `DraftPick` and 73.3%
  `RecruitRating`.

**B5. Does the gap operate through escaping the minimum-salary mass?**
- *Design:* race gap in `NearMinimum` (LPM) and quantile or distribution
  regressions of `LogAPYCapPct` (Chernozhukov, Fernández-Val and Melly 2013,
  *Econometrica*).
- *Threat:* `RefMinAPY` is data-driven (the modal APY by year x experience
  bin), not the CBA schedule. OTC coverage of low-end deals jumps in 2017, so
  restrict to 2017+.
- *Data:* 43,390 `SampleMain` contracts; 58.9% are near the minimum (93.9% of
  ERFA and 10.7% of UFA deals).

**B6. Conditional on signals and early output, are Black players less likely to
earn a veteran contract or to stay on a roster?**
- *Design:* LPM of `VeteranContract` on race, `LogPick`, pre-draft signals and
  `NFLGamesStartedFirst3`, with class x position FE; release and retention in
  `player_season` (`WeeksCut`, presence in t+1). This margin disciplines
  B3-B5; bound selection with Lee (2009, *Review of Economic Studies*).
- *Threat:* voluntary exits (retirement, injury) are not separated from cuts.
  The at-risk population misses undrafted players who were not combine
  invitees.
- *Data:* drafted 2011-22: `VeteranContract` = 1 for 57.7%.

**B7. Positional sorting ("stacking"), quarterbacks, and bias in the signals
themselves.**
- *Designs:*
  - Are Black high-school QBs moved off QB more often, conditional on
    `RecruitRating`?
  - Among QBs, is there a gap in draft slot and pay conditional on passing
    production?
  - Outcome tests of the signals (Knowles, Persico and Todd 2001, *JPE*): does
    the same `RecruitRating`, `PreDraftGrade` or pick predict different later
    production by race? This is statistical discrimination before the NFL.
- *Threat:* position switches reflect athleticism; power for QBs is low;
  infra-marginality in outcome tests.
- *Data:*
  - 348 NFL players were recruited as high-school QBs; 72 (20.7%) never
    play QB.
  - 136 QBs were drafted in 2011-22, and there are 353 veteran QB contracts
    signed 2014+.
  - 5,260 prospects have a recruit profile.

### C. Further questions

**C1. Player-coach race match.** Do players perform, get paid, or stay
longer under a same-race position coach?
- *Design:* player FE with identification from coach turnover (Dee 2004,
  *Review of Economics and Statistics*; Fairlie, Hoffmann and Oreopoulos 2014,
  *AER*). Use independently measured outcomes (EPA, PFR coverage measures),
  since snaps are the coach's own choice.
- *Gap:* the player x position-coach crosswalk (7,410 position-coach
  person-seasons to map) does not exist yet.

**C2. Hometown and outcomes.** Do players from poorer or more segregated
counties earn less or go later in the draft, conditional on performance?
- *Design:* county variation within state (`RecruitState` FE); also usable as
  a falsification check.
- *Gap:* `RecruitCountyFips` exists (5,236 prospects), but there are no county
  covariates yet; merge ACS or SAIPE.

**C3. College-to-NFL coaching pipeline.** Are Black college head coaches less
likely to move to the NFL, conditional on their record?
- *Data:* `college_coaches` (2,928 FBS head-coach seasons, 529 coaches,
  2004-2025).
- *Limitation:* only 57 NFL team-seasons have a head coach with college
  head-coaching experience, and college assistants are not in CFBD.

### Before any of this

1. **Hand-code race** per `notes/race-coding-protocol.md`:
   - tiers 1-2 (1,051 head coaches, coordinators, GMs, executives, owners) for
     A1-A7;
   - tiers 3-4 (2,545 other coaches and front office) for A2, A5 and C1;
   - tiers 5-6 (8,214 players with a 2011-26 contract) for B1-B7.
2. **Report agreement.** Report raw agreement and Cohen's kappa by tier from
   `programs/08-race-coding-agreement.R`, and adjudicate the flagged cases.
3. **Keep proxies out of the treatment.** Use `p_black_bifsg`, `race_bifsg`
   and `black_provisional` only for measurement-error work (bounds,
   misclassification corrections), never as the treatment.
4. **Build what the designs need** (see the gaps below):
   - an HC spell and hire file (A1, A4);
   - preseason win totals (A1);
   - the player x position-coach crosswalk (C1);
   - an HBCU list and pre-2022 FCS statistics (B2);
   - a hand-coded top football executive (A7);
   - OL quality measures (B3).

## Gaps that matter for the research designs

For each item below, add a loader in `scripts/` that caches raw responses
under `data/raw/` and writes tables with `common.write_table`. Then register
the loader in `scripts/99_run_all.py` and add checks to
`scripts/06_validate_db.py`.

### Race (all designs)
Hand-coding is set up but not done (`data/hand_coded/race_coding/`). Code
tier 1 (176 head coaches) and tier 2 (coordinators, GMs, owners) first. The
team-level designs need tiers 1-4. The pay designs need tiers 5-6, about
8,200 players.

### Top football decision-maker
In 59 of 608 template-era team-seasons nobody holds a GM title (e.g. NE under
Belichick, CIN under Mike Brown). A rule based on titles picks the wrong
person too often, so this needs a hand-coded file (franchise, season,
person_id, source), for example `data/hand_coded/top_football_exec.csv`.

### Pro Bowl / All-Pro by season
nflverse draft picks carry career counts only. Season-level honors are on
Pro-Football-Reference award pages. PFR blocks direct requests, so use the
Wayback Machine at 3 seconds or more per request.

### PFF grades
PFF grades are the standard OL/DL/coverage quality measure. They require a
license and CSV export; join them on `pff_id`, which is in `nfl_players` and
`nfl_ids`.

### Contract signing dates, agents, cap space
OverTheCap gives no signing date. Spotrac and OverTheCap pages list dates,
agents and team cap space; check each site's terms before scraping.

### High-school game statistics
No source used here has them; the 247 composite summarizes the high-school
signal the market sees. MaxPreps is the only broad source; it has no API and
is protected by Cloudflare. Any sample from it would be partial and
non-random.

### Pre-2009 college box scores
CFBD has almost no player statistics before 2009. Sports-Reference college
pages (`cfbref_id` in `nfl_ids`) cover earlier seasons.

### College assistant coaches
CFBD lists FBS head coaches only. Coordinators and position coaches would come
from school media guides or Wikipedia season articles.

### Participation data
`nflreadpy.load_participation` gives the 22 players on the field for each play
(2016+). Use it for play-level OL and coverage exposure. The raw
play-by-play is already cached in `data/raw/nflverse/pbp_<season>.parquet`.

## Coverage gotchas

- **Team codes:** always join on `franchise_id`. The raw `team` columns mix
  GSIS (ARZ, BLT, CLV, HST, SL), historical (OAK, SD, STL) and current codes.
- **Rosters:** `nfl_rosters_weekly` keeps every status. Use `is_key_primary`
  for one row per player-team-week, and status `ACT`/`INA` for the game-day
  roster. Practice-squad (`DEV`) rows appear only from 2017.
- **College stats:** never sum rate or maximum stat types (PCT, YPA, AVG,
  LONG) across seasons; recompute rates from their components. Use
  `college_team_stats` completeness flags to set FCS (before 2022) and FBS
  (before 2009) production to missing.
- **Crosswalk confidence:** exclude `confidence = 'medium'` links in
  robustness checks. `first_college_season` can be early for players whose
  CFBD roster rows were backfilled.
- **Staff snapshots:** a person listed only in the late snapshot joined
  mid-season or was listed late by editors. Use the game-day snapshot
  assignment in `team_game` for timing-sensitive designs.
- **Contracts:** `guaranteed = 0` can mean unknown. Flag or exclude those rows
  for guarantee outcomes.
