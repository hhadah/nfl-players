# NFL Player Capability Database

A starter project for building a comprehensive player database that joins NFL stats, college stats, recruiting/HS data, and team-level context.

## Architecture

Single DuckDB database (`data/nfl_research.duckdb`) with these tables:

**Player tables**
- `nfl_players` — roster/bio data, IDs, college name
- `nfl_player_stats` — weekly + seasonal NFL stats
- `nfl_combine` — combine measurables (40, bench, vertical, etc.)
- `nfl_contracts` — historical contracts from OverTheCap (value, APY, guaranteed, cap %)
- `college_players` — college roster data
- `college_player_stats` — college season stats
- `recruits` — 247Sports composite ratings, HS info, measurables

**Team tables**
- `nfl_teams` — franchise info
- `nfl_team_stats` — weekly + seasonal team stats
- `college_teams` — FBS/FCS programs
- `college_team_stats` — season-level team performance (SP+, SRS, etc.)

**Coach tables**
- `college_coaches` — head coach by school by season, with W-L and SP+ context
- `nfl_coaches` — HC / OC / DC by team by season

**Join tables**
- `player_id_map` — canonical_id ↔ gsis_id ↔ pfr_id ↔ cfbd_id ↔ recruit_id

## Data Sources

| Tier | Source | Cost | Coverage |
|------|--------|------|----------|
| NFL | nfl_data_py (nflverse) | Free | 1999–present full, older partial |
| NFL Combine | nfl_data_py | Free | 2000–present |
| College | CollegeFootballData API | Free w/ key | ~2000–present |
| Recruiting/HS | CollegeFootballData API | Free w/ key | 2000–present (247 composite) |
| RAS scores | Kent Lee Platte's site | Free | 1987–present |

## Setup

```bash
pip install nfl_data_py cfbd duckdb pandas requests beautifulsoup4 --break-system-packages
```

CFBD requires a free key from https://collegefootballdata.com/key. Save it
once in `.env` at the project root (gitignored — never committed):
```
CFBD_API_KEY=your_key_here
```
`scripts/config.py` reads `.env` automatically; no shell exports required.

## Run order

```bash
python scripts/99_run_all.py             # runs the whole pipeline below
```

Or step by step:
```bash
python scripts/01_build_db.py            # schema (idempotent)
python scripts/02_load_nfl.py            # nflverse: rosters, weekly stats, contracts, ...
python scripts/02b_load_nfl_coaches.py   # PFR (via Wayback) HC + OC + DC + STC + GM + asst
python scripts/03_load_college.py        # CFBD pulls (incl. coaches)
python scripts/04_load_recruiting.py     # 247 composite + HS
python scripts/04c_infer_race.py         # BIFSG name → race for players + coaches
python scripts/05_join_players.py        # build player_id_map
python scripts/06_example_queries.py     # sanity checks
python scripts/07_export_csv.py          # all CSV exports incl. weekly panel
python scripts/08_nfl_coach_data.py      # team-week × coach × race × diversity
```

Total runtime first time: ~30–60 min depending on year range. The expanded
coach scrape adds ~20 min (480 PFR team-year pages). Subsequent runs upsert.

## Race / ethnicity inference

`scripts/race_utils.py` predicts race probabilities from names using **BIFSG**
(Bayesian Improved First-name Surname Geocoding without geocoding). It
combines:

- **Census 2010 surname file** — 162k surnames with race percentages.
- **Tzioumis (2018) first-names file** — 4,250 first names with race
  percentages (HMDA mortgage records, doi:10.7910/DVN/TYJKEZ).

For each name we compute  P(race | first, last) ∝ P(race | first) × P(race | last) / P(race),
where P(race) is the U.S. population marginal. When only one signal is
available the script falls back gracefully (`race_source` column records
which signal was used: `bifsg` / `surname` / `firstname`).

**Caveats** — BIFSG over-corrects toward whatever race is dominant in the
U.S. population when both signals weakly support it (e.g. a common
white-leaning first name like "Mike" can pull the prediction toward white
even when the surname is more racially-mixed). For *individual* labels prefer
the surname-only signal; for *group* statistics (diversity indices) use the
probability vector directly rather than the discrete `race_pred` argmax.

## Output datasets

`data/datasets/`:
- `panel_player_team_week.csv` — every active player × team × season × week
  with weekly stats, team weekly stats, season coaches, contract,
  recruiting, college team-season context, college career stats, and race.
- `nfl_team_coach_week.csv` — team-week stats, named coaches (HC/OC/DC/STC/
  GM), per-coach race, plus a per-team-season diversity index (Blau,
  Shannon, share-non-white) over the FULL coaching staff.
- `players_master.csv`, `nfl_player_stats.csv`, `college_player_stats.csv`,
  `recruits.csv`, `nfl_team_seasons.csv`, `college_team_seasons.csv`,
  `nfl_contracts.csv` — flat per-topic exports.
