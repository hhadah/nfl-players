# Extending the dataset

What this starter gives you and what it doesn't.

## What's covered

- **NFL career**: bio, draft, weekly + seasonal stats, combine, NextGen
- **College career**: roster, season stats by category, team SP+/SRS context
- **Recruiting / HS**: 247 composite, star ratings, HS school, hometown, height/weight at recruitment
- **Team context**: weekly + season records on both NFL and college sides

## What's NOT covered (and how to add it)

### High school game stats
The CFBD recruiting data gives you HS *identity* (school, ratings, measurables) but not HS *production* (yards, TDs, etc.). Options:

1. **MaxPreps** — most complete HS source. No API. Scraping is technically feasible but the site uses Cloudflare and rate-limits aggressively. Use Selenium/Playwright with realistic delays. Expect ~30% coverage even then.
2. **Hudl** — has profiles for many recruits but stats are spotty and behind login.
3. **Local newspaper archives** — for individual players, sometimes the most detailed source.
4. **247Sports profile pages** — scrape directly. Slower but more permissive than MaxPreps.

### NFL play-by-play
Set `INCLUDE_PBP = True` in config.py and add this loader:
```python
pbp = nfl.import_pbp_data(NFL_YEARS)   # ~50k rows per season
```
Big table — 1M+ rows for a decade. Useful for situational analysis (third-down conversions, pressure rates, etc.).

### PFF grades
Best in the business for player capability metrics, but ~$40/mo for Premium Stats and they don't have a public API. You'd export CSVs from their site.

### RAS (Relative Athletic Score)
Kent Lee Platte publishes RAS as a Google Sheet at https://ras.football. It's a 0-10 percentile composite of combine performance. Easy to download as CSV and join to `nfl_combine` on player name.

### Pro Football Reference scraping
For pre-1999 data or the linked college page, scrape PFR. They allow ~20 requests/min. Each player page has a deterministic URL: `/players/<letter>/<pfr_id>.htm`.

### Injuries
nfl_data_py has `import_injuries(years)`. Add a table for it if relevant to your research.

### Contracts / cap hits
Spotrac and OverTheCap don't have APIs. nflverse has a partial dataset via `import_contracts()` — try that first.

## Coverage gotchas

- **Transfers**: a player who attended 3 colleges has multiple `college_players` rows. Handle in your queries with `GROUP BY cfbd_id`.
- **JUCO**: many players spend a year at junior college. CFBD only covers FBS/FCS, so JUCO years are missing.
- **International players**: Efe Obada-types skip the college tier entirely. Their `cfbd_id` will be NULL — that's correct.
- **Name collisions**: Mike Williams and James White each have multiple NFL careers. The DOB field in `player_id_map` is your tiebreaker — add it as a join condition for ambiguous names.
- **2024+ NIL era**: stats look the same but recruiting rankings have shifted in meaning.

## Schema additions to consider

```sql
-- Pro Day results (some prospects skip combine)
CREATE TABLE pro_days (...);

-- Injury history
CREATE TABLE injuries (...);

-- Awards / honors (All-American, Heisman, Pro Bowl, AP)
CREATE TABLE honors (...);

-- Snap counts (great for "did they actually play" context)
CREATE TABLE nfl_snap_counts (...);   -- nfl_data_py.import_snap_counts()
```
