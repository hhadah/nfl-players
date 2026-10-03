# Race and ethnicity coding protocol

> **Status (2026-10-03):** hand coding was not carried out. The PI chose
> predicted race instead (`notes/race-prediction-design.md`). This protocol
> and the sheets stay in the repository. If codes are entered, every
> estimation script uses them automatically once they cover at least 80% of
> its sample.


Purpose: produce a validated race/ethnicity measure for NFL coaches, front-office
staff and players. Name-based inference (BIFSG, `race_bifsg` in the DuckDB)
misclassifies most Black individuals in this population: in the 2026-05 build,
17 of 20 Black head coaches were labelled white. It is therefore used only as a
secondary measure and for misclassification-correction exercises. The primary
measure is the human coding described here.

## Who is coded (priority tiers)

Sheets are ordered by tier; code tier 1 first. Tier definitions are applied by
`scripts/09_race_coding_sheets.py`.

| Tier | Population |
|---|---|
| 1 | Head coaches (including interim) |
| 2 | Coordinators (OC, DC, STC, pass/run-game coordinators), assistant head coaches, GMs, assistant GMs and football-operations executives, owners, chairs, presidents/CEOs |
| 3 | All other on-field coaches (position coaches, assistants, quality control, S&C, other coaching staff) |
| 4 | All other front-office staff (personnel, scouting, administration) |
| 5 | Players with a veteran contract (OverTheCap type UFA, RFA, ERFA, SFA, Extension, Franchise or Transition) signed 2011-2025 |
| 6 | Players with a drafted-rookie or UDFA contract signed 2011-2026 |
| 7 | All other players on 2002-2025 season rosters |

A staff person gets the most senior tier any of his roles implies (staff come
from `staff_team_season`, 1999-2025). Players are the `nfl_players` ids
(gsis_id). A player who later joined a staff is coded ONCE, as staff: his
row carries the player's id in `also_player_gsis_id` and his player career in
the context, and he has no player row. These links come from Wikidata (the
staff article's item -> property P3561, Pro Football Reference player ID) and
are listed in `person_links.csv`. For staff without a Wikidata item, a unique
name and timing match is only a candidate: the context says "POSSIBLY the NFL
player ..."; check it and write "not the same person" in `notes` when it is
wrong (the player keeps his own row either way).

## Categories

Each coder records, for every person:

- `race`: one of `black`, `white`, `asian`, `pacific_islander` (includes
  Samoan, Tongan, Native Hawaiian), `american_indian` (American Indian/Alaska
  Native), `multiracial`, `unknown`.
- `multiracial_components`: when `race = multiracial`, the components separated
  by `;` (e.g. `black;white`).
- `hispanic`: `yes`, `no` or `unknown` (ethnicity is recorded separately from
  race, as in the Census).
- `basis`: the strongest evidence used, from the hierarchy below
  (`self_id`, `documented`, `list`, `photo`).
- `source_url`: the URL of that evidence (required unless `basis = photo`).
- `confidence`: 3 = certain, 2 = probable, 1 = uncertain.
- `notes`: free text (e.g. conflicting sources).

Derived in R (not coded): `black_any` = 1 if `race = black` or `black` is a
component of a multiracial code; `nonwhite` = 1 unless `race = white` and
`hispanic = no`.

## Evidence hierarchy

Use the highest available level and record it in `basis`:

1. `self_id` — the person's own public statements (interviews, memoirs,
   foundation or team biographies written in the first person).
2. `documented` — reliable third-party documentation that states race or
   ethnicity: news coverage ("the franchise's first Black general manager"),
   team or league biographies, Wikipedia article text with a citation.
3. `list` — published lists: Fritz Pollard Alliance materials, the TIDES
   Racial and Gender Report Card (aggregate lists of minority head coaches,
   coordinators and GMs), league diversity reports.
4. `photo` — perceived race from a public headshot. Use only when levels 1-3
   are unavailable; confidence may not exceed 2.

## Procedure

1. `scripts/09_race_coding_sheets.py` writes, in `data/hand_coded/race_coding/`:
   `persons_to_code.csv` (the universe), `coder_A.csv` and `coder_B.csv`
   (identity columns `person_uid`, `tier`, `entity`, `display_name`, `context`,
   `wiki_url`, `headshot_url`, `pfr_url`, `also_player_gsis_id`, `stale`, then
   the blank coding columns), `person_links.csv` and a header-only
   `adjudicated.csv`. Rerunning it refreshes identity columns and appends new
   people; it never overwrites or drops a code (it stops if a merge would
   change the number of coded cells), and people who leave the universe keep
   their rows with `stale = 1`.
2. Two coders (A and B) work independently in `coder_A.csv` and `coder_B.csv`.
   Coders do not see each other's sheet. The sheets carry no machine signals;
   coders do not look at the Wikipedia category flags (DuckDB tables
   `staff_person_wiki_signals`, `player_wiki_signals`) or the BIFSG
   probabilities (`race_bifsg`) while coding, to avoid anchoring. The article,
   headshot and Pro-Football-Reference links on the sheet are sources to read,
   not signals.
3. Do not edit identity columns. Leave rows you have not reached blank. Keep
   the file as CSV (UTF-8) and do not re-sort or de-duplicate it by hand.
4. `programs/08-race-coding-agreement.R` computes raw agreement and Cohen's
   kappa for `race`, `hispanic` and `black_any`, overall, by tier and by entity
   type, and writes `adjudication_needed.csv`: persons whose coders disagree on
   `race`, `hispanic` or `multiracial_components`, or with any code of
   confidence 1, and who are not yet in `adjudicated.csv`. It also tabulates the
   misclassification of the machine signals against the hand codes.
5. The PI adjudicates in `adjudicated.csv` (columns `person_uid`,
   `display_name`, `tier` and the coding columns; one row per person). The
   adjudicated code is the primary race measure in every analysis sample; the
   sample records its source (`race_source` = `adjudicated`, `coder_agree`,
   `single_coder`; a coder disagreement without adjudication is `disputed`).
6. Report the agreement statistics and the share coded by each basis in the
   paper's data appendix.

## Scope and handling

- Coding is limited to public figures in their professional roles, using
  public information.
- Person-level codes stay in this repository's `data/hand_coded/` and are not
  published; any released data should aggregate to the team-season level.
- Machine signals are a screening aid only. The absence of a Wikipedia
  category is not evidence of race (for example, Carnell Lake has no
  race-related category).
