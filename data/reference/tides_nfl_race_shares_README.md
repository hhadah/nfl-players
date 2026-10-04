# TIDES NFL racial/ethnic shares: reference file

`tides_nfl_race_shares.csv` holds league-wide racial and ethnic shares for NFL players, head coaches, assistant coaches, coordinators and general managers. Every value was read from a published source; nothing is estimated, interpolated or derived. Missing seasons are left missing. The file exists to validate the automated race-prediction model against published benchmarks.

Built 2026-10-02 from The Institute for Diversity and Ethics in Sport (TIDES, University of Central Florida; Richard Lapchick, later Adrien Bouchet), *Racial and Gender Report Card: National Football League* (RGRC), plus two all-league RGRCs for the early years. All PDFs were downloaded from tidesport.org (`/nfl` and `/complete-sport` pages).

## Layout (one row per value)

| column | meaning |
|---|---|
| `report_year` | Year in the report title (e.g., "The 2013 Racial and Gender Report Card") |
| `season` | NFL season the value describes, as the source states it (see "Season timing") |
| `as_of` | Reference date the report gives for that group's current figures (e.g., `2011-09-01`, `2023-08`). Blank for full-season media-guide data and for retrospective statements |
| `group` | `players`, `head_coaches`, `assistant_coaches`, `coordinators`, `general_managers` |
| `category` | `black`, `white`, `latino`, `asian`, `asian_pacific_islander`, `pacific_islander`, `american_indian_alaska_native`, `two_or_more_races`, `two_or_more_races_or_other`, `not_disclosed`, `other`, `people_of_color` |
| `value`, `unit` | Number as printed (trailing zeros dropped: 25.0 → 25); `percent` or `count` |
| `base_n` | Total head count, only where the source prints one (e.g., 537 assistant coaches in 2012) |
| `source_type` | `primary` (TIDES PDF) or `secondary` (news coverage, used only where TIDES has no report) |
| `page_or_section` | Physical PDF page (`PDF p. N`, counting the cover as page 1) plus section. Printed page labels in 2018+ reports are about 2 lower |
| `quote` | Verbatim text containing the number, under 25 words. For table cells the quote is a transcription (`Appendix ..., row YEAR: Label X% (n=...)`), not prose |
| `notes` | Category counts from tables (`count = ...`), conflicts and caveats |

The file has 733 rows: 731 primary and 2 secondary. 391 rows transcribe the retrospective Appendix II tables of the 2023 report. The other 342 come from each report's text and its own current-season table cells. These include a few backward-looking statements, such as the 2015 values given in the 2016 report, and the 2 secondary rows.

**How the quotes were checked.** Each prose quote was checked by script against the `pdftotext` output of its PDF, after normalizing whitespace, curly quotes and dashes. The check also confirmed that the value appears in the quote. A handful of quotes match only after ignoring line-break hyphenation; in those, the PDF text extraction gives forms like "AfricanAmerican" or "African- American". For each table row, the transcribed cells were confirmed to appear on the cited page.

## Coverage

| group | seasons with contemporaneous TIDES values | extra retrospective values (2023 Appendix II) |
|---|---|---|
| players | 2000, 2001, 2003, 2005–2016, 2019–2023 | 1990–1999, 2003, 2005–2014, 2016, 2019–2023 |
| head coaches | 2000–2023 (counts; percentages for some years) | 1992–2023 (gaps: 1996, 1998, 2002, 2004) |
| assistant coaches | 2000, 2002*, 2003–2023 (2004: count only) | 1994–2023 (gaps: 1998, 2000, 2002, 2004) |
| coordinators | 2002*, 2003–2017 (counts of African-American coordinators; 2004 = coordinators of color) | none |
| general managers | 2000–2023 (2004 missing) | 1995–2023 (gaps: 2000, 2002, 2004) |

\* Season label is ambiguous; see "Conflicts".

**Missing.** There are no TIDES player figures for 2002, 2004, 2017 or 2018. The 2017 and 2018 reports reuse the 2016 player data, so they add no new rows. There is no 2007 NFL report; the 2008 report covers the 2007 season.

No TIDES NFL report for 2024 or 2025 could be found. tidesport.org/nfl lists reports through 2023, and its "2024" Complete RGRC link opens the same file as the 2022 Complete RGRC. For 2024 the file has only two secondary rows: an AP story (Jan. 26, 2024, via FOX Sports) reporting 9 head coaches of color and 6 Black head coaches entering the 2024 season. No authoritative published player shares for 2024 or 2025 were found. The NFL's own DEI reports were searched; their player demographics were not located.

**Seasons before 2000.** These come only from the 2023 Appendix II, plus one statement in the 2001 RGRC that whites were 32% of players in 1999. The 2003 RGRC also has an NFL players column labelled 1989-90 through 2001-02; only its 2001 figures were transcribed.

## Player Black/African-American share (contemporaneous figure for each season)

2000: 67 · 2001: 65 · 2003: 69 (table 69.2) · 2005: 66 (table 65.5) · 2006: 67 · 2007: 66 · 2008: 67 · 2009: 67 · 2010: 67 · 2011: 67 (restated as 66.6 in the 2013 report) · 2012: 66.3 · 2013: 67.3 · 2014: 68.7 · 2015: 69.2 · 2016: 69.7 · 2019: 58.9 · 2020: 57.5 · 2021: 58.0 · 2022: 56.4 · 2023: 53.5 (percent).

The 2023 retrospective appendix gives these values for 2003–2014: 69.2, 65.0 (2005), 65.8, 64.9, 65.2, 65.9, 66.4, 66.1, 65.4, 66.3, 68.0. They differ from the contemporaneous values because the denominator differs (next section).

## Definitions and how TIDES counts

- **Who assigns race.**
  - 2003–2016: the TIDES research team coded race from team media guides; the reports say "Baseline data was gathered from ... media guides". From 2015 the NFL League Office audited the data.
  - 2017: the NFL Player Personnel Department switched to a self-identification "Player Information Form". Results were not available for the 2017 or 2018 reports.
  - 2019 onward: player figures use self-identified categories, which add "two or more races" and "not disclosed/chose not to specify". The 2018 assistant-coach table already includes these categories.
- **Does "African-American" include multiracial players?** Before 2019 there was no multiracial category: each player was put in exactly one of White, African-American, Latino, Asian (later Asian/Pacific Islander) or Other. So multiracial players were counted inside a single-race category. From 2019, "Black or African American" excludes players who identify as two or more races (9.1% in 2019, 10.9% in 2023) and those who did not disclose (3.1% in 2019, 8.9% in 2023). The 2019 report itself says "The addition of these two racial categories helps to explain the decline in players of African-American classification." The drop from 69.7% (2016) to 58.9% (2019) is therefore largely a break in definition, not a change in rosters. Compare pre-2019 "African-American" with post-2019 "Black + two or more races" (or with people of color), not with "Black" alone.
- **Asian and Pacific Islander.**
  - Labels vary by report: "Asian-American" (2001–2004), "Asian" (2005–2007, 2010), "Asian/Pacific Islanders" (2009, 2011–2016 text). The `category` column follows the label used in the cited passage.
  - From 2019, Native Hawaiian/Pacific Islander is separate (1.4–1.8%), and Asian falls to 0.1%. This suggests Polynesian players were inside "Asian"/"Asian/Pacific Islander" before 2019. Treat `asian` before 2019 as roughly Asian + Pacific Islander.
- **"Other" and "International".**
  - Tables for 2003–2016 also list "International" players (1–4%) separately. The contemporaneous percentages appear to leave them out of the denominator. As an arithmetic check (not a data value): for 2009, 1,761 African-American players out of 782 + 1,761 + 24 + 55 + 2 = 2,624 gives 67%, the figure printed.
  - The 2023 retrospective appendix moves International into "Other" and recomputes: with the same counts it shows 65.9% for 2009.
  - The 2022 report merges "two or more races/other" (10.5%).
- **People of color.** TIDES uses this for everyone who is not white. For players from 2019, it equals 100 − white − not disclosed (2019: 100 − 26.8 − 3.1 = 70.1; 2023: 100 − 24.4 − 8.9 = 66.7). "Grade box" values for 2012–2016 (70, 69, 71.4, 72.6) are the people-of-color percentages printed beside the players grade.
- **Head counts.** The player counts in TIDES tables are far larger than 53 × 32 (for example, the 2009 table lists 782 white and 1,761 African-American players). They come from media guides and rosters that include players beyond the 53-man rosters, and the universe changes from year to year. Use shares, not counts, for validation.
- **Head coaches.** These are counts and percentages at the start of the season, usually out of 32 teams. Two rows (2022 and 2023, 21.9%) are dated "as of publication" and include in-season interim hires; their `as_of` and `notes` fields say so. Before 2011 the text gives African-American head-coach counts. The people-of-color count first appears in the 2011 report and adds, for example, Ron Rivera (Latino) and later Robert Saleh and Mike McDaniel.
- **Assistant coaches.** This is everyone on the coaching staff below head coach: assistant head coaches, coordinators, position coaches and strength staff (2021 definition). Printed totals include 537 (2012, 2013 report) and 631 (2018, 2018 report).
- **Coordinators.** Counts of African-American coordinators. Until the 2013 report these are offensive and defensive coordinators. From the 2014 report (2013 season) the lists add special-teams, run-game and similar coordinator titles. TIDES stopped reporting coordinator counts after the 2017 report.
- **General managers.**
  - 2000–2016 is "General Manager/Principal-in-Charge": the position the NFL defines as equivalent to GM, which can be a VP of football operations or player personnel. 2017 explicitly adds VPs of personnel performing the GM role. From 2021 it is "General Manager / Primary Football Executive".
  - The denominator is not always 32. For 2023, the appendix GM table lists 21 white, 8 Black and 1 Latino GMs, and the text gives 9 people of color = 30.0%.
  - Through the 2013 report the counts are African-American (the 2009 report calls the 2008 figure "minorities"). From the 2014 report they are people of color.

## Season timing (report year vs. season)

| report | player data | staff data / "as of" |
|---|---|---|
| 2001 RGRC (all leagues; PDF is a 10/5/06 re-issue) | 2000 season | coaching and GM changes updated to July 1, 2001 |
| 2003 RGRC (all leagues) | 2001 season | 2001 season (some 2002 statements) |
| 2004 NFL (pub. May 2005) | 2003 season | 2003 season; owners, GMs and head coaches updated May 15, 2005 |
| 2005 NFL | 2005 season | 2005 season; updated July 3, 2006 |
| 2006 NFL | 2006 season | 2006 season; updated Aug. 25, 2007 |
| 2008 NFL | 2007 season | 2007 season (+ 2008 notes); Aug. 2008 |
| 2009–2014 NFL | season = report year − 1 | assistant coaches = report year − 1; head coaches and GMs = start of report-year season (Aug.–Sept.) |
| 2015 NFL | "2014 season" rosters, as of July 1, 2015 | 2015 season, as of Aug. 21, 2015 |
| 2016 NFL | 2016 (as of Aug. 2016) | 2016 |
| 2017, 2018 NFL | reuse 2016 data (no rows added) | 2017 (Sept. 2017), 2018 (Dec. 2018) |
| 2019–2023 NFL | season = report year (as of Aug.–Sept.) | same |

## Conflicts and known errors (all kept as printed, flagged in `notes`)

- **1999/2000 labels.** The 2001 RGRC gives the following figures for the **2000** season: 67% African-American players; 2 African-American head coaches (6%), "one less ... than in 1999"; assistant coaches 72% white / 28% African-American; GMs 87% white / 13% African-American. Later TIDES appendices (2004 onward, including 2023) list these same numbers under **1999**, mark 2000 as not recorded for players, assistant coaches and GMs, and put 3 head coaches (10%) under 2000. The 2001 text is the contemporaneous source. Treat appendix labels for 1999 and 2000 as unreliable.
- **2003 RGRC assistant coaches.** The paragraph says "2002 season" (71% white / 28% African-American, 12 African-American coordinators). But the report says it covers the 2001 NFL season, and later appendices list 71/28 under 2001. Season = 2001 or 2002.
- **Restated values.**
  - Assistant coaches of color: 2010 is 32% (2011 report) vs 33% (2012 report); 2012 is 32% (2013 report) vs 33% (2014 report); 2017 is 31.3% (body) vs 31.4% (executive summary); 2022 is 42.9% (2022 report) vs 42.8% (2023 report).
  - African-American coordinators: start of 2011 is 9 (2011 report) vs 8 (2012 report); 2016 is 13 (2016 report) vs 14 (2017 report).
  - Head coaches 2004: 16% before the 2004 season (2004 report) vs "three in 2004" (2005 report).
  - African-American GMs 2006: five as of July 2006 (2005 report) vs four in the 2006 season (2006 report).
- **2019 players.**
  - The text gives 9.1% two or more races; the appendix table prints 9.6% for the same 150 players. 9.1% matches the counts, so 9.6% looks like a typo.
  - Pacific Islander is 1.4% in the 2023 appendix vs 1.5% in the 2019 table.
  - The 2019 sentence listing Latino, Asian, NHPI, AIAN and not-identified gives five groups but four numbers, so only table values are used for those categories.
- **Errors in TIDES's own appendices (not used for any row).** The 2019 appendix repeats the 2016 player row under 2015. The 2016 appendix omits 2015. The only 2015 player figures are 69.2% Black and 27.9% white, stated in the 2016 report's text.
- **2003 RGRC.** It prints "other players of color (.06 percent)" (kept as 0.06). Its own table shows Other <1%.
- **2001 RGRC.** It prints "Americans-Americans" (sic) for African-Americans.

## Sources

- TIDES NFL RGRC PDFs, 2004–2023 (no 2007): links on https://www.tidesport.org/nfl. The exact PDF URL is in `source_url`; tidesport.org links redirect to a filesusr.com host.
- *The 2001 Racial and Gender Report Card*: https://www.tidesport.org/_files/ugd/7d86e5_fff5f25555de4e0087a8078790afe2fc.pdf
- *The 2003 Racial and Gender Report Card*: https://www.tidesport.org/_files/ugd/7d86e5_2b6a39fc09ce4fdfa47bbae7b94458de.pdf
- Secondary, 2024 head coaches only: Associated Press via FOX Sports, Jan. 26, 2024, https://www.foxsports.com/stories/nfl/nfl-reaches-major-milestone-with-record-nine-minority-head-coaches-in-2024
