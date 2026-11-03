# TODO: make the repo runnable on a fresh clone

Originally written 2026-09-28 from the laptop; revised 2026-10-29 on the
laptop (`C:\GitHubRepos`) after auditing the actual state, and 2026-11-02 to
add steps 5 and 6. Delete this file (and the pointer in `CLAUDE.md`) once
steps 2, 5 and 6 are done.

## Status

| Step | State |
|---|---|
| 1. Sibling-repo layout | Works. `bh-school-system`, `QM_Fork` and `BH_Schools_Consultation` are all present as siblings. |
| 2. Missing data | **Done (2026-11-03, laptop).** `school_effect_decomp.rds`, the one unrebuildable file, is committed, and `R/00_fetch_data.R` rebuilt the other fourteen outputs here on its first real run. Two ordering bugs it exposed are fixed. |
| 3. Hard-coded `E:` paths | Done. |
| 3b. Cross-repo reads | Done: `optionZ_Mar25.geojson` is tracked; `cluster_data_explorer.qmd` now falls back to `data/cache/la_boundaries.rds`. |
| 4. Render check | **Done (2026-11-03, laptop).** `bh_schools_talk_2026_v2.qmd` renders clean: all 51 chunks, no errors or warnings. But it renders *different numbers* than the committed HTML, because `bh-school-system` has moved on. See below. |
| 5. Suspension pupil fields | **Done (2026-11-02, main machine).** |
| 6. Ofsted ratings | Done (2026-11-02, main machine): ratings dated, and a "latest Ofsted" field from Ofsted's monthly data beside the model's rating. |

## Step 3 — done

Both remaining `E:` paths are gone:

- `output/brighton_case_study.qmd` already used `here::here("data",
  "optionZ_Mar25.geojson")`; the absolute path had been fixed previously and
  the note in the old version of this file was stale.
- `R/01_extract_data.R` now resolves QM_Fork as a sibling via
  `dirname(here::here())` instead of `E:/QM_Fork/...`. Verified: the path
  resolves and `dir.exists()` is `TRUE` on this machine. The call site is
  already guarded by `dir.exists()`, so it degrades to a warning elsewhere.

A grep for `E:/` across `*.R`, `*.qmd`, `*.py` and `*.md` now returns no code
paths — only the prohibition in `CLAUDE.md` and the prose in this file.

## Step 2 — the real problem, and why the original plan was wrong

The old plan was "get the missing data onto GitHub". That is mostly the wrong
fix, for two reasons:

1. **The repo is public.** Committing bulk DfE extracts and derived panels
   raises licensing and embargo questions that should not be settled by
   default. `data/` is ~1.1 GB on this machine.
2. **Nearly all of it is rebuildable.** 16 of the 17 files listed as missing
   are produced by something already in the repo. They do not need to be in
   git at all; they need a documented build order.

### Rebuildable — run the script, don't commit the output

| File | Built by |
|---|---|
| `data/ks4_destinations.rds` | `R/09_ks4_destinations.R` |
| `data/gias/gias_establishments.rds`, `data/gias/school_closures.rds` | `R/10_gias_closures.R` |
| `data/engagement/absence_by_yeargroup.rds`, `suspensions_by_yeargroup.rds`, `suspensions_school.rds` | `R/11_ees_engagement.R` |
| `data/place/absence_residence_idaci.rds`, `pupil_projections.rds`, `pupil_yield.rds` | `R/12_ees_place_demand.R` |
| `data/census/school_census_context.rds` | `R/13_census_context.R` |
| `data/early_years/la_entry_baselines.rds` | `R/14_early_years_baselines.R` |
| `data/ks2/ks2_panel.rds` | `R/20_extract_ks2.R` |
| `data/ks5/ks5_panel.rds` | `R/30_extract_ks5.R` |
| `data/cache/oa_pwc_2021.rds` | `R/13_census_context.R` (`fetch_oa_centroids()`, paged ArcGIS REST) |
| `data/cache/analysis_f_results.rds` | `output/model_experiments.qmd`, Analysis F (~line 5526) |

These hit the EES/DfE APIs, so they need network access and take a while.
**`R/00_fetch_data.R` now exists (2026-11-02).** It runs whichever of these
have missing outputs, each in its own `Rscript` process, in dependency order
(GIAS before KS2, KS2 before census), and reports what failed or was
skipped. `--dry-run`, `--all`, named steps and `--analysis-f` (which renders
`model_experiments.qmd`) are documented in its header. Tested on the main
machine, where it correctly finds nothing to do, and with stubbed steps for
the fresh-clone paths.

**First real run: 2026-11-03, on the laptop.** Seven of the nine steps worked
first time. The two that failed exposed a bug in the step graph rather than
anything wrong with the data: `needs` is checked but never sorted on, so a
step must also be *listed* after everything it needs, and two were not.
`early_years` (`R/14`) reads the KS2 panel but declared no `needs` and ran
before `ks2`; `census` (`R/13`) reads the KS5 panel but declared only
`needs = "ks2"` and ran before `ks5`. Both now declare their real
dependencies and sit after them in `STEPS`, and the comment above `STEPS`
records why order matters. Re-running the two steps by name then succeeded,
so all fifteen outputs are present and a plain run reports nothing to fetch.
`census` had already cached `oa_pwc_2021.rds` before it died, so the ~95
paged centroid requests did not repeat. Run under R 4.5.1; there is no
`renv.lock` or `.Rprofile` pinning a version.

### Not rebuildable — the one actual blocker

`data/cache/school_effect_decomp.rds` is the only file nothing in the repo
creates. Every reference to it is a `readRDS`:

- read by `bh_schools_talk_2026.qmd`, `bh_schools_talk_2026_v2.qmd`,
  `pulling_the_right_lever_slides.qmd` and `..._slides_v2.qmd`
- `R/scratch_add_nondisadvantaged_decomp.R` **reads it, augments it and
  writes it back** — it adds `decomp_non` to an existing cache, so it cannot
  recreate one from nothing

So it exists only on the main machine, and four decks fail without it.
**Resolved 2026-11-02 by committing it**: it is 975 bytes of national
variance shares, the same figures the published decks show, so neither
size nor embargo stood in the way. `.gitignore` now ignores
`data/cache/*` with a `!` rule for this one file. Promoting the Analysis F
decomposition into a script remains the durable fix. The original note:
**Top priority.** Options: commit it (if small and not embargoed), ship it as
a GitHub Release asset via `piggyback::pb_upload()`, or promote the Analysis F
decomposition that produced it into a proper script alongside
`R/scratch_add_nondisadvantaged_decomp.R`. The third is the durable fix, and
would also make `analysis_f_results.rds` reproducible without rendering the
whole of `model_experiments.qmd`.

Committing needs `data/cache/` changed to `data/cache/*` in `.gitignore`
first, plus a `!` rule — git cannot re-include a file under an ignored
directory.

### Minor

- `data/la_boundaries.rds` is listed as missing, but a copy exists at
  `data/cache/la_boundaries.rds` (6.4 MB). `cluster_data_explorer.qmd` already
  falls back from `app/data/` to `data/`. Either copy it up a level or point
  the fallback at `data/cache/`.
- `data/optionZ_Mar25.geojson` is present (25 KB) and small enough to commit.
  `BH_Pupil_Destinations` and `bh-school-system/R/01_assemble.R` both read from
  this repo, so this one and `data/panel_data.rds` are the two that most need
  to be fetchable.

## Which machine is which

This matters and was ambiguous before. **`C:\GitHubRepos` is the laptop.** It
has the `school_attainment_tool` pipeline outputs (`panel_data.rds`, the
`models*.rds` set, `data/cache/` with the workforce/finance/Ofsted extracts).

It used to have **none** of `data/census/`, `data/early_years/`,
`data/engagement/`, `data/gias/`, `data/ks2/`, `data/ks5/`, `data/place/`, and
the old note here said step 2 could only be finished on the main machine (the
`E:` one). That is no longer true: `school_effect_decomp.rds` arrived by
commit, and `R/00_fetch_data.R` built all seven of those directories on the
laptop on 2026-11-03. `data-raw/ees/` is still absent here, so the `area_*.rds`
rebuild in step 5 remains main-machine work.

## Step 4 — check it

Once step 2 is resolved, pull on a fresh clone and run:

```
quarto render bh_schools_talk_2026_v2.qmd --to revealjs
```

**Run 2026-11-03 on the laptop, and it passed**: Quarto 1.9.37, R 4.5.1, all
51 chunks executed including `[decomp]` (the one that reads the
`school_effect_decomp.rds` committed in step 2), no errors and no warnings,
`bh_schools_talk_2026_v2.html` written at 12.9 MB self-contained. So the
fresh-clone path works end to end.

Two things the run turned up, neither a blocker:

- **The deck no longer renders the numbers it was published with.** The
  committed HTML was built 2026-10-14; `../bh-school-system/app/data/sim_inputs.rds`
  was rebuilt 2026-11-02 by that repo's commit `40a055a`, "Adopt the sibling
  tier (priority 3) as the model baseline". Every difference is on the
  simulation slides and consistent with a changed baseline: displacement
  under 20% open admissions 18 → 14 children, the deprived-pupil journey
  penalty 3.4 → 3.5 falling to 2.5 → 2.6 minutes, the cost −£0.8m → −£0.9m,
  and the headline counts 90 → 89, 117 → 91 becoming 114 → 90. Nothing in
  `data/` that step 2 rebuilt feeds these slides.

  **Resolved 2026-11-03: the sibling-tier baseline is adopted.** The
  re-rendered `bh_schools_talk_2026_v2.html` at the repository root is the
  new numbers, and it exposed one piece of prose that had silently gone
  stale. The intro to "Why segregation goes up" was hard-coded words —
  "about six more children", "about twelve more", "**twenty-eight** more" —
  so it did not move with the model and no diff of two renders could catch
  it. Recomputed against the current inputs those are 4.8, 8.1 and 34.2,
  and that sentence now takes all three from `lev` inline, in the same
  computed style as the key-finding block on the open-admissions slide, so
  it cannot go stale again. The slide's argument is unaffected: the two
  levers still multiply rather than add (4.8 + 8.1 against 34.2), and the
  Gorard index still rises, 0.1165 → 0.1174. A scan of the rest of the
  deck's prose found no other hard-coded simulation figure — the other
  literals in it ("ten schools", "six objections", "the 60 places") are
  structural, not model output.

  **All seven decks are now done (2026-11-03).** Seven read
  `sim_inputs.rds`, not three: `bh_schools_talk_2026`,
  `bh_schools_talk_2026_v2`, `ai4ci_collab_talk`, `ai4ci_collab_talk_short`,
  `ai4ci_collab_talk_v1`, `brighton_ai_talk` and `nov_2026_talk`. All were
  re-rendered clean, with no errors or warnings. Only three changed in any
  way that matters:

  | Deck | Text lines | Figures | Real change? |
  |---|---|---|---|
  | `bh_schools_talk_2026` | 16, all computed | 2 of 21 | yes |
  | `ai4ci_collab_talk` | 8 | 1 of 37 | yes, and prose was false |
  | `ai4ci_collab_talk_short` | 8 | 1 of 15 | yes, same |
  | `ai4ci_collab_talk_v1` | date only | 1 of 38 | no |
  | `brighton_ai_talk` | 0 | 1 of 29 | no |
  | `nov_2026_talk` | 0 | 1 of 29 | no |

  **The three "no" decks were not affected by the sibling tier at all.**
  Their one changed figure is label jitter, not data: rendering
  `nov_2026_talk` twice from identical source and identical data produced a
  different image again. The `ggrepel::geom_text_repel` calls on that chunk
  (`brighton_ai_talk.qmd:967`, `ai4ci_collab_talk_v1.qmd:855`) pass no
  `seed`, unlike the seeded calls in `bh_schools_talk_2026_v2.qmd`. **So any
  render of those decks dirties their HTML in git for nothing.** Adding a
  `seed` would make them reproducible; not done. That slide's prose is
  hard-coded ("1,900 children in 2010 and 2,250 in 2026", "Longhill fell
  from 239 to 81") and all four were checked against the current
  `factsheet_panel.rds`: still exact.

  `bh_schools_talk_2026`'s prose was scanned for the trap that caught `_v2`
  and is clean; its literals are structural. One latent risk left alone:
  its line 159 hard-codes the attainment figures "+5.7" and "+3.1" GCSE
  points. Correct today, because `R/00_fetch_data.R` does not rebuild the
  attainment models, but they would go stale silently if those are refitted.

  **The Varndean slide in `ai4ci_collab_talk` and `_short` was rewritten.**
  Its prose was computed and so updated itself correctly, but the claims
  around it had become false: it asserted "the model has no priority 3",
  which `../bh-school-system/app/R/model.R:218` now contradicts outright,
  and headed the slide "why no parameter fixes it" when the sibling tier
  had just fixed it. Out-of-catchment R² went 0.00 to 0.74 and Hove Park
  (-10.0) replaced Varndean (+4.8) as the largest error. It now runs the
  model both ways -- `without_siblings()` reproduces the published figures
  exactly -- and tells it as what it was: the model's largest error pointed
  at a missing rule, and adding the rule closed it. Nothing in that slide
  is typed in, so it moves with the model from here.

  **Dates.** `ai4ci_collab_talk`, `_short` and `_v1` carried `date: today`,
  so re-rendering silently re-dated talks delivered on 16 and 15 October to
  the day they were rebuilt. All three are now pinned to their delivery
  dates, in the style the `bh_schools_talk_2026*` decks already used.

  Separately, `docs/` holds published copies that are copied in, not
  rendered in place: `docs/bh_schools_talk_2026_v2.html` is dated
  2026-09-28 and so is behind even the old root HTML. **Nothing has been
  published.**
- The render deleted six tracked PNGs under
  `bh_schools_talk_2026_v2_files/figure-revealjs/`. They are leftovers from
  an older non-embedded render, committed in `5a44328` and unreferenced by
  the self-contained HTML ever since, so Quarto cleaning them is correct.
  Let the deletion stand.

## Step 5 — finish the suspension change on the main machine

**Done 2026-11-02 on the main machine.** `data-raw/ees/` already held the
2024/25 exclusions file (the latest published), so no download was needed.
The rebuild of all sixteen area files changed nothing but the new `excl`
columns. England came out at 7.30 pupils per 100 suspended at least once,
2.90 times each (21.2 episodes); East Sussex 8.82, 3.17 times (28.0). The
England comparator is now in the suspensions prose of `east_sussex`,
`west_sussex`, `mid_sussex`, the district template and the review's
practice bullet, and in the England row of the district summary's table.
All documents re-rendered and published. The original note follows.

Added 2026-11-02 from the laptop, after a question about the Sussex review:
is 28 suspensions per 100 pupils in East Sussex 28% of children? It is not.
The DfE file counts suspension *episodes*, and one pupil can be suspended
several times in a year — East Sussex's 28.0 episodes per 100 in 2024/25 are
2,541 pupils, 8.8 per 100, suspended about 3.2 times each.

Commit `70d13df` reports it that way throughout: every column, axis and
caption says "suspension episodes per 100 pupils", and wherever a document
quotes a rate it also gives the pupils suspended at least once per 100 and
the mean episodes each. `AREA_REPORTS.md` records the convention. The
documents are done, rendered and published.

**The pipeline half of that commit has never run.** `excl_agg()` and
`excl_la_ranks()` in `R/17_area_analysis.R` now carry `pupils`, `pupil_rate`
and `susp_per_pupil`, so England and every authority would get the pupil
measure across all years. Nothing has exercised that code: `data-raw/ees/`
does not exist on the laptop, so `area_*.rds` cannot be rebuilt here, and
every file in `data/` predates the change. Until a rebuild the documents take
their pupil figures from each area file's `excl_schools` table, which holds
`one_plus` and reproduces the area totals exactly — but only for the area's
own schools, which is why **no England pupil-level comparator appears in the
prose yet**.

On the main machine:

1. `ees_download("exclusions_school")` from `R/00b_download_ees_api.R` if
   `data-raw/ees/` is stale.
2. `Rscript R/17_area_analysis.R` — rebuilds every `data/area_<slug>.rds`.
3. Check `X$excl$area` has `pupil_rate`. England should come out near 7.3
   pupils per 100 suspended at least once in 2024/25 (the figure from
   `data/exclusions_absence.rds`, which covers slightly fewer schools), with
   East Sussex at 8.8, against 21.2 and 28.0 on episodes.
4. Put the England comparator into the prose that already quotes the area's
   own pupil rate: the suspensions sections of `east_sussex.qmd`,
   `west_sussex.qmd` and `templates/district_report.qmd`, the practice bullet
   of `sussex_review.qmd`, and the England cell of the "Pupils suspended per
   100" column in `sussex_districts.qmd`, which is blank today for want of
   the figure.
5. Re-render and publish: the four hand-written documents
   (`east_sussex`, `west_sussex`, `sussex_review`, `mid_sussex`), each
   through `mobile_safe()` and into `docs/`, then
   `Rscript R/20_district_reports.R` for the eleven templated districts and
   the summary.

The rebuild also settles whether anything else in the area files moved, since
they were last built before the URN and district work.

## Step 6 — Ofsted: date the ratings, and handle the end of single grades

**The dating is done (2026-11-02, main machine).** What the audit found:

- The performance tables' school information file carries Ofsted only up
  to 2022/23. For 2023/24 and 2024/25 the panel's rating comes from
  Ofsted's statistics *as at 31 August 2024* (`OFSTED_URL`, via
  `02_build_panel.R`), which is why BACA still shows its March 2023
  Requires Improvement, and why no rating in the panel is later than
  July 2024.
- `R/02c_ofsted_dates.R` (new) adds `OFSTEDLASTINSP` and `OFSTED_SOURCE`
  to `panel_data.rds` and `panel_data_5yr.rds` without rebuilding them,
  attaching a date only where the source's rating matches the panel's.
  Every rated school-year in 2021/22 to 2024/25 is dated; no rating
  changed, so no model moves.
- `R/17_area_analysis.R` carries it as `ofsted_date` in each area's
  school table. The map's dropdown now reads "Ofsted rating, last graded
  inspection before September 2024"; the hover gives the rating and its
  inspection month; the school tables show "Good (Mar 2023)" with a
  caption saying schools inspected since may have newer judgements.
- Several ratings are very old: Blatchington Mill's and Hove Park's
  Good come from graded inspections in 2013, the last graded inspection
  each had on record at August 2024.

**Representation, decided and built 2026-11-02.** Adam chose: keep the
dated old grade as the model term, and add a "latest Ofsted" field for
the map and tables, taken from the monthly management information in this
order: report card (achievement and curriculum grades), else an
inspection with no overall grade (quality of education), else the last
graded inspection (plus any later ungraded outcome), else the latest
ungraded outcome for schools last graded before 2019.
`R/15b_ofsted_latest.R` builds it (also a step in `R/00_fetch_data.R`);
`add_ofsted_latest()` in `R/17_area_analysis.R` joins it, following URN
changes, and all 76 Sussex schools have one. `AREA_REPORTS.md` records
the convention. Refresh monthly with `Rscript R/15b_ofsted_latest.R`, then
rebuild the area files. The findings that led there:

**Background.** Checked 2026-11-02 (not from
memory): since November 2025 Ofsted grades each evaluation area on a
five-point scale (Exceptional, Strong standard, Expected standard, Needs
attention, Urgent improvement), with safeguarding met / not met, on a
report card. Only about one school in nine had a report card by
September 2026. The live source is Ofsted's monthly management
information, "State-funded school inspections and outcomes", which has
one row per school with URN (latest file as at 30 September 2026; next
due 11 November 2026). The September 2024 to November 2025 inspections,
which gave sub-judgements on the old four-point scale with no overall
grade, sit between the two schemes. The questions below stand; the model
term can stay on the dated legacy grade for 2021/22 to 2024/25 whatever
is chosen for the map and text.

**The source question is answered (checked 2026-11-02).** The monthly
"latest inspections" management information CSV (one row a school, URN,
~16 MB, published mid-month) holds every scheme side by side:

- report-card grades for up to eight areas (inclusion, curriculum and
  teaching, achievement, attendance and behaviour, personal development
  and wellbeing, leadership and governance, early years, post-16), plus
  safeguarding met / not met, with the inspection date;
- the latest graded inspection under the old framework: overall
  effectiveness 1 to 4, or "Not judged" for September 2024 to November
  2025, with its four sub-judgements and date;
- the latest ungraded inspection and its outcome ("School remains Good").

The twice-yearly official statistics (latest as at 31 March 2026) have the
same layout plus a date for each report-card area. As at 30 September
2026, 685 of 3,423 secondary schools had a report card and 579 had a "Not
judged" inspection. It confirms Adam's examples: BACA was inspected on
23 April 2025, "Not judged" overall with quality of education Good;
Longhill has a June 2026 report card, "Needs attention" for achievement,
curriculum and attendance. 70 of the 76 Sussex schools match on URN; the
other six will need the GIAS successor links, as the absence files did.

Raised 2026-11-02 by Adam, from the Sussex map. Two problems, one small and
one that needs a decision.

### The ratings are stale and nothing says when they are from

The map's "Ofsted rating" (`map_vars()` in `R/area_report_helpers.R`) carries
no date, and the ratings behind it are out of date. Brighton Aldridge
Community Academy (URN 136164) shows **Requires Improvement**; Adam reports
it was inspected in April 2025 and came out Good across the areas it was
graded on. Longhill High School (URN 114581) also shows Requires Improvement
and has been inspected recently, with a mixed picture across areas.

The rating comes from `OFSTEDRATING_1`, derived in `R/01_extract_data.R` from
the DfE performance tables' school information file, so it is a snapshot as
at that file's cut-off, not as at today. `OFSTEDLASTINSP` is parsed there
(and in `R/20_extract_ks2.R`) but **dropped before `panel_data.rds`**, which
has only `OFSTEDRATING` and `OFSTEDRATING_1`. Carrying the inspection date
through the panel is the cheapest fix and is what lets the map and the
documents say when a rating is from, per school rather than in general.

Smallest useful change, whatever is decided below: keep `OFSTEDLASTINSP` in
the panel, put the inspection date in the map hover beside the rating, and
date the dropdown label the way every other map variable is dated.

### Single-word grades have gone, and the data model assumes them

Ofsted no longer gives state schools a single overall grade; a school is
graded separately across several areas (quality of education, behaviour and
attitudes, personal development, leadership and management, plus sixth form
and early years where they apply), and the framework has moved again since.
**Check the current framework, the evaluation areas and the band names
before building anything** — this note is written from memory and the
details matter.

The repo assumes one grade on four bands in several places:

- `MAP_CATS$ofsted` in `R/area_report_helpers.R`: Outstanding / Good /
  Requires Improvement / Inadequate, with a colour each.
- `OFSTEDRATING_1` is a four-level factor with treatment contrasts
  (`R/17_area_analysis.R`, around lines 99–107 and 537).
- It is a **model term**, not just a label: `(1 | OFSTEDRATING_1)` in the
  area models, and "Ofsted" is one of the decomposition components every
  area document reports. So this is not only a map question — changing the
  representation moves the decomposition, and `AREA_REPORTS.md` records that
  component.

Sussex today, on the stale snapshot: 53 Good, 12 Outstanding, 9 Requires
Improvement, 2 Inadequate.

### What needs deciding

1. **Source.** The performance tables' school information file is a lagging
   snapshot and carries one grade. Ofsted's own inspections and outcomes
   management information is the live source and should carry inspection
   dates and the area judgements. Check what it actually publishes now, and
   whether it is joinable on URN.
2. **Representation.** With no overall grade, the options are roughly: keep
   a legacy grade for schools last inspected under the old framework and
   mark the rest "not graded overall"; take one area (quality of education)
   as the headline; or build a composite. Each is defensible and each
   changes the model term. A school like BACA, graded the same across all
   its areas, is easy; one graded unevenly across areas is the case that
   decides the design.
3. **Mixed vintages.** Schools inspected before and after the change cannot
   share a scale. The models cover 2021/22 to 2024/25, so for the model
   years a legacy grade is mostly still the right variable; it is the map
   and the current-state text where the new judgements belong. Dating every
   rating may be most of the fix for the documents, with the model term left
   on the legacy grade and the limitation stated.
4. Whether to show the areas at all in the map hover, or only in the school
   tables.

Start with the date — it is small, it is right whatever else is decided, and
it is what makes the staleness visible rather than silent.
