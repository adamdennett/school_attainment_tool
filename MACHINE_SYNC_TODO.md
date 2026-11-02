# TODO: make the repo runnable on a fresh clone

Originally written 2026-09-28 from the laptop; revised 2026-10-29 on the
laptop (`C:\GitHubRepos`) after auditing the actual state, and 2026-11-02 to
add steps 5 and 6. Delete this file (and the pointer in `CLAUDE.md`) once
steps 2, 5 and 6 are done.

## Status

| Step | State |
|---|---|
| 1. Sibling-repo layout | Works. `bh-school-system`, `QM_Fork` and `BH_Schools_Consultation` are all present as siblings. |
| 2. Missing data | **Outstanding — needs a decision, see below.** |
| 3. Hard-coded `E:` paths | Done. |
| 3b. Cross-repo reads | Partly done; `optionZ_Mar25.geojson` is present but still git-ignored. |
| 4. Render check | Not run — blocked on step 2. |
| 5. Suspension pupil fields | **Outstanding — written but never run; needs a rebuild on the main machine, see below.** |
| 6. Ofsted ratings | **Outstanding — stale, undated, and still single-word; needs a design decision, see below.** |

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

These hit the EES/DfE APIs, so they need network access and take a while. The
natural next step is a thin `R/00_fetch_data.R` that runs whichever of these
have missing outputs — deliberately **not** written yet, because it cannot be
tested from this machine without running the full fetch.

### Not rebuildable — the one actual blocker

`data/cache/school_effect_decomp.rds` is the only file nothing in the repo
creates. Every reference to it is a `readRDS`:

- read by `bh_schools_talk_2026.qmd`, `bh_schools_talk_2026_v2.qmd`,
  `pulling_the_right_lever_slides.qmd` and `..._slides_v2.qmd`
- `R/scratch_add_nondisadvantaged_decomp.R` **reads it, augments it and
  writes it back** — it adds `decomp_non` to an existing cache, so it cannot
  recreate one from nothing

So it exists only on the main machine, and four decks fail without it.
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
`models*.rds` set, `data/cache/` with the workforce/finance/Ofsted extracts)
but **none** of `data/census/`, `data/early_years/`, `data/engagement/`,
`data/gias/`, `data/ks2/`, `data/ks5/`, `data/place/`, which do not exist here
at all. `school_effect_decomp.rds` is also absent.

So step 2 cannot be completed from the laptop. It needs the main machine
(the `E:` one), which is where those files live.

## Step 4 — check it

Once step 2 is resolved, pull on a fresh clone and run:

```
quarto render bh_schools_talk_2026_v2.qmd --to revealjs
```

## Step 5 — finish the suspension change on the main machine

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
