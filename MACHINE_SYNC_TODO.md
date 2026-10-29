# TODO: make the repo runnable on a fresh clone

Originally written 2026-09-28 from the laptop; revised 2026-10-29 on the
laptop (`C:\GitHubRepos`) after auditing the actual state. Delete this file
(and the pointer in `CLAUDE.md`) once step 2 is done.

## Status

| Step | State |
|---|---|
| 1. Sibling-repo layout | Works. `bh-school-system`, `QM_Fork` and `BH_Schools_Consultation` are all present as siblings. |
| 2. Missing data | **Outstanding — needs a decision, see below.** |
| 3. Hard-coded `E:` paths | Done. |
| 3b. Cross-repo reads | Partly done; `optionZ_Mar25.geojson` is present but still git-ignored. |
| 4. Render check | Not run — blocked on step 2. |

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
