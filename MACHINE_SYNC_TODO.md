# TODO on the main machine: make the repo runnable on a fresh clone

Written 2026-09-28 from the laptop. Delete this file (and the pointer in
`CLAUDE.md`) once everything below is done.

## Why

On a fresh clone (the laptop, `C:\GitHubRepos`), the talk decks fail because:

1. **Hard-coded `E:` paths.** Partly fixed: the decks now find `bh-school-system`
   as a sibling folder of this repo, under either name (`bh-school-system` or
   `bh_school_system`). Two `E:` paths remain (see step 3).
2. **Git-ignored data.** Everything under `data/` that the decks read is in
   `.gitignore`, so it never reaches GitHub. Some of it can't be rebuilt from
   anything in the repo: `data/cache/school_effect_decomp.rds` was built by hand.

## Steps

### 1. Check the sibling-repo layout still works here

`bh_schools_talk_2026_v2.qmd` should render on this machine with
`E:/bh_school_system` sitting next to `E:/school_attainment_tool`. If it
doesn't, fix the `BHS <-` line in the decks rather than going back to an
absolute path.

### 2. Get the missing data onto GitHub

These files are read by the `.qmd` files but were missing on the laptop:

```
data/cache/analysis_f_results.rds
data/cache/oa_pwc_2021.rds
data/cache/school_effect_decomp.rds      <- no script builds this; top priority
data/census/school_census_context.rds
data/early_years/la_entry_baselines.rds
data/engagement/absence_by_yeargroup.rds
data/engagement/suspensions_by_yeargroup.rds
data/engagement/suspensions_school.rds
data/gias/gias_establishments.rds
data/gias/school_closures.rds
data/ks2/ks2_panel.rds
data/ks4_destinations.rds
data/ks5/ks5_panel.rds
data/la_boundaries.rds
data/place/absence_residence_idaci.rds
data/place/pupil_projections.rds
data/place/pupil_yield.rds
```

The laptop already had `data/panel_data.rds`, `data/models_imputed.rds` and
`data/r27_la_absence.rds` (copied over by hand), so they may need the same
treatment for the next machine.

Check sizes first:

```r
f <- c("data/cache/school_effect_decomp.rds", ...)  # list above
data.frame(f, MB = round(file.size(f) / 1e6, 1))
```

Then, for each file:
- **Small (under ~50 MB) and not embargoed:** commit it. Add a
  `!path/to/file.rds` rule to `.gitignore`. Git can't re-include a file whose
  parent folder is ignored, so for `data/cache/` etc. change the rule from
  `data/cache/` to `data/cache/*` first.
- **Large:** upload as a GitHub Release asset (e.g. `piggyback::pb_upload()`),
  and add a `R/00_fetch_data.R` that downloads whatever is missing.
- **Embargoed / pre-release:** leave it out, and add a comment in `.gitignore`.

Remember the repo is **public**.

### 3. Remove the last `E:` paths

- `R/01_extract_data.R:38`: `E:/QM_Fork/sessions/L6_data/...`
- `output/brighton_case_study.qmd:2805`: `E:/BH_Schools_Consultation/data/optionZ_Mar25.geojson`

Point these at sibling repos in the same way (`dirname(here::here())`), or
copy the data into this repo.

### 4. Check it

Pull on the laptop and run
`quarto render bh_schools_talk_2026_v2.qmd --to revealjs`.
