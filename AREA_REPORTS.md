# Area reports: the format

This file is the record of how the area documents are laid out, so the
format can be changed in one place and the change carried to every
document. To change something, edit the relevant section below, then ask
for it to be applied ("apply AREA_REPORTS.md to all the area documents").
Whoever applies it should update the documents, the template and this
file together, so they never disagree.

The documents apply the national models of *How to Pull the Right Lever*
to a place. Everything is built from published DfE data; nothing uses or
hints at pupil-level records.

## The tiers

| Tier | Area | Document | Source | How it is made |
|---|---|---|---|---|
| Top | Sussex as a whole | `sussex_review.html` | `sussex_review.qmd` | written by hand |
| Upper | East Sussex | `east_sussex.html` | `east_sussex.qmd` | written by hand |
| Upper | West Sussex | `west_sussex.html` | `west_sussex.qmd` | written by hand |
| Upper | Brighton and Hove | `pulling_the_right_lever_v2.html` | the Lever report | (the report itself) |
| Lower | Mid Sussex | `mid_sussex.html` | `mid_sussex.qmd` | written by hand (the model for the template) |
| Lower | every other district of East and West Sussex | `district_<name>.html` | `templates/district_report.qmd` | generated, one per district |
| Lower | all districts together | `sussex_districts.html` | `sussex_districts.qmd` | written by hand, all figures computed |

"Upper tier" means an authority that runs schools (a county or a unitary
authority). "Lower tier" means a district within a county: it does not
run schools, so on the national models its schools take the county's
effect, and what is particular to it shows up in its schools' residuals
and in the district test.

Which areas exist at which tier is set in **`R/area_tiers.R`**
(`DISTRICTS`, `HAND_WRITTEN`). The top and upper tier areas are listed in
`AREAS` in `R/17_area_analysis.R`.

## Building

```
Rscript R/18_district_effects.R          # district + school effects (slow; rarely rerun)
Rscript R/19_trust_effects.R             # trust effects (slow; rarely rerun)
Rscript R/19b_sussex_boundaries.R        # district boundaries
Rscript R/17_area_analysis.R [slug ...]  # data/area_<slug>.rds for each area
quarto render <doc>.qmd                  # a hand-written document, then mobile_safe()
Rscript R/20_district_reports.R [slug]   # every templated district + the summary
```

`R/20_district_reports.R` writes `district_<slug>.qmd` from the template,
renders it, runs `mobile_safe()` (`R/16_mobile_safe_html.R`) and copies the
HTML to `docs/`. The generated `.qmd` files are not committed: the
template is the source. Hand-written documents are rendered with
`quarto render`, then `mobile_safe()`, then copied to `docs/` by hand.

Shared code lives in **`R/area_report_helpers.R`**: colours, number
formats, ranks, the map, and every chart and table used in more than one
document. A change there reaches every document on the next render.

## Conventions for every document

- **Front matter.** Title "Attainment and Attendance in <area>"
  ("across Sussex" for the top tier); a subtitle ending
  "**Draft for discussion.**"; author Adam Dennett, CASA, UCL;
  `embed-resources: true`, code folded, `ucl-theme.scss`.
- **Callout first.** A "Draft for discussion" callout says which documents
  this one sits beside or beneath, links them, and states that every
  figure is from published DfE data and that nothing uses pupil-level
  records.
- **Summary before section 1.** Unnumbered. A bold one-sentence finding,
  then short bullets with the headline numbers, then "The other
  findings" as bold-led bullets.
- **Plain, short sentences.** Bullets over long paragraphs. Every number
  in the text is computed inline from the data, never typed.
- **Years.** The model covers 2021/22 to 2024/25 (four complete years).
  Value added adds provisional 2025/26. Absence runs a year ahead:
  autumn and spring terms, latest 2025/26. Write years as 2024/25.
- **Ranks.** Local authorities are ranked among all authorities. Districts
  are ranked among the districts in England with five or more schools; a
  smaller district is placed among them and the text says so
  (`district_rank()`). 1 = highest; for gaps, 1 = widest.
- **Units.** Attainment 8 points for results and decompositions;
  percentages for model effects (`pct()`); percentage points for absence
  against intake.
- **Comparators.** A county is set against its neighbour county,
  Brighton and Hove and England. A district is set against its county,
  Brighton and Hove and England.
- **Colours.** `AREA_COL` for areas; teal good / red bad on diverging
  scales; orange for disadvantaged and teal for non-disadvantaged pupils.
- **The map** (`map_area()`): schools within district boundaries, a menu
  of 22 measures above it (absence first and default), circle size =
  roll, hover for figures and who runs the school.
- **Naming schools.** Schools are named, including those below
  expectation. (Open question: whether to soften this before documents
  go to councils.)
- **Ending.** "Implications and questions for discussion" (or "Questions
  for discussion" in the templated districts), then an unnumbered "Notes
  on method" naming the scripts that build the document.
- **Publishing.** Render, `mobile_safe()`, copy to `docs/`, add or update
  the card in `docs/index.html`, commit and push.

## Top tier: Sussex review

1. Summary
2. About this review: Sussex is a lens, not a governing body; Brighton
   and Hove's figures come from the Lever report's models.
3. Sussex's schools: map (all 76), who runs them by authority.
4. Three systems in one region: profile table (Sussex, three authorities,
   England, with LA ranks); model table (LA effects, absence control,
   absence against intake); a bullet per authority.
5. What explains attainment across Sussex: decomposition for Sussex and
   the three authorities; table for Sussex.
6. Place: authorities and districts: LA caterpillar; value added by
   district; district table; links to the district summary.
7. Trusts across Sussex.
8. Absence across Sussex: series, reasons, practice table.
9. School by school: joint-signal plane, top and bottom ten.
10. Implications for Sussex.

The top tier compares the three authorities only. Districts appear in
the map, the district plot and table, and through the link to the
district summary; they do not get lines of their own in the authority
comparisons.

## Upper tier: county documents

`east_sussex.qmd` and `west_sussex.qmd` follow the same order:

1. Summary
2. About this analysis (models, years, what they cannot do)
3. The county's schools: map, school table; who runs them (governance
   chart, changes table)
4. How the county compares: profile table and charts against its
   comparators
5. What explains attainment in the county: decomposition
6. The county's own effect: LA effect and rank; do the districts differ
   (district test); do trusts make a difference (trust level)
7. Absence: ten-year series; is it the intake; what the absence is
   (reasons); within the county; suspensions
8. What absence costs, school by school: joint-signal plane; scenarios
9. Contextual league tables (all, disadvantaged, non-disadvantaged)
10. The county against its comparators
11. Implications and questions for discussion

## Lower tier: district documents

The template, `templates/district_report.qmd`, follows the Mid Sussex
document. Placeholders `@@DISTRICT@@`, `@@COUNTY@@` and `@@SLUG@@` are
filled by `R/20_district_reports.R`; everything else is computed from
`data/area_<slug>.rds`.

1. **Summary.** Lead sentence: results "among the strongest / above the
   middle / below the middle / among the weakest" of England's districts
   (by quarter of rank), on an intake more or less disadvantaged than
   England's; the towns. Four headline bullets (Attainment 8 and rank,
   disadvantage, disadvantaged pupils' results and rank, the gap and
   rank). Then the other findings:
   - the two largest decomposition pieces for all pupils and for
     disadvantaged pupils, and what the schools add or take;
   - the district test (none / disappears with a school term / survives);
   - how many schools are above expectation, the strongest and weakest;
   - absence now and a year earlier, against England and the county, and
     against intake, naming schools more than a point above;
   - reduced timetables and suspensions as multiples of the national rate;
   - the size of disadvantaged groups.
2. **About this analysis.** District, not authority; the LA effect is the
   county's; adds a note on ranks if the district has fewer than five
   schools.
3. **The district's schools.** Map; school table with disadvantaged Year
   11 size; who runs each academy (trust, sponsored, moved since);
   governance chart.
4. **How the district compares.** Profile table against county, Brighton
   and Hove and England; gap chart for the county's districts with this
   one in bold; the gap over the years.
5. **What explains attainment.** Decomposition chart and table; every
   piece of at least 0.25 points listed for each group. The "disadvantage
   counts against disadvantaged pupils" paragraph appears only when that
   piece is below −0.25 and the district is less disadvantaged than
   England.
6. **The disadvantaged gap, school by school.** Value added for the two
   groups; schools above expectation for others and more than a point
   below for disadvantaged pupils; schools above for both; each school's
   own effect (model with a school term); disadvantaged results by year.
7. **Is there a district effect?** Table and a sentence chosen by the
   three cases; school effects chart.
8. **Absence.** Series; bullets; table against intake with reasons;
   schools more than a point above or below prediction; reduced
   timetables and the highest school; scenario table; suspensions.
9. **Contextual league tables.**
10. **Questions for discussion**, chosen by rule:
    - disadvantaged pupils (always), naming the schools above
      expectation for them if any;
    - attendance "is a lever here" if absence costs more than 1 point or
      schools run more than 1 point above intake; "is a modest lever"
      above 0.3 points or 0.5 above intake; otherwise "is not the main
      lever";
    - schools, not the place / something the schools share (by the
      district test);
    - practice, if reduced timetables are over 1.5 times or suspensions
      over 1.3 times the national rate;
    - the county effect for disadvantaged pupils.
11. **Notes on method.**

The thresholds above are in the template; change them there and here
together.

### District summary (`sussex_districts.qmd`)

1. Summary: range of results; disadvantaged pupils below England; the
   widest gaps; which district effects survive; absence change; range of
   practice.
2. The tiers of this analysis, with a link to every district document.
3. The districts side by side: profile table with ranks and value added;
   gap chart faceted by county.
4. What explains each district's results: decomposition as a heat map
   (pieces grouped: absence, prior attainment, disadvantage, other
   intake, Ofsted, place, other, schools).
5. Is there a district effect: effects chart, and a line per district
   with an effect.
6. Absence and practice: table and series by county.
7. District by district: a short computed paragraph and a link for each.

## Applying the format elsewhere in England

The method is general; some of the pipeline is written for Sussex. To
cover another county:

1. **Tiers.** Add the county and its districts to `DISTRICTS` in
   `R/area_tiers.R`, and the county (and any top-tier grouping) to
   `AREAS` in `R/17_area_analysis.R`. Choose its comparators
   (`comps`): Brighton and Hove is hard-coded as the comparator for the
   Sussex districts.
2. **Districts for each school.** `school_lad()` in `R/17_area_analysis.R`
   places schools outside the model's rows by postcode only for the
   three Sussex authorities; widen that list.
3. **District and school effects.** `R/18_district_effects.R` fits every
   district in England but keeps the detailed outputs (`focus`,
   `school_eff`, `sussex_va`) for the Sussex authorities only (`SUSSEX`).
   Widen it.
4. **Trusts.** `R/19_trust_effects.R` keeps who runs each school
   (`sussex_now`, `sussex_years`) for the Sussex authorities only
   (`SUSSEX`). Widen it.
5. **Boundaries.** `R/19b_sussex_boundaries.R` keeps the 13 Sussex
   districts; add the new ones (the ONS service covers the UK).
6. **Region.** The absence series compares with the South East; use the
   county's region instead (`absence_agg()` in `build_area()`).
7. **Documents.** The district template needs no change. Upper and top
   tier documents are hand-written: copy the nearest one and follow the
   section order above.

Once a standard format is settled, the upper tier could be templated in
the same way as the districts.
