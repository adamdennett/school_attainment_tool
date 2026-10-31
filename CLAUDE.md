# school_attainment_tool

**Outstanding:** read `MACHINE_SYNC_TODO.md` first. There are setup fixes to make
on the main machine (where the ignored data lives) so the repo runs from a
fresh clone. Mention it to the user at the start of a session until it's done.

- The Sussex area documents (Sussex review, county, district and
  district-summary documents) follow `AREA_REPORTS.md`. When asked to change
  their format, change the documents, `templates/district_report.qmd` and
  `AREA_REPORTS.md` together, then re-render (`R/20_district_reports.R` for
  the districts).
- Other repos are expected as sibling folders (e.g. `../bh-school-system`).
  Never use absolute drive paths like `E:/...`; use `here::here()` and
  `dirname(here::here())`.
