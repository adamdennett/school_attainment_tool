# Supplementary materials

Anonymised HTML supplementary materials accompanying the manuscript
submission to peer review.

The footnotes in the manuscript reference specific files in this
supplementary archive (e.g. *"see `model_experiments.html`"*).
This README describes how to access them.

## How to view

OSF cannot render interactive HTML files in its built-in preview.
To view the supplementary material:

1. Download **`supplementary.zip`** from the Files list on this OSF project page.
2. Extract the archive on your local machine.
3. Open **`index.html`** in a modern web browser (Chrome, Firefox, Edge, Safari).

The other HTML files are cross-linked from `index.html` and from each
other. Interactive plots and filters (plotly + crosstalk) require a
real browser environment — they will not render in OSF's built-in
viewer, which shows raw HTML source only.

## Files inside `supplementary.zip`

| File | Description |
|---|---|
| `index.html` | Project overview and navigation |
| `data_overview.html` | Descriptive statistics and data preparation |
| `model_results.html` | Full multilevel model fits and diagnostics |
| `model_experiments.html` | Sensitivity analyses, robustness checks, the two-stage absence decomposition, and the reviewer-response diagnostics |
| `about.html` | Background notes on the earlier analysis project |
| `absence.html` | Earlier descriptive analysis of city-level absence and the Gorard Segregation Index |
| `absence_files/figure-html/` | Image assets for `absence.html` |

## Reviewer-response diagnostics

The revised manuscript references three additional analyses, all in
`model_experiments.html` under **Reviewer-response diagnostics**:

- **Prior-attainment robustness** — refits adding mean KS2 scaled score
  on the three-year sub-sample where it is published.
- **Ofsted specification** — random effect versus categorical fixed
  effect, with per-band coefficients.
- **Functional-form diagnostics** — AIC comparison of logged, linear and
  spline treatments, plus component-plus-residual plots.

## Policy Simulator tool

References to the live Policy Simulator tool (an interactive R Shiny
web application developed as part of this work) have been withheld
from this anonymised version. Tool details, a walkthrough and access
can be made available on request from the corresponding author via
the editorial office.
