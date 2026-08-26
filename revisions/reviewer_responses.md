# Reviewer responses — tracking document

**Manuscript:** *How to Pull the Right Lever: School Attainment, Open Data Analytics and Local Education Policy in England*
**Decision:** Minor revisions
**Journal:** Research Papers in Education (RRED) — *not "JEP"; correct in the response letter*
**Source file:** ⚠️ **SEE [`SOURCE_VERSION_NOTE.md`](SOURCE_VERSION_NOTE.md)** — the submitted text is in `Manuscript_Anonymous.docx`, which was **hand-edited after the last render of `JEP_Paper_tight.qmd`** (~27% of body sentences differ; abstract and intro rewritten). Reconcile the qmd to the submitted text **before** editing. Model tables/figures still come from the qmd + `output/model_experiments.qmd` pipeline.

**Progress: ALL 14 prose items applied to RPE_Paper.qmd. Outstanding: add R2.2/R2.3/R2.4 diagnostics to the supplementary (model_experiments.qmd) so the referenced tables/plots exist; then final render + response letter.**

| | Reviewer 1 | Reviewer 2 | Minor | Total |
|---|---|---|---|---|
| Points | 2 | 9 | 3 | 14 |
| Done | 0 | 0 | 0 | 0 |

---

## Priority triage

Work in this order — the first two change results, the rest are writing.

| Priority | Item | Why |
|---|---|---|
| **1** | **R2.7** — B&H LA effect with/without absence control | Reviewer calls it "pivotal for the overall interpretation". Could soften the headline "7th best" claim. Needs new model runs. |
| **2** | **R2.3 / R2.1** — mean KS2 prior attainment control | Goes to whether estimates are inflated. We *do* have the data (see below). Needs a robustness run. |
| 3 | R1.1 — clarify selective admissions variable | Easy factual fix, but reviewer flags it as blocking interpretation. |
| 4 | R2.4, R2.2 | Methodological justification; may need refits/diagnostics. |
| 5 | R1.2, R2.6, R2.8, R2.9 | Discussion/framing — writing only. |
| 6 | R2.5 | Partly already done, needs signposting. |
| 7 | Minor points | Trivial. |

---

# Reviewer 1

## R1.1 — Clarify what the "selective admissions" variable captures

- [x] **Done** — RPE_Paper.qmd §3.2 (three-group ADMPOL_PT clarification + faith-school note)

> *"greater clarity is needed regarding the inclusion of a variable labelled 'selective admissions'. Given that independent schools are excluded from the analytical sample, it is unclear what this variable captures in practice within the state-funded sector. The authors should explain whether this refers to grammar schools, partial selection, banding arrangements, aptitude-based selection, or faith-based oversubscription criteria, and indicate how prevalent such schools are within the sample."*

**Type:** Writing (factual clarification) · **Effort:** ~30 min · **Files:** `JEP_Paper_tight.qmd` §3.1 Data, §3.2 Model specification

**What the variable actually is.** `ADMPOL_PT` is the DfE performance-tables admissions classification, which has three levels. I've verified the counts in our analytical sample:

| Level | Meaning | School-years | Unique schools | % of schools |
|---|---|---|---|---|
| `SEL` | Wholly selective — **state-funded grammar schools** (11+ entrance test) | 650 | 166 | 4.8% |
| `NON SEL IN HIGHLY SEL AREA` | Non-selective schools located in selective areas (e.g. Kent, Buckinghamshire, Lincolnshire) — the **reference category** | 850 | 234 | 6.8% |
| `OTHER NON SEL` | All other non-selective schools | 11,517 | 3,055 | 88.4% |

**Suggested fix.** Add 3–4 sentences to §3.1 or §3.2 stating:

1. This is the DfE's own classification, not our construction — cite the performance tables metadata.
2. `SEL` = wholly selective state grammar schools (166 schools, 4.8% of the sample); it does **not** capture partial selection, banding, aptitude-based selection, or faith oversubscription criteria — those schools sit in `OTHER NON SEL`.
3. The reference category is deliberately *non-selective schools in highly selective areas*, so the selective coefficient is interpreted against the most appropriate comparator (schools whose intake is shaped by the same local grammar system) rather than against all non-selective schools nationally.
4. Note the limitation explicitly: because banding and partial selection are folded into `OTHER NON SEL`, our estimate is of the **fully selective** effect only, and any milder forms of selection are unmeasured.

**Also worth adding:** a sentence noting faith schools are not separately identified in the main model. (We tested a faith interaction in the supplementary — see `model_experiments.qmd` Analysis F — and it was negative but non-significant/underpowered at ~18% faith schools. Could cite that.)

---

## R1.2 — Engage more explicitly with FSM + absence as strongest predictors, and link to the admissions changes

- [x] **Done** — RPE §4.3 (ranking-clarity), §6.1 (two-pathway mechanism chain + expected-vs-observed caveat)

> *"The results indicate that FSM eligibility (%) and school absence rates are the strongest predictors... this finding aligns with existing evidence... and warrants more explicit engagement in the paper. In particular, the conclusion could reflect more directly on these findings and clarify how, if at all, the observed attainment patterns are linked to the post-2022 admissions policy changes in Brighton & Hove. Greater discussion of the mechanisms through which admissions reforms might interact with FSM composition and attendance would strengthen the interpretation."*

**Type:** Writing / framing · **Effort:** ~2 hours · **Files:** `JEP_Paper_tight.qmd` §6 Discussion, §7 Conclusion

**Note a possible misreading to address.** The reviewer says "FSM eligibility (%) and school absence rates are the strongest predictors" — but one of our central findings is that FSM is a *weaker* lever than commonly assumed once absence and prior attainment are controlled, and that for disadvantaged pupils specifically the FSM concentration coefficient *flips sign*. Either the reviewer has read the standardised-importance figure as putting FSM alongside absence at the top, or our presentation isn't making the ranking clear enough. **Treat this as a signal that the relative-importance message needs sharpening**, not just expanding.

**Suggested fix — three parts:**

1. **Sharpen the ranking statement** in §4.3. Make explicit that absence dominates, prior attainment is second, and FSM concentration is a distant third whose sign depends on the pupil group. A one-line summary sentence immediately after the standardised-coefficient figure would help.

2. **Add an explicit mechanisms paragraph** to §6 (probably a new §6.x or an extension of 6.1). The causal chain the reviewer wants spelled out:
   - Admissions changes → altered FSM composition at school level → *small, direction-ambiguous* effect on attainment (our finding).
   - Admissions changes → longer journeys for displaced pupils → *increased absence* [@Thomson2023] → *large negative* effect on attainment.
   - So the admissions lever operates on attainment mainly through the **second, unintended** pathway, and in the wrong direction. This is arguably the sharpest version of the paper's argument and it isn't currently stated as a mechanism chain.

3. **Add a short passage to §7 Conclusion** connecting the national finding to the local policy timeline: what the 2023 FSM-priority policy and the 2024/26 proposals would each be expected to do to FSM composition and to absence, and why the net expected effect on the disadvantage gap is close to zero or negative.

**Caution.** The reviewer says "observed attainment patterns... post-2022 admissions policy changes". We should be careful not to over-claim: we do **not** have a causal evaluation of the 2023 FSM-priority policy, and the 2026 arrangements post-date our outcome data. Add an explicit sentence saying the paper models *expected* effects from national associations rather than *observed* effects of the local reform, and note the council's own commissioned FSM-priority evaluation is still pending.

---

# Reviewer 2

## R2.1 (§3.2) — Loss of efficiency from school-level-only prior attainment correction

- [x] **Done** — RPE_Paper.qmd §3.2 (pupil-level caveat) + §6.7

> *"as you are not selecting Progress 8 and do not have pupil-level data, there will be a loss of efficiency in correcting for prior attainment. The most robust correction for prior attainment would be at both pupil and school level. Without it, there is a danger of over-inflating your estimates."*

**Type:** Writing (methodological caveat) · **Effort:** ~45 min · **Files:** `JEP_Paper_tight.qmd` §3.2, §6.7 Limitations

**This is a fair point and should simply be conceded clearly.** We can't fix it without NPD access.

**Suggested fix:**

1. Add an explicit paragraph in §3.2 acknowledging that school-level aggregate prior-attainment controls are a coarser correction than pupil-level, and that residual confounding from within-school intake variation is therefore possible.
2. State the direction of the likely bias — with an imperfect prior-attainment control, coefficients on correlated intake variables (FSM, EAL, absence) may absorb some prior-attainment signal and be **upward-biased in magnitude**.
3. Strengthen §6.7 Limitations with the same point and note that NPD/SRS-based replication is the obvious next step (we already mention this elsewhere — make sure it's linked to this specific concern).
4. **Pair this with the R2.3 robustness check** below — adding mean KS2 scaled score is the strongest available partial answer, and lets us say "when we add a richer prior-attainment control, coefficients move by X".

---

## R2.2 (§3.2) — Justify the logarithmic functional form; per-variable diagnostics

- [x] **Done** — RPE §3.2 (which vars logged vs linear + supplementary diagnostics ref). ⚠️ NEED: add the diagnostics to model_experiments.qmd

> *"Please provide further justification of using the logarithmic functional form. Can this be justified for all variables? Can you present information (e.g., residual plots) justifying this decision for all variables? A uniform log transformation across all variables assumes each requires the same correction, which may not be the case, particularly for proportion-based variables such as low prior attainment. Reporting variable-specific diagnostics (e.g. partial residual plots?), or at least confirm that this have been conducted, would help justify this choice."*

**Type:** Analysis + writing · **Effort:** ~3 hours · **Files:** `output/model_experiments.qmd` (new section), `JEP_Paper_tight.qmd` §3.2

**Important clarification to make first:** we do **not** apply a uniform log transform to all variables. Checking the model formula, only `PTFSM6CLA1A`, `PERCTOT`, `PNUMEAL` and `average_number_of_days_taken` are logged; `PTPRIORLO`, `gorard_segregation`, `remained_in_the_same_school` and `teachers_on_leadership_pay_range_percent` enter **linearly**. The reviewer's specific worry — that `PTPRIORLO` is logged despite being a proportion — is based on a misreading, which suggests §3.2 isn't stating the specification clearly enough.

**Suggested fix:**

1. **Rewrite the functional-form paragraph in §3.2** to state variable-by-variable which enter logged and which linear, and why. Consider a small table: variable | transform | rationale.
2. **Add a diagnostics section to the supplementary** (`model_experiments.qmd`) containing:
   - Partial residual (component-plus-residual) plots for each continuous predictor under the chosen specification.
   - A comparison of AIC/BIC for logged vs linear vs spline treatment of each predictor — we already have a splines-vs-linear comparison at KS2 in the sibling analysis; do the equivalent at KS4.
   - Confirmation that log-transformation was chosen on fit + interpretability grounds (elasticities are policy-legible), not applied by default.
3. **Add a forward reference** from §3.2 to that supplementary section so the reviewer can find it.

**Note:** we already justify the log form substantively (non-linearity / accelerating returns is a core argument of the paper). The gap is *diagnostic evidence*, not rationale. Point 2 is the real work.

---

## R2.3 (§3.2) — Only low prior attainment included; why not average KS2 score?

- [x] **Done** — RPE_Paper.qmd §3.2 (KS2 availability constraint + robustness summary)

> *"Am I correct that only low prior attainment is included as a KS2 prior attainment control? This does not strike me as adequate. Is the average KS2 score not included in the school-level data? Why is just the low prior attainment percentage included? Is there a risk of inflated estimates elsewhere without this being well controlled for?"*

**Type:** Analysis + writing · **Effort:** ~3 hours · **Files:** `output/model_experiments.qmd` (new section), `JEP_Paper_tight.qmd` §3.2, §6.7

**The reviewer is right, and we can partly fix it.** I've checked the panel:

| Variable | What it is | Completeness (analytical sample) | Availability |
|---|---|---|---|
| `PTPRIORLO` | % low prior attainment | **99.4%** | All 4 years |
| `PTPRIORAV` | % middle prior attainment | 74.8% | 2021-22 → 2023-24 only |
| `PTPRIORHI` | % high prior attainment | 74.8% | 2021-22 → 2023-24 only |
| **`KS2ASS`** | **Mean KS2 scaled score** (values ~100–110) | **74.8%** | **2021-22 → 2023-24 only** |

So: **the average KS2 score does exist in the data (`KS2ASS`), but DfE did not publish it for 2024-25.** That is the actual reason only `PTPRIORLO` is in the main model — it's the only prior-attainment measure available across all four years. This has never been stated in the paper and absolutely should be.

Two further facts worth reporting:
- `PTPRIORLO + PTPRIORAV + PTPRIORHI` sums to 100 (mean 100.01, sd 0.40) — they are compositional, so all three cannot enter simultaneously.
- `cor(PTPRIORLO, PTPRIORHI) = −0.78` — strongly but not perfectly collinear.

### ✅ ANALYSIS RUN — results below

Script: `revisions/r23_prior_attainment.R` · Output: `revisions/r23_output.txt`

Refit on the 9,047 school-years (152 LAs, 2021-22 → 2023-24) where the richer measures exist, estimation sample held fixed:

| Spec | Prior-attainment controls |
|---|---|
| `P0` | `PTPRIORLO` only — **the published specification** |
| `P1` | `PTPRIORLO` + mean KS2 scaled score (centred at 100) |
| `P2` | `PTPRIORLO` + `PTPRIORHI` |
| `P3` | all three |

The three measures are near-substitutes: r(`PTPRIORLO`, `KS2ASS`) = **−0.93**, r(`PTPRIORHI`, `KS2ASS`) = **+0.94**.

#### The central finding is robust: absence barely moves

| Coefficient | P0 | P1 (+KS2) | Change |
|---|---|---|---|
| **All pupils** | | | |
| `log(PERCTOT)` — absence | −0.2099 | −0.2041 | **+2.8%** |
| `log(PTFSM6CLA1A)` — FSM | −0.0637 | −0.0435 | **−32%** |
| `log(PNUMEAL)` — EAL | 0.0062 | 0.0073 | +17% |
| **Disadvantaged** | | | |
| `log(PERCTOT)` — absence | −0.2961 | −0.2911 | **+1.7%** |
| `log(PTFSM6CLA1A)` — FSM | +0.0108 | +0.0257 | +137% |
| `log(PNUMEAL)` — EAL | 0.0232 | 0.0242 | +4.2% |

**Absence is essentially unaffected (1.7–2.8%) by adding a far better prior-attainment control.** That is a strong, direct answer to "is there a risk of inflated estimates" for the paper's central claim.

#### The reviewer is partly right — but it helps us

- **FSM attenuates 32%** for all pupils. So `PTPRIORLO` alone *was* leaving some prior-attainment signal to be absorbed by FSM. Conceding this costs nothing: the paper's argument is that FSM concentration is a *weak* lever, and it turns out to be weaker still.
- **The contentious sign-flip gets stronger.** For disadvantaged pupils the FSM coefficient moves from +0.0108 (t = 2.60) to +0.0257 (**t = 6.01**). Better prior-attainment control makes the positive coefficient larger and far more significant — useful ammunition for R2.6 too.
- **Gorard segregation** shows a large % change but is **non-significant in every specification** (t = −0.36, 1.03, 0.89, 1.11); the percentage is an artefact of a near-zero base. Report the t-values, not the % change.

#### KS2 mean score is the better control, and `PTPRIORLO` was proxying for it

| Spec | Marginal R² (all) | AIC (all) | `PTPRIORLO` t |
|---|---|---|---|
| P0 | 0.641 | −17,609 | **−40.5** |
| P1 | 0.663 | −18,126 | **−0.25** |
| P2 | 0.659 | −18,002 | −25.7 |
| P3 | 0.664 | −18,121 | −2.08 |

Once mean KS2 score is included, `PTPRIORLO` becomes **statistically indistinguishable from zero**. AIC improves by ~500.

> ⚠️ **Interpretation consequence.** The paper currently treats low prior attainment as the second most important driver. On this evidence `PTPRIORLO` is largely acting as a **proxy for the school's overall prior-attainment level**, not as a distinct low-attainer effect. Claims about the *specific* importance of the low-prior-attainment share need softening to "prior attainment" generally.

#### Interaction with R2.7 — B&H's rank is sensitive to this too

| Spec | All pupils rank | Disadvantaged rank |
|---|---|---|
| P0 | 3 | **7** |
| P1 (+KS2) | 5 | **12** |
| P2 | 6 | 10 |
| P3 | 5 | 9 |

Adding mean KS2 moves B&H from **7th to 12th** for disadvantaged attainment — a smaller shift than the absence question (7th → 46th) but in the same direction. Both should be reported together so the sensitivity is transparent.

*(P0 reproduces the published rank of 7 on the 3-year subsample — a useful validation of the setup.)*

---

### Recommended response

**Keep `PTPRIORLO` in the 4-year main specification, and report P1 as a prominent robustness check.** Rationale: switching to the 3-year model would sacrifice 2024-25, the year that anchors the "2nd worst absence in England" claim and the most policy-relevant cohort — and the coefficient that matters most for the paper's argument (absence) is unaffected.

**Actions:**

1. **State the availability constraint in §3.2** — mean KS2 scaled score and the middle/high bands are published for three of our four years; `PTPRIORLO` is the only measure spanning the full panel. This has never been said and is the direct answer to the reviewer's question.
2. **Add the robustness table** to §3.2 or a new §4.x, full detail in the supplementary.
3. **Concede the FSM attenuation openly** and note it strengthens rather than weakens the argument.
4. **Soften low-prior-attainment claims** to "prior attainment" per the interpretation warning above.
5. **Report the B&H rank sensitivity jointly with R2.7.**
6. **Note in §6.7** that even mean KS2 is a school-level aggregate — this addresses R2.1's pupil-level point but does not resolve it.

**Also note:** sample has **152** LAs for all pupils but **151** for disadvantaged (one LA has no reportable disadvantaged ATT8). This explains the 151/152 discrepancy flagged in R2.7.

---

## R2.4 (§4.2) — Why is Ofsted rating a random effect rather than an ordinal fixed effect?

- [x] **Done** — RPE_Paper.qmd §4.2 (RE-vs-FE justification, per-band magnitudes, endogeneity note)

> *"Random effects are usually reserved for grouping variables with many levels; Ofsted rating has only four ordered categories and its relationship to the outcome seems important to your argument. I would have assumed that an ordinal fixed effect would be more appropriate here. Can you further justify your choice?"*

**Type:** Analysis + writing · **Effort:** ~2 hours · **Files:** `output/model_experiments.qmd`, `JEP_Paper_tight.qmd` §3.2 / §4.2

**The reviewer is methodologically correct** — four ordered categories is well below the conventional threshold (~5–8 minimum) for a random effect, and the variance component will be poorly estimated. This is a legitimate specification criticism and the honest response is either to justify it properly or to change it.

### ✅ ANALYSIS RUN — results below

Script: `revisions/r24_ofsted_spec.R` · Output: `revisions/r24_output.txt`

Four specifications on the full 12,199 school-year sample (3,300 schools), identical except for how Ofsted enters:

| Spec | Ofsted term |
|---|---|
| `O-re` | `(1 \| OFSTEDRATING_1)` — **the published specification** |
| `O-fe` | categorical fixed effect (ref = Outstanding) |
| `O-ord` | single linear ordinal term (Outstanding = 1 … Inadequate = 4) |
| `O-none` | omitted entirely — bears on endogeneity |

Rating distribution: Outstanding 15.2%, Good 70.1%, Requires Improvement 12.2%, Inadequate 2.4%.

#### The specification choice is substantively immaterial

Comparing `O-re` with `O-fe`, all pupils:

| Coefficient | O-re | O-fe | Difference |
|---|---|---|---|
| `log(PTFSM6CLA1A)` | −0.06748 | −0.06747 | **<0.02%** |
| `log(PERCTOT)` | −0.21323 | −0.21306 | **0.08%** |
| `log(PNUMEAL)` | 0.00586 | 0.00585 | 0.2% |
| `PTPRIORLO` | −0.00575 | −0.00575 | 0.0% |

Disadvantaged is the same story (FSM +0.00765 → +0.00767; absence −0.30476 → −0.30452). **Every other coefficient is unchanged to three or four significant figures.** The reviewer's objection is methodologically legitimate but changes nothing substantive — which is the ideal position to be in when conceding it.

B&H's LA effect is also unmoved: rank **4 → 4** (all pupils), **7 → 7** (disadvantaged). So this does not interact with R2.7.

#### The fixed effect gives us something useful: reportable per-band coefficients

| Rating (vs Outstanding) | All pupils | Disadvantaged |
|---|---|---|
| Good | −2.21 ATT8 pts | −2.04 ATT8 pts |
| Requires Improvement | −4.24 | −3.86 |
| Inadequate | −4.70 | −4.25 |

All highly significant (\|t\| = 15–24). Note the **scale is not linear**: Outstanding → Good costs ~2.2 points and Good → RI a further ~2.0, but RI → Inadequate only a further ~0.5. Requires Improvement and Inadequate are nearly equivalent in attainment terms.

**That is an argument for the categorical fixed effect over the ordinal one** the reviewer suggested: `O-ord` imposes a uniform −1.92 points per band and hides the flattening at the bottom of the scale. Worth saying so explicitly — it shows we engaged with the suggestion rather than just complying.

#### Fit statistics — be careful not to over-claim

| Spec | Params | Marginal R² | Conditional R² | AIC | BIC |
|---|---|---|---|---|---|
| `O-re` | 11 | 0.632 | 0.771 | **−22,600** | **−22,481** |
| `O-fe` | 14 | 0.706 | 0.783 | −22,587 | −22,454 |
| `O-ord` | 12 | 0.705 | 0.783 | −22,578 | −22,459 |
| `O-none` | 11 | 0.689 | 0.774 | −22,002 | −21,891 |

> ⚠️ The jump in marginal R² (0.632 → 0.706) is **largely definitional**, not a genuine fit gain: as a random effect Ofsted's contribution sits in the random part and is excluded from marginal R². Conditional R² moves only 0.771 → 0.783, and AIC/BIC marginally *favour* the random effect (by ~13 and ~27 points on ~22,600 — negligible). Do not present the marginal R² rise as evidence the fixed effect fits better.

#### Bonus finding: including Ofsted makes our absence estimate conservative

Dropping Ofsted entirely (`O-none`) *increases* the absence coefficient:

| Outcome | With Ofsted | Without | Change |
|---|---|---|---|
| All pupils | −0.2132 | −0.2476 | **+16%** |
| Disadvantaged | −0.3048 | −0.3437 | **+13%** |

FSM barely moves (−0.0675 → −0.0699). So Ofsted is absorbing absence signal — unsurprising, since inspectors see attendance data and high-absence schools rate worse. **Because the rating is partly a consequence of absence, controlling for it attenuates absence's total effect by 13–16%: our headline absence estimate is conservative.** Worth stating — it strengthens the central claim while honestly flagging the endogeneity.

---

### Recommended response

**Switch the main specification to the categorical fixed effect (`O-fe`).**

The cost is nil (no other coefficient moves, B&H's rank is unchanged), and the gains are real: it concedes a legitimate methodological point, produces interpretable per-band coefficients the paper can report, and surfaces the non-linear structure of the rating scale.

**Actions:**

1. **Refit the main models with Ofsted as a categorical fixed effect** and update §3.2, §4.2 and all downstream tables.
2. **Report the per-band coefficients** in §4.2 — they are genuinely informative and directly answer "its relationship to the outcome seems important to your argument".
3. **Note why categorical rather than ordinal** — the RI/Inadequate flattening means a linear ordinal term would misrepresent the scale. Shows engagement with the specific suggestion.
4. **Report the robustness explicitly**: all other coefficients unchanged to 3–4 s.f. under either specification.
5. **Be honest about the marginal R² artefact** per the warning above.
6. **Add the endogeneity caveat** to §3.2 or §6.7: Ofsted is included as a contextual control, not a causal factor; reverse causality is likely; and the `O-none` comparison shows this makes the absence estimate conservative rather than inflated.

**Sample-definition note.** R2.4's all-pupil models use the full 12,199-row sample (rank 4, matching the published paper). R2.7's all-pupil models were fitted on the common sample shared with the disadvantaged outcome (rank 3). The disadvantaged results — the ones that matter — use the correct sample in both. Worth standardising before the final write-up so no table contradicts another.

---

## R2.5 (§4.2) — Make the magnitude of non-linear effects concrete

- [x] **Done** — RPE §4.2 per-band Ofsted magnitudes + forward-ref to §5.3 accelerating-returns

> *"The points about strength of effect as one moves up/down the scale are interesting. Would it be possible to show this graphically or by giving examples at selected points within the scale? It is hard to concretely grasp the magnitude of this."*

**Type:** Writing / signposting (mostly already done) · **Effort:** ~1 hour · **Files:** `JEP_Paper_tight.qmd` §4.2, §5.3

**We already have exactly this figure** — the accelerating-returns plot in §5.3 shows predicted ATT8 gain from a 1pp reduction at every starting level for both absence and FSM. The problem is it appears in the *case study* section, ~10 pages after the non-linearity discussion in §4.2, so a reader hits the abstract claim long before the concrete illustration.

**Suggested fix:**

1. **Add a forward reference** in §4.2: *"the practical magnitude of this non-linearity is illustrated in Figure X (§5.3)"*. Cheapest possible fix.
2. **Better:** add a small **marginal-effects table** in §4.2 giving predicted ATT8 change from a 1pp shift at, say, the 10th, 25th, 50th, 75th and 90th percentile of each logged predictor. This gives the reviewer concrete numbers at the point of the claim, for all logged variables rather than just the two in the case-study figure.
3. Optionally move or duplicate a simplified version of the accelerating-returns figure into §4.2.

---

## R2.6 (§4.3) — "Differing effects" vs "differing pupils" interpretation

- [x] **Done** — RPE §4.3 (differing-pupils concession; policy argument holds under both)

> *"Is it not equally possible that this is an issue with the measure? i.e., disadvantaged pupils, even when accounting for length of FSM status and/or attainment, are a heterogeneous group and those highly concentrated in a school are not necessarily equivalent to those with similar characteristics who are few in number. Have you any reason to favour 'differing effects, equivalent pupils' over the 'differing pupils' (within the measures available) interpretation?"*

**Type:** Writing / framing · **Effort:** ~1.5 hours · **Files:** `JEP_Paper_tight.qmd` §4.3

**This is the sharpest methodological point in either review, and the honest answer is: no, we don't have grounds to favour one over the other.** The FSM6CLA1A binary cannot distinguish depth or duration of disadvantage, so a school with 50% FSM and a school with 20% FSM may have systematically different *kinds* of disadvantaged pupils, not just different proportions. This is composition bias, and it's a completely plausible alternative explanation for the positive coefficient.

**Suggested fix — concede explicitly and honestly:**

1. Add a paragraph in §4.3 stating the "differing pupils" interpretation directly and acknowledging we cannot rule it out with aggregate data.
2. Note what *partial* evidence we have and be careful not to overstate it:
   - The sign-flip is robust across multiple alternative disadvantage measures and outcome definitions (supplementary, `#sec-directionality`) — this makes a *pure* measurement artefact less likely, but does **not** resolve composition bias, since all available measures share the same underlying limitation.
   - The vocational/Open-element result is suggestive of a genuine specialisation mechanism rather than pure composition.
3. **Soften the surrounding claims accordingly.** Where the paper currently implies an effect, shift to "consistent with" language.
4. Add to §6.7 Limitations: distinguishing these interpretations requires pupil-level data with FSM duration (NPD `FSMEVER` history), and state this as a specific, tractable follow-up.

> This concession costs the paper little — our policy argument only needs "redistribution is unlikely to *help* disadvantaged attainment", which holds under both interpretations. Being upfront here will read as rigour.

---

## R2.7 (§5.2) — Is the B&H disadvantaged-performance result justifiable when controlling for absence?

- [x] **Done** — RPE_Paper.qmd §5.2 (comparison table + reframe), abstract, intro, §6.5 retitled+reframed

> *"Am I right in saying that the result about B&H performance for disadvantaged pupils (as per Fig.3 and paragraph bottom of page 17) is based on a calculation that has controlled for absence? This does not seem justifiable to me. Would it be possible to report with and without this factor taken into account? (I assume that absence picks up both school and contextual variation – a point made later on page 20). Given the very low attendance in the city, this is a pivotal point for the overall interpretation."*

**Type:** Analysis + writing · **Effort:** ~3 hours · **Files:** `output/model_experiments.qmd`, `JEP_Paper_tight.qmd` §5.2, §6

### ⚠️ This is the most consequential point in the review. Address it first.

**The reviewer is right that it matters, and right about the direction of the concern.** Brighton and Hove has the 2nd-worst absence rate in England. Controlling for absence means the LA effect is estimated *conditional on* that terrible absence — i.e. "given how badly its pupils attend, B&H schools do remarkably well". Remove the control, and some of what is currently credited to the city's schools will be reattributed to the absence it fails to prevent. **The "7th best in England" headline could move materially.**

**We already have most of the machinery.** The two-stage decomposition (§5.5 / supplementary `#sec-two-stage-absence`) already fits three specifications — no absence control (M0), raw absence (M1), and intake-predicted absence (M2). What we have *not* done is extract the **LA-level random effects** and the caterpillar ranking under each. That's the exact comparison the reviewer is asking for.

### ✅ ANALYSIS RUN — results below

Scripts: `revisions/r27_absence_control.R`, `revisions/r27_followup.R` · Output: `revisions/r27_output.txt` · Objects: `revisions/r27_results.rds`

Four specifications, **identical except for the absence term**, fitted on the same 151-LA sample so rankings are directly comparable:

| Spec | Absence term |
|---|---|
| `A-none` | none |
| `A-raw` | `log(PERCTOT)` — **the published specification** |
| `A-expL` | intake-predicted absence, stage 1 **with** LA random effect |
| `A-expNoL` | intake-predicted absence, stage 1 **without** LA random effect |

#### Disadvantaged pupils — the headline claim does not survive

| Spec | Effect | 95% CI | Significant? | Rank | ATT8 pts |
|---|---|---|---|---|---|
| `A-none` | 0.018 | −0.022 to 0.058 | **No** | **46** | +0.69 |
| `A-raw` *(published)* | 0.050 | 0.015 to 0.086 | Yes | **7** | +1.98 |
| `A-expL` | 0.059 | 0.022 to 0.096 | Yes | **7** | +2.33 |
| `A-expNoL` | 0.017 | −0.023 to 0.057 | **No** | **45** | +0.67 |

**Without an absence control, Brighton and Hove is 46th of 151 and no longer statistically distinguishable from the national average.** The "7th out of 152" claim is entirely conditional on controlling for absence.

#### All pupils — more robust

| Spec | Effect | Significant? | Rank |
|---|---|---|---|
| `A-none` | 0.036 | Yes | 17 |
| `A-raw` *(published)* | 0.063 | Yes | 3 |
| `A-expL` | 0.055 | Yes | 6 |
| `A-expNoL` | 0.036 | Yes | 16 |

The all-pupils result **remains significantly positive** without the absence control; the rank moves 3 → 17. So the "high-performing city" claim survives for all pupils but not for the disadvantaged subgroup that the policy debate was actually about.

#### Why the two expected-absence variants diverge

| Quantity | Value |
|---|---|
| B&H observed absence (4-yr mean) | 10.77% |
| Expected — stage 1 **with** LA effect | 10.23% → excess **0.54 pp** |
| Expected — stage 1 **intake only** | 8.82% → excess **1.95 pp** |
| B&H mean FSM | 25.2% (national 27.1%) |

B&H's intake is slightly *less* deprived than the national average, so intake explains almost none of its high absence. Nearly 2 percentage points of absence sit above what intake predicts. When stage 1 carries an LA random effect it simply absorbs that excess as "context", which is why `A-expL` reproduces `A-raw`. Rank correlations confirm two families: ρ(`A-none`, `A-expNoL`) = **0.994**; ρ(`A-raw`, `A-expL`) = **0.965**; ρ(`A-none`, `A-raw`) = 0.891.

> **This is the crux.** By the paper's own §5.5 argument, absence over and above intake prediction is the *school/system-manageable* component. On that logic `A-expNoL` is the more principled specification — and it puts B&H 45th, not 7th.

#### Not unique to B&H

Absence control reshuffles LA rankings generally: Bradford moves 113 → 38, York 114 → 31 (all pupils), Thurrock 58 → 111. **14 LAs lose significance** for disadvantaged attainment when absence is dropped (21 for all pupils). Worth reporting as a general methodological finding about caterpillar plots, not just a B&H caveat.

#### Separate factual issue found

The "second worst absence rate in England" claim holds **only for 2024-25**:

| Year | B&H mean absence | Rank (1 = worst) |
|---|---|---|
| 2021-22 | 10.82% | 9th |
| 2022-23 | 10.51% | 16th |
| 2023-24 | 10.97% | 10th |
| **2024-25** | **10.79%** | **2nd** |

Add the year qualifier wherever this appears unqualified — notably the **abstract** (line 66).

#### But the trend is the stronger story

B&H's absence has been **flat**; the country's has **fallen**. It slid from 9th- to 2nd-worst on an essentially unchanged number because everyone else recovered post-pandemic and B&H didn't.

| Year | National | B&H | Gap |
|---|---|---|---|
| 2021-22 | 9.12% | 10.82% | 1.70 pp |
| 2022-23 | 9.18% | 10.51% | 1.34 pp |
| 2023-24 | 9.03% | 10.97% | 1.95 pp |
| 2024-25 | **8.27%** | **10.79%** | **2.52 pp** |

Over 2021-22 → 2024-25: **143 of 152 LAs (94%) reduced absence**; national mean −0.85 pp; **B&H −0.03 pp, ranking 143rd of 152 for improvement**.

**Use this instead of attributing the absence *level* to the council.** "Absence rose under this administration" is false and easily rebutted. "Absence fell in 94% of English local authorities and conspicuously not here" is evidenced and hard to argue with.

**Two cautions when writing this up:**

1. **"Not intake-explained" ≠ "the council's fault."** The ~2 pp excess over intake prediction rules out the *composition* defence (B&H's FSM rate is *below* national average), but the residual could still be local health, transport geography, seaside deprivation not captured by FSM, or SEND provision. Write it as *"not explicable by intake, and not improving when comparable authorities improved"* and let the reader infer.

2. **Manage the tension with R2.8 and §5.5.** Our decomposition says the school-controllable share of absence is the *smaller* component; R2.8 asks us to evidence how amenable absence is to school/LA action. If §5.2 reads as "this is the council's failure" while §5.5 reads as "most absence isn't school-manageable", a careful reviewer will catch it. The reconciliation is honest and available — the structural share is defined relative to *intake*, and B&H's excess sits above it — but both sections must be written with each other in view.

#### Strategic reframing worth considering

The intro (line 220) currently argues the council mischaracterised a high-performing city as failing. Unconditionally the city is **46th and statistically ordinary** for disadvantaged attainment — so the council's *"we have a problem"* framing was **not wrong**. The diagnosis was.

**"They were right that there is a problem, and wrong about its cause"** is a more persuasive paper than "they were wrong that there is a problem". It concedes the motivating observation, removes the easiest line of attack, and makes the misdiagnosis the entire story: the city saw an attainment gap and reached for admissions, when the gap between its conditional 7th and unconditional 46th was sitting in plain sight in the absence data.

---

### Recommended response — reframe rather than retreat

The honest reading strengthens the paper's central argument:

> Conditional on the absence its schools face, Brighton and Hove is 7th in England for disadvantaged attainment. Unconditionally it is 46th and statistically ordinary. **The distance between those two numbers — about 1.3 Attainment 8 points — is what absence costs the city's disadvantaged pupils.**

That is a far stronger claim than "we are 7th best". It converts the reviewer's objection into a quantification of the paper's own thesis: absence is not a footnote to Brighton and Hove's story, it is the difference between a top-10 authority and a mid-table one.

**Actions:**

1. **Report all specifications in §5.2** — replace the single caterpillar with a comparison (table or multi-panel plot), and state the conditional interpretation explicitly.
2. **Revise the affected claims** — see checklist below.
3. **Retitle §6.5** — "What is driving Brighton and Hove's over-achievement?" presumes the conclusion. Something like *"Conditional performance and the cost of absence"*.
4. **Present `A-expNoL` as the principled middle ground** and connect it to §5.5, using the reviewer's own observation that absence "picks up both school and contextual variation".
5. **Forward-reference the decomposition from §5.2** so the reader meets this question where it first arises.
6. **Add the general finding** that LA rankings are materially sensitive to absence control — useful methodological contribution in its own right.

**Claims requiring revision:**

- [ ] **Line 66 (abstract)** — "second worst absence rate in England" → add "in 2024-25"
- [ ] **Line 220 (intro)** — "7th out of 152 ... 5th out of 152" → qualify as conditional on absence
- [ ] **Line 679 (§5.2)** — "ranks 7th ... 5th ... 4th ... roughly 2 GCSE points" → report both conditional and unconditional
- [ ] **Line 1125 (§6.5)** — "ranks 7th; 5th; 4th" → same
- [ ] **Line 1127 (§6.5)** — "one of the highest-performing LEAs in the country" → qualify, or reframe around the conditional/unconditional gap
- [ ] **§6.5 section title** — presumes over-achievement
- [ ] Check whether `pulling_the_right_lever.qmd` executive summary and slide decks carry the same claims and need syncing

**Note:** our sample has **151** LAs, not 152. Check the "out of 152" phrasing against the estimation sample.

---

## R2.8 (§5.5) — How amenable is attendance to school/LA-level intervention?

- [x] **Done** -- RPE 2.2 amenability paragraph (structural vs school-manageable; cross-departmental), consistent with the decomposition

> *"it was (if I've understood) controlled for in the estimates of disadvantaged pupil performance, therefore treated as a non-school factor, but in Section 5.5 the discussion is about schools addressing the issue. Could evidence be presented (in the literature review perhaps) about how amenable to change attendance is for schools and whether large-scale, local-authority wide change is likely to come from 'council-led attendance interventions' and sharing best practice as suggested? Or are there other (potentially social) interventions that should be mentioned alongside these suggestions?"*

**Type:** Literature + writing · **Effort:** ~4 hours (needs new reading) · **Files:** `JEP_Paper_tight.qmd` §2.2 Attendance, §5.5, §6.2

**Two distinct criticisms here — separate them:**

**(a) An apparent internal inconsistency.** We control for absence (treating it as exogenous context) but then recommend schools act on it (treating it as endogenous). *We have a good answer:* this is precisely the tension the two-stage decomposition resolves — absence is partly structural, partly school-manageable. But if the reviewer read §5.5 and still saw an inconsistency, **the decomposition isn't doing its explanatory job early enough in the paper.** Fix by flagging the dual nature of absence when it is *first* introduced (§2.2 and §3.2), not only in §5.5.

**(b) Missing evidence on intervention effectiveness.** This is a genuine gap. We assert attendance is actionable but cite little on *what actually works*. Needs new literature in §2.2:

- EEF attendance evidence reviews and the (limited, mixed) trial evidence on attendance interventions.
- DfE attendance-mentor and attendance-hub programme evaluations.
- Evidence on the post-pandemic structural shift in attendance — important because it argues that a substantial share of current absence is **not** school-manageable, supporting our decomposition finding.
- Literature on wider determinants: child/adolescent mental health, SEND provision gaps, family poverty, term-time holidays, transport.

**Also add — and this is the honest framing:** our own decomposition finds the school-controllable share of absence is the *smaller* component. So we should **temper the "council-led attendance interventions" recommendation** and give more weight to cross-departmental and social interventions (health, children's services, family support, transport). Recommending practice-sharing alone would be inconsistent with our own result. Reviewer 2 has effectively spotted this and is inviting us to correct it.

---

## R2.9 (Conclusion) — Academisation and reduced LA capacity as context for the analytical-infrastructure recommendation

- [x] **Done** — RPE §6.6 (post-2010 LA capacity erosion; MAT gap; system-level framing; simulator as prototype)

> *"it is perhaps worth noting that academisation and reduced local authority resourcing since 2010 have significantly reduced LA analytical capacity... I felt more than a little exasperated with policy-makers when I saw this recommendation about investment in local analytical infrastructure given the context. Presumably MATs have taken on this function in relation to their constituent schools, but this leaves a gap for locality-wide analyses to inform local (as opposed to MAT) policy. Is this a fair point? Could it be made?"*

**Type:** Writing / framing · **Effort:** ~1.5 hours · **Files:** `JEP_Paper_tight.qmd` §7 Conclusion (and possibly §6.6)

**Yes, it's a fair point, and it materially strengthens the paper.** The reviewer is offering us a better version of our own argument: it explains *why* the evidence vacuum in Brighton and Hove existed in the first place, rather than treating it as an unexplained local failure.

**Suggested fix — add a paragraph to §7 (or §6.6) making the structural argument:**

1. LA analytical capacity has been substantially eroded since 2010 through academisation and sustained real-terms funding reductions to LA central services.
2. MATs have absorbed some of this function — but **only for their own constituent schools**. This creates a structural gap: no actor has both the remit and the capacity for **locality-wide** analysis spanning maintained schools, academies across multiple trusts, and church schools.
3. Brighton and Hove is a clean illustration: the council remains the admissions authority for a system it has limited capacity to analyse as a whole, and the two academies (BACA/PACA) sit outside its analytical reach while being central to the policy question.
4. **Reframe our recommendation accordingly** — the ask is not simply "councils should invest" (which, as the reviewer notes, is exasperating given the funding context) but that the *system* has a structural analytical gap at the locality level, and either central funding or nationally maintained tooling is needed to close it. **This is exactly what our Policy Simulator is a prototype of** — reposition the tool as a partial response to a structural gap rather than a nice-to-have.

**Also worth citing:** relevant literature on post-2010 LA capacity reduction and the fragmentation of local school systems. `WilliamsGrayson2018` (school funding since 2010) is already in the bibliography and may be a starting point; will likely need 1–2 more.

---

# Minor points

## R2.10 — Cite an example or review for the "large volume of literature" claim (p6)

- [x] **Done** — RPE §2.1 (review citations added)

> *"P6. End of first para. Cite an example or review of 'large volume of literature' to support readers in following this up."*

**Effort:** ~15 min · **Files:** `JEP_Paper_tight.qmd` §2

Add 2–3 citations to a review or meta-analysis at that point. Candidates already in the bibliography that could serve; otherwise a recent systematic review of school-effectiveness determinants would be ideal. Locate the exact sentence at the end of the first paragraph of §2.

---

## R2.11 — Inconsistent Burgess citation ('Simon' vs 'S') (p8)

- [x] **Done** — bib: expanded Burgess initials; dropped duplicate Burgess2022 from the teaching-quality cite

> *"P8. Para3. There is a 'Simon' Burgess and an 'S' Burgess cited."*

**Effort:** ~20 min · **Files:** `SchoolAttainment.bib`

I've traced this. Two issues:

**(a) Inconsistent name formatting** — some entries use full first names, others initials:

| Bib line | Key | Current author field |
|---|---|---|
| 268 | `Burgess2022` | `Burgess, S and Rawal, S and Taylor, E` |
| 296 | `burgess_school_2020` | `Burgess, S and Greaves, E and Vignoles, A` |
| others | — | `Burgess, Simon and ...` ✅ |

Fix: expand initials to full names in those two entries.

**(b) Probable duplicate — worth checking.** `Burgess2022` (techreport, 2022) and `Burgess2023` (article, 2023) appear to be the **same paper**: *"Teachers' use of class time and student achievement"*, by Burgess, Rawal and Taylor — the working paper and the published version. Recommend citing only the published 2023 article and removing the 2022 techreport (or retaining it only if we deliberately cite the working paper for something not in the published version).

---

## R2.12 — 'to general' typo (p11, line 22)

- [x] **Done** — RPE §4.1 ("to general" removed)

> *"P11. Line 22. 'to general' typo."*

**Effort:** ~5 min · **Files:** `JEP_Paper_tight.qmd`

Likely "to general" where "to generalise" / "in general" / "the general" was intended. Needs locating against the submitted PDF pagination — search the qmd for `to general`.

---

# Cross-cutting observations

**1. Two reviewers, one shared theme: absence is doing a lot of work and its status is under-explained.** R1.2, R2.7 and R2.8 all circle the same issue — is absence a control, an outcome, or a lever? We *have* the answer (the two-stage decomposition), but it arrives in §5.5, far too late. **Consider introducing the dual nature of absence in §2.2 and §3.2**, so the reader is primed before §5.2 raises it implicitly.

**2. Several criticisms stem from presentation, not substance.** R2.2 (thinks all variables are logged — they aren't), R2.5 (the figure exists but is 10 pages later), R2.8(a) (the inconsistency is resolved but too late). Cheap fixes with good returns.

**3. Only two points can change our claims:** R2.7 (absence control in the LA effect) and R2.3 (mean KS2 control). Run both before rewriting anything, so the prose is written once against final numbers.

**4. Nothing here requires new data collection** — everything is answerable with the existing panel plus additional literature for R2.8 and R2.9.

---

# Suggested work order

| Step | Items | Output |
|---|---|---|
| 1 | R2.7 | LA effects with/without absence control; decide whether headline claims need revising |
| 2 | R2.3, R2.1 | KS2ASS robustness run on 3-year subsample; coefficient stability table |
| 3 | R2.4 | Ofsted fixed-effect refit; decide main vs robustness |
| 4 | R2.2 | Partial residual plots + transform comparison → supplementary |
| 5 | R2.5 | Marginal-effects table in §4.2 |
| 6 | R1.1 | Selective admissions clarification |
| 7 | R1.2, R2.6, R2.8, R2.9 | Discussion and framing rewrites (write once, after steps 1–2) |
| 8 | R2.10, R2.11, R2.12 | Minor fixes |
| 9 | — | Draft point-by-point response letter from this document |

---

# Response letter

Once items are ticked off, this document converts into the formal response. Structure per point:

> **Comment.** [verbatim reviewer text]
>
> **Response.** [what we did, where it now appears — section/page/figure]

Keep the verbatim quotes above intact for that purpose.

---

# Author-initiated additions (not reviewer-prompted)

## A1 (§6.2) — Class Divide "mould and damp" analogy

- [x] Done

Local campaign group Class Divide responded to an earlier presentation of this
work with the analogy that **absence is the mould and poverty is the damp** —
i.e. that treating absence directly is futile because the underlying poverty
will keep regenerating it. The paper previously mentioned Class Divide only in
passing (as a supporting organisation for Equity in Education) and did not
engage the argument. §6.2 now does.

The passage concedes what is right about the analogy — at pupil level the
disadvantage/absence association is strong and well evidenced (DfE2025b), and
our own two-stage decomposition (§5.5) shows a substantial share of school-level
absence is inherited from intake — then shows it does not account for Brighton
and Hove:

| Evidence | Value |
|---|---|
| LA-level regression of mean absence on mean FSM | **R² = 0.033** |
| B&H FSM eligibility | 25.2% (national 28.5% — *below* average) |
| B&H absence predicted from FSM alone | 8.82% |
| B&H actual absence | 10.77% (**+1.95pp excess**) |
| Rank of that excess | **5th largest of 152 LAs** |
| Mean absence of the 25 LAs within 2pp of B&H's FSM rate | 9.04% vs B&H 10.77% |
| Absence change 2021-22 → 2024-25 | national −0.88pp; B&H −0.03pp (143rd of 152) |

Framing: the conclusion drawn is *not* that poverty is irrelevant to absence,
but that it cannot explain Brighton and Hove's position, and that the portion of
the city's absence sitting above its intake prediction is precisely the portion
local action can reach.

---

## A2 — Cross-check of prose figures against recomputed tables

- [x] Done

After the switch to mean KS2, several numbers quoted in the prose no longer
matched the tables the models now produce. All figures below were recomputed
from `data/panel_data.rds` / `data/models_imputed.rds`. Where practical the
corrected values are now **inline R expressions** rather than typed constants,
so they cannot drift again.

### Table 1 (stepwise) — §4.2

| Claim in prose | Was | Now | Note |
|---|---|---|---|
| M1 FSM coefficient | −0.201 | −0.201 | correct |
| M1 R² ("almost 40%") | ~40% | 0.393 | correct |
| M2 FSM ("halves to") | −0.122 | −0.122 | correct |
| M2 R² ("over 60%") | >60% | 0.612 | correct |
| M3 variance explained | "approximately two-thirds" | **0.696** | now "close to 70%", inline |
| M3 *t*, % absence | −65.28 | **−60.5** | inline |
| M3 *t*, mean KS2 | 48.49 | **58.2** | 48.49 was the *M5* value, inline |
| M3 *t*, % FSM | −33.46 | **−21.0** | inline |
| M5 marginal R² | 0.63 | **0.65** | inline |
| M5 conditional R² | ~0.77 | **0.78** | inline |
| Variance from grouping factors | 14% | **13%** | inline |

### Ofsted (§4.2)

Fixed-effect form, vs 'Outstanding': Good **−1.9** (was 2.2), Requires
Improvement **−4.0** (was 4.2), Inadequate **−4.5** (was 4.7). Omitting Ofsted
increases the absence effect by **12–15%** (was 13–16%).

### Figure 1 (standardised coefficients) — §4.3 — substantive

Switching to mean KS2 **changed the ordering**. Prior attainment now enters
*positively* and is the largest standardised coefficient for all pupils
(0.089 vs absence 0.059) and for non-disadvantaged pupils (0.085 vs 0.047).
Absence remains largest for **disadvantaged** pupils (0.086 vs 0.079).

The text claimed "absence is the dominant predictor across all groups", which is
no longer true. Rewritten to distinguish **inherited intake** from **actionable
levers**: prior attainment is fixed for a cohort already in school and belongs in
the model as an intake control, so absence remains the largest factor open to
intervention. The paper's argument and the abstract's "most impactful single
policy lever" are unaffected.

Separately, the claim that "nearly half of the variation for disadvantaged
pupils is explained by attendance alone" was never supported: absence is
**33%** of the combined standardised effect (34.8% under the old specification).
Corrected to "roughly a third".

### Value added (§5.6) — BACA improves

Confirmed: BACA's disadvantaged value added *rises* under the new
specification. Latest year **+5.2 → +5.7**; four-year mean **+2.81 → +3.06**.
Still rank 1 of 10 in the city. Patcham's four-year mean moves **−2.57 → −2.37**
(text said "~2.6", now inline). All now computed inline from `bh_long`.

### Class Divide passage (§6.2) — recomputed on the paper's own sample

The figures first drafted came from an ad-hoc sample. Recomputed on
`national_latest` (the same basis as the "second-worst absence" claim) they are
**stronger**: LA-level absence~FSM R² = **0.05**; B&H FSM 28.8% vs 29.6% average;
predicted absence 8.2% vs actual 10.8%; excess **+2.6pp, the largest of any
authority in England** (was reported as 5th); among the **20** authorities within
2pp of its FSM rate, B&H has the highest absence, **2.3pp** above their mean.
Now computed inline in a `mould-stats` chunk.

### Other fixes

- Intro claim "over 80% of the variation" → **"close to 80%"** (conditional R² is 0.78).
- §3.2 "the remaining predictors, including the low-prior-attainment percentage" → mean KS2.
- §4.3 "after controlling for absence and low prior attainment" → mean KS2.
- Duplicated model equation (`$$…$$$$…$$`) removed — an editing artefact.
- Duplicated citation `@nakagawa_coefficient_2017@nakagawa_coefficient_2017` collapsed.

### Word table formatting

Table cells use pandoc's `Compact` paragraph style, which inherited 12pt Times
New Roman **double-spaced** from `Normal` — the reason tables overflowed. All 415
`Compact` paragraphs in the document are inside tables (none outside), so
restyling it is precisely targeted. `RPE_reference.docx` is a copy of
`Manuscript_Anonymous.docx` with `Compact` set to **8pt, single-spaced** and
table cell margins reduced from 108 to 43 twips. `RPE_Paper.qmd` now points at it.

---

## A3 — Three additions arising from post-submission critiques

- [x] Done

Prompted by criticisms of the underlying report from Prof. Gorard and Prof. Reay
(commissioned by Class Divide and circulated to councillors). Those reviews are
unpublished and so cannot be cited; each addition therefore stands on its own as
the authors' own methodological reasoning, engaging only published work. All four
references used were already in `SchoolAttainment.bib` but previously unused.

**1. §3.2 — what the disadvantage coefficient measures.** The concession Adam
judged most persuasive, and made sharper by the switch to mean KS2: a whole-
distribution control absorbs more of the poverty signal than a single-tail one,
which is why FSM attenuated by a quarter. New paragraph states the surviving
coefficient is a *within-intake* comparison, not an estimate of poverty's total
role. Cites `StopforthGayle2025`, `gorard_how_2019`. Closes the "his own model
shows disadvantage barely matters" misreading — the mirror image of the mould/damp
passage in §6.2.

**2. §6.1 — school-effect magnitude.** The paper had no citation to the school-
effectiveness literature. Now anchors "redistribution is a weak lever" to the
8–15% between-school consensus: `Coleman1966`, `SmithTomlinson1989` (~10%),
`TeddlieReynolds2000`. Own decomposition (supplementary) recovers the same order.
Converts an assertion into a recovery of settled consensus.

**3. §6.8 Limitations — inference on population data.** New paragraph: the panel
is a population, not a sample; standard errors are interpreted as cohort-to-cohort
stochastic variation; multilevel shrinkage guards against over-reading small-school
noise; significance treated descriptively, citing `WassersteinLazar2016`. **No one
is named and Gorard (2016) is deliberately not cited** — citing it would invite the
argument into the paper. Pre-empts the objection for readers who have heard it.

### Deliberately excluded

- All material from `blog_gorarded_draft.md` — combative, names a senior academic;
  its own counsel note advises holding it.
- The commissioning context (Class Divide soliciting the reviews) — score-settling.
- The "already established vs methodologically unsound" tension — a debate move.

### Already covered, no action taken

- Reay on reframing a political question as technical — intro scope paragraph.
- "No one loses" / commute→absence mechanism — §6.1, incl. the pushback on
  'almost no cost' citing `gorard_how_2019`.
