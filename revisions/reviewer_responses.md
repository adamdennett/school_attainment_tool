# Reviewer responses — tracking document

**Manuscript:** *How to Pull the Right Lever: School Attainment, Open Data Analytics and Local Education Policy in England*
**Decision:** Minor revisions
**Source file to edit:** `JEP_Paper_tight.qmd` (supplementary: `output/model_experiments.qmd`)

**Progress: 0 / 14 complete**

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

- [ ] **Done**

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

- [ ] **Done**

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

- [ ] **Done**

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

- [ ] **Done**

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

- [ ] **Done**

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

**Suggested fix — three parts:**

1. **State the availability constraint explicitly in §3.2.** One short paragraph: mean KS2 scaled score and the middle/high prior-attainment bands are published for three of our four years; `PTPRIORLO` is the only measure spanning the full panel; we therefore use it in the main specification to preserve the 2024-25 cohort.

2. **Run a robustness check on the 3-year subsample** (2021-22 → 2023-24) adding `KS2ASS` (and optionally `PTPRIORHI`) to the main models. Report:
   - How much the FSM, absence and EAL coefficients move.
   - Change in marginal/conditional R².
   - Whether the substantive ranking of predictors is unchanged.

   This is the direct answer to "is there a risk of inflated estimates". **If coefficients are stable, it substantially strengthens the paper.** If absence attenuates materially, we need to say so and adjust the claims.

3. **Report the result in §3.2 or a new §4.x**, with full detail in the supplementary, and reference it again in §6.7 Limitations.

> **Recommendation:** do this one early. It's the point most likely to change what we can claim, and it's cheap to run.

---

## R2.4 (§4.2) — Why is Ofsted rating a random effect rather than an ordinal fixed effect?

- [ ] **Done**

> *"Random effects are usually reserved for grouping variables with many levels; Ofsted rating has only four ordered categories and its relationship to the outcome seems important to your argument. I would have assumed that an ordinal fixed effect would be more appropriate here. Can you further justify your choice?"*

**Type:** Analysis + writing · **Effort:** ~2 hours · **Files:** `output/model_experiments.qmd`, `JEP_Paper_tight.qmd` §3.2 / §4.2

**The reviewer is methodologically correct** — four ordered categories is well below the conventional threshold (~5–8 minimum) for a random effect, and the variance component will be poorly estimated. This is a legitimate specification criticism and the honest response is either to justify it properly or to change it.

**Suggested fix (recommend option B):**

- **Option A — justify and keep.** Argue we treat Ofsted as a grouping factor because ratings are assigned at different times under different frameworks and we want partial pooling rather than a strong parametric ordering assumption. Weak; the reviewer will likely push back.
- **Option B — refit with Ofsted as an ordinal/categorical fixed effect and report both.** ✅ Recommended. Show that substantive conclusions are unchanged, and either (i) switch the main specification to the fixed-effect version, or (ii) keep the current one and present the fixed-effect version as a robustness check with the reviewer's concern cited.

Refitting is cheap — it's one term in three models. Given the reviewer notes "its relationship to the outcome seems important to your argument", the fixed-effect version arguably *helps* us: it gives interpretable coefficients per rating band that we can report directly.

**Also worth addressing:** Ofsted ratings are partly *endogenous* to attainment (inspectors see results). Worth a sentence in §3.2 or Limitations noting we include Ofsted as a contextual control rather than a causal factor, and that reverse causality is likely.

---

## R2.5 (§4.2) — Make the magnitude of non-linear effects concrete

- [ ] **Done**

> *"The points about strength of effect as one moves up/down the scale are interesting. Would it be possible to show this graphically or by giving examples at selected points within the scale? It is hard to concretely grasp the magnitude of this."*

**Type:** Writing / signposting (mostly already done) · **Effort:** ~1 hour · **Files:** `JEP_Paper_tight.qmd` §4.2, §5.3

**We already have exactly this figure** — the accelerating-returns plot in §5.3 shows predicted ATT8 gain from a 1pp reduction at every starting level for both absence and FSM. The problem is it appears in the *case study* section, ~10 pages after the non-linearity discussion in §4.2, so a reader hits the abstract claim long before the concrete illustration.

**Suggested fix:**

1. **Add a forward reference** in §4.2: *"the practical magnitude of this non-linearity is illustrated in Figure X (§5.3)"*. Cheapest possible fix.
2. **Better:** add a small **marginal-effects table** in §4.2 giving predicted ATT8 change from a 1pp shift at, say, the 10th, 25th, 50th, 75th and 90th percentile of each logged predictor. This gives the reviewer concrete numbers at the point of the claim, for all logged variables rather than just the two in the case-study figure.
3. Optionally move or duplicate a simplified version of the accelerating-returns figure into §4.2.

---

## R2.6 (§4.3) — "Differing effects" vs "differing pupils" interpretation

- [ ] **Done**

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

- [ ] **Done**

> *"Am I right in saying that the result about B&H performance for disadvantaged pupils (as per Fig.3 and paragraph bottom of page 17) is based on a calculation that has controlled for absence? This does not seem justifiable to me. Would it be possible to report with and without this factor taken into account? (I assume that absence picks up both school and contextual variation – a point made later on page 20). Given the very low attendance in the city, this is a pivotal point for the overall interpretation."*

**Type:** Analysis + writing · **Effort:** ~3 hours · **Files:** `output/model_experiments.qmd`, `JEP_Paper_tight.qmd` §5.2, §6

### ⚠️ This is the most consequential point in the review. Address it first.

**The reviewer is right that it matters, and right about the direction of the concern.** Brighton and Hove has the 2nd-worst absence rate in England. Controlling for absence means the LA effect is estimated *conditional on* that terrible absence — i.e. "given how badly its pupils attend, B&H schools do remarkably well". Remove the control, and some of what is currently credited to the city's schools will be reattributed to the absence it fails to prevent. **The "7th best in England" headline could move materially.**

**We already have most of the machinery.** The two-stage decomposition (§5.5 / supplementary `#sec-two-stage-absence`) already fits three specifications — no absence control (M0), raw absence (M1), and intake-predicted absence (M2). What we have *not* done is extract the **LA-level random effects** and the caterpillar ranking under each. That's the exact comparison the reviewer is asking for.

**Suggested fix:**

1. **Run it.** Extract the LA random effects for disadvantaged attainment under all three specifications and produce:
   - B&H's rank out of 152 under each.
   - A caterpillar plot or a small comparison table.
   - Ideally the rank correlation across specifications, so we can say how much LA rankings generally move (not just B&H's).

2. **Report honestly in §5.2**, whatever it shows. Three scenarios:
   - *Rank stable* → strong result; state it plainly and the reviewer's concern is dispatched.
   - *Rank drops moderately* → report both figures, explain the conditional interpretation, and note that this is precisely why absence is the priority lever.
   - *Rank drops sharply* → the "high-performing city" framing needs revising throughout, including abstract and conclusion. Better to find this now than post-publication.

3. **Use the reviewer's own framing in the fix.** They note absence "picks up both school and contextual variation" — that is *exactly* the argument of our §5.5 decomposition. The cleanest response is to present the M2 (intake-predicted absence) version as the principled middle ground: it controls for the part of absence the city cannot help, while leaving the school-manageable part in the residual. **This turns the criticism into a showcase for the paper's own methodological contribution.**

4. Consider **moving a short summary of the decomposition earlier** (or forward-referencing it from §5.2) so the reader meets the with/without-absence question at the point it first arises rather than three sections later.

---

## R2.8 (§5.5) — How amenable is attendance to school/LA-level intervention?

- [ ] **Done**

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

- [ ] **Done**

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

- [ ] **Done**

> *"P6. End of first para. Cite an example or review of 'large volume of literature' to support readers in following this up."*

**Effort:** ~15 min · **Files:** `JEP_Paper_tight.qmd` §2

Add 2–3 citations to a review or meta-analysis at that point. Candidates already in the bibliography that could serve; otherwise a recent systematic review of school-effectiveness determinants would be ideal. Locate the exact sentence at the end of the first paragraph of §2.

---

## R2.11 — Inconsistent Burgess citation ('Simon' vs 'S') (p8)

- [ ] **Done**

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

- [ ] **Done**

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
