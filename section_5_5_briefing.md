# Section 5.5: Two-Stage Decomposition — Methodological Note

**From**: DCM
**Re**: Collinearity issue in the two-stage absence decomposition (section 5.5 / Figure 6), and a clean fix
**Priority**: Worth addressing before submission — the fix is straightforward and the narrative doesn't change

---

## 1. The problem with the current two-stage approach

### What the code does

**Stage 1** (`RPE_Paper.qmd` L972–977): predicts `log(PERCTOT)` from `log(FSM)`, `log(EAL)`, `KS2`, `segregation`, plus `(1|year_label)` and `(1|gor_name/LANAME)` random effects.

**Stage 2** (`RPE_Paper.qmd` L990–996): predicts `log(ATT8SCR)` from `log(FSM)`, `log_expected_absence`, `log(EAL)`, `KS2`, `ADMPOL_PT`, `segregation`, plus `(1|year_label)`, `(1|OFSTEDRATING_1)`, and `(1|gor_name/LANAME)` random effects.

### Why this creates a collinearity problem

`log_expected_absence` is a linear combination of the Stage 1 covariates plus Stage 1's random intercepts:

```
log_expected_absence = β̂₁·log(FSM) + β̂₂·log(EAL) + β̂₃·KS2 + β̂₄·Seg + û_year + û_gor + û_LA:gor
```

Stage 2 has the **same random-effects structure** (`year_label`, `gor_name/LANAME`). Its own random intercepts absorb the random-effect components embedded in `log_expected_absence` — the model just shifts its own LA and year intercepts to soak up those terms. What's left is the fixed-effect linear combination: `β̂₁·log(FSM) + β̂₂·log(EAL) + β̂₃·KS2 + β̂₄·Seg` — which is near-collinear with the other Stage 2 covariates.

The model doesn't crash (lme4's iterative estimation and random-effect shrinkage prevent exact singularity), but the coefficient on `log_expected_absence` is poorly identified and unstable.

### The supplementary material already flags this

`model_experiments.qmd` L3165:

> *"if disadvantage is essentially the only exogenous predictor of absence, expected absence becomes collinear with disadvantage and the two-stage approach collapses to 'just don't control for absence'"*

And confirms it empirically: M1 (raw absence) and M2 (expected absence) produce near-identical FSM coefficients (-0.067 vs -0.069), meaning expected absence adds negligible information beyond what the covariates already provide.

---

## 2. The proposed fix: two parallel models

The goal of section 5.5 is to measure the **school's specific impact on absence** and see how that relates to overall school performance. This requires two independent signals:

- **How much absence does the school produce beyond what its intake predicts?** (the school's impact on absence)
- **How effective is the school overall?** (total value-added — teaching, leadership, curriculum, pastoral care, attendance management, and everything else combined)

The quadrant plot's power comes from reading these side by side. A school might be excellent overall despite an attendance problem (Q2), or it might have great attendance but underperform on everything else (Q4). You need both signals to tell these stories apart.

The current two-stage approach tries to build both signals from a sequential model. The parallel approach builds them independently:

| Signal | Source | What it measures |
|--------|--------|------------------|
| **X-axis**: School impact on absence | Stage 1 residuals (unchanged) | Excess absence — how much absence the school has beyond what its intake predicts. This is the school-specific absence signal. |
| **Y-axis**: Total value-added | M0 residuals (no absence control) | The school's total contribution to attainment — everything it does (teaching, leadership, attendance management, culture, pastoral care) beyond what intake predicts. |

The Y-axis is deliberately the **total** school effect, not specifically an attendance-management signal. That's what makes the quadrant informative: the X-axis isolates the attendance dimension, and the Y-axis gives the overall picture. Reading them together tells you whether attendance is the binding constraint for a particular school, or whether the issues lie elsewhere.

M0 is already implemented in `model_experiments.qmd` L3270–3279:

```r
mod_M0 <- lmer(
  log(ATT8SCR) ~
    log(PTFSM6CLA1A) + log(PNUMEAL) +
    ks2_c + ADMPOL_PT + gorard_segregation +
    (1 | year_label) + (1 | OFSTEDRATING_1) + (1 | gor_name/LANAME),
  data = stage2_data, REML = TRUE, ...)
```

---

## 3. Why M0 is conceptually sound for the Y-axis

The natural objection: *"If we don't control for absence, won't schools with high structural absence be unfairly penalised?"*

This would be a problem if structural absence variation weren't already captured by the other covariates. But it largely is — and here's why.

### The Frisch-Waugh insight

In the **headline model** (which does include `log(PERCTOT)`), the absence coefficient is not estimated from all absence variation. It's estimated only from the variation in absence that the **other covariates cannot explain** — this is the Frisch-Waugh-Lovell theorem. Once FSM, EAL, KS2, segregation, and the LA/year random effects have absorbed the structural drivers of absence, what remains in `log(PERCTOT)` is predominantly **school-controllable** variation (attendance management, pastoral follow-up, ethos) plus unmeasured noise.

This tells us something important: the structural part of absence is already being absorbed by the intake covariates and the LA random effects. It was always doing double duty — driving both the FSM/EAL/KS2 coefficients and the LA intercepts.

### What this implies for M0

When M0 omits absence entirely, the structural part of absence is still captured — it was always being absorbed by FSM/EAL/KS2 and the LA random effects anyway. What M0 leaves in the residual is the **total school contribution**: predominantly teaching quality, leadership, pastoral care, attendance management, and the school-controllable part of absence. Some structural absence variation will escape the covariates and leak into the residual, but the Stage 1 model already shows that the intake variables explain most absence variation, so the leakage is modest.

---

## 4. An even sharper version (if desired)

If you want a two-stage approach that's **both** conceptually right **and** econometrically clean, give Stage 1 predictors that aren't in Stage 2 — genuine exclusion restrictions.

The census neighbourhood variables already in `scratch_add_nondisadvantaged_decomp.R` (`z_l4plus_k150`, `z_ltsick_k150`, `z_unemp_k150`) are natural candidates: they predict absence through area-level health and deprivation channels but don't directly affect school-level attainment once FSM/EAL/KS2 are controlled. With these in Stage 1 but not Stage 2, `log_expected_absence` would contain variation that **isn't** a linear combination of the Stage 2 covariates, breaking the collinearity.

This is a more substantial code change and would need careful justification of the exclusion restriction, so M0 may be the pragmatic choice — but it's worth mentioning in the supplementary as a direction for refinement.

---

## 5. What changes in the paper

### Code change (`RPE_Paper.qmd`, `quadrant-fits-setup` chunk, L990–996)

Replace `mod_decomp` with the M0 specification. The quadrant data construction (L1000–1013) stays the same.

### Prose change (section 5.5, ~2 paragraphs)

Replace the "two-stage decomposition" framing with something like:

> *"Two parallel models isolate these dimensions. First, we model school-level absence on the variables we treat as exogenous (FSM, EAL, mean KS2 prior attainment, segregation, plus place and year random effects): the residual for each school-year is its excess absence — the part beyond what its intake would predict. This is the school's specific impact on attendance.*
>
> *Second, we fit the attainment model without any absence control and without workforce predictors. The residual is a contextualised value-added measure: the school's total contribution to attainment, encompassing teaching quality, leadership, curriculum, pastoral care, and attendance management. Because the structural drivers of absence are already absorbed by the intake covariates and area random effects, the value-added residual is not materially contaminated by structural absence that lies outside the school's control.*
>
> *Plotting these two signals against one another — total value-added on the vertical axis, excess absence on the horizontal — places each school into one of four diagnostic quadrants."*

### Supplementary material

No changes needed. The M0/M1/M2 comparison stays as a sensitivity analysis.

### What does NOT change

- Figure 6 and the quadrant assignments
- Brighton & Hove's position in Q2
- The "attendance is the lever" narrative
- The Stage 1 absence model and excess absence calculation
- Any other section of the paper

---

## 6. How to read the revised quadrant plot

The four quadrants still tell the same policy stories, but it helps to be precise about what each axis now represents:

| Quadrant | Y-axis (total value-added) | X-axis (school impact on absence) | Policy story |
|----------|---------------------------|-----------------------------------|--------------|
| **Q1** (top-left) | High overall effectiveness | Better attendance than intake predicts | Exemplar — strong school with attendance helping. Look here for practice worth sharing. |
| **Q2** (top-right) | High overall effectiveness | Worse attendance than intake predicts | Impressive despite attendance — strong teaching/leadership is compensating. Fixing attendance would compound an already-strong signal. |
| **Q3** (bottom-right) | Low overall effectiveness | Worse attendance than intake predicts | Attendance is the clearest single lever — most natural target for council-led attendance interventions. |
| **Q4** (bottom-left) | Low overall effectiveness | Better attendance than intake predicts | Attendance isn't the problem — issues lie in teaching, curriculum, or other school factors. The simple "fix attendance" story isn't available. |

Brighton & Hove's schools sit predominantly in **Q2**: delivering more than their intake predicts, while carrying more absence than their intake explains. The pedagogical work is the more impressive for it; closing the excess absence gap would compound directly onto an already-strong value-added signal.

---

## 7. Why this matters for review

A quantitative reviewer who traces the algebra of Stage 2 will spot the shared-random-effects collinearity. The parallel-model framing pre-empts this entirely, is actually simpler to explain to a policy audience, and — because structural absence is already captured by the intake covariates — achieves the same diagnostic purpose without the econometric vulnerability.

---

*This note was developed through a systematic examination of the actual R code in the repository, with input from both Claude and Gemini. The initial analysis (Claude) defended the two-stage approach; Gemini's counter-argument on the shared random-effects structure was correct on the technical merits; further discussion refined the M0 justification via the Frisch-Waugh argument. The recommendation above is the consensus position.*
