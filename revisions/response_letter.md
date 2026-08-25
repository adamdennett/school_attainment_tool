# Response to reviewers

**Manuscript:** *Pulling the Right Lever: School Attainment, Open Data Analytics and Local Education Policy in England*
**Journal:** Research Papers in Education

We thank both reviewers for their careful and constructive reading. The revision
has, we think, been materially strengthened by their comments --- in particular
Reviewer 2's point about the role of absence in the local-authority effect, which
led us to a finding that now sits closer to the centre of the paper's argument.
Below we respond to each point in turn; reviewer comments are quoted in italics
and our responses follow. Section references are to the revised manuscript.

---

## Reviewer 1

**1. Clarity on the "selective admissions" variable.**
*"greater clarity is needed regarding the inclusion of a variable labelled 'selective admissions' … whether this refers to grammar schools, partial selection, banding arrangements, aptitude-based selection, or faith-based oversubscription criteria, and … how prevalent such schools are within the sample."*

We agree this was under-explained and have added a clarifying passage to §3.2. The
variable is the DfE's own performance-tables admissions classification, which
within our state-funded sample distinguishes three groups: wholly selective state
grammar schools admitting by entrance test (166 schools, 4.8% of the sample);
non-selective schools in otherwise selective areas (234 schools, the reference
category); and all other non-selective schools (~88%). We now state that the
estimate is therefore of *full* academic selection, benchmarked against the most
appropriate comparator, and that it does *not* capture partial selection, banding,
aptitude-based selection or faith-based oversubscription criteria --- all of which
fall in the "other non-selective" group and are consequently unmeasured; faith
schools in particular are not separately identified in the main model.

**2. Engage more explicitly with FSM and absence as the strongest predictors, and link to the admissions changes.**
*"the conclusion could reflect more directly on these findings and clarify how, if at all, the observed attainment patterns are linked to the post-2022 admissions policy changes … Greater discussion of the mechanisms through which admissions reforms might interact with FSM composition and attendance would strengthen the interpretation."*

We have done three things. First, in §4.3 we now state the ordering of predictors
explicitly --- absence dominates, low prior attainment is second, and concentration
of disadvantage is a distant contributor whose sign depends on the pupil group ---
and flag that this ordering is the central empirical message of the paper. Second,
in §6.1 we have added an explicit statement of the two mechanisms by which an
admissions reform reaches attainment: the *intended* pathway (changing FSM
composition, which has at most a small and possibly wrong-signed effect) and the
*unintended* pathway (longer journeys → higher absence → a large negative effect).
This makes concrete why we characterise the policy as pulling the wrong lever.
Third --- and prompted by the reviewer's phrase "observed attainment patterns" ---
we have been careful to state that we model *expected* effects from national
associations, not *observed* effects of the 2023 or 2026 local reforms, which
post-date our outcome data and whose evaluation the council's own commissioned work
has yet to report.

---

## Reviewer 2

**§3.2 — Loss of efficiency from a school-level-only prior-attainment control.**
*"there will be a loss of efficiency in correcting for prior attainment … Without it, there is a danger of over-inflating your estimates."*

We accept this and now say so explicitly in §3.2 and §6.7: because we lack
pupil-level data, prior attainment is corrected only at the school level, a coarser
adjustment than a pupil-and-school-level correction, and some residual confounding
of the intake coefficients cannot be ruled out. We note NPD/SRS-based replication as
the natural next step. We also address the concern empirically via the robustness
check described in the next response.

**§3.2 — Justify the log functional form; per-variable diagnostics.**
*"Please provide further justification of using the logarithmic functional form … A uniform log transformation across all variables assumes each requires the same correction … Reporting variable-specific diagnostics … would help justify this choice."*

We have clarified a point that we suspect caused the concern: the log transformation
is *not* applied uniformly. Only the four skewed, strictly-positive rate/count
predictors (the disadvantage, absence and EAL percentages and teacher sickness
days) are logged; the remaining predictors, *including the low-prior-attainment
percentage the reviewer specifically mentions*, enter linearly. §3.2 now states this
variable by variable. The supplementary material adds the diagnostics requested: for
each logged predictor we compare logged, linear and natural-spline treatments by
AIC (the log form is at or near best, and the spline offers no material
improvement), and we provide component-plus-residual (partial residual) plots
showing an approximately linear relationship on the log scale for each.

**§3.2 — Only low prior attainment is included; why not mean KS2 score?**
*"Is the average KS2 score not included in the school-level data? … Is there a risk of inflated estimates elsewhere without this being well controlled for?"*

The reviewer's question prompted us to set out the data constraint more precisely
than the submitted version did, and we are grateful for it. The mean KS2 score is
published for three of our four years, but the more important point --- which we
had not stated clearly --- is that for 2024-25 *no* prior-attainment measure is
observed at all. Because the KS2 assessments of 2019-20 and 2020-21 were cancelled
during the pandemic, the cohorts reaching the end of KS4 in 2024-25 and 2025-26
have no KS2 results, and the DfE publishes neither prior-attainment measures nor
Progress 8 for those years; in the raw performance tables every prior-attainment
field is null for that year. The low-prior-attainment share we use for 2024-25 is
therefore carry-forward imputed from each school's most recent observed year and
flagged as such. §3.2 now states this explicitly, with a citation to the DfE's
statement, and §3.1 now names prior attainment among the imputed variables rather
than referring only to workforce and Ofsted data.

On the substantive question of whether estimates are inflated: as a robustness
check (supplementary material) we refit on the three-year sub-sample where prior
attainment is genuinely observed, adding mean KS2 score alongside the
low-prior-attainment share. The absence coefficient --- the paper's central
quantity --- moves by under 3%, while the FSM coefficient attenuates by about a
third, which reinforces rather than weakens our argument that concentration of
disadvantage is a weak lever. We report this in §3.2.

**§4.2 — Why is Ofsted a random effect rather than an ordinal fixed effect?**
*"Ofsted rating has only four ordered categories … I would have assumed that an ordinal fixed effect would be more appropriate here."*

This is a fair point. We now justify the choice in §4.2 and, more importantly, show
in the supplementary material that it does not matter substantively: treating Ofsted
as a categorical fixed effect leaves every other coefficient unchanged to three or
four significant figures and does not move Brighton and Hove's rank. We report the
fixed-effect per-band coefficients in §4.2 (relative to Outstanding, roughly −2.2,
−4.2 and −4.7 Attainment 8 points for Good, Requires Improvement and Inadequate),
and note that because the scale flattens at the bottom a *linear ordinal* term would
misrepresent it --- an argument for the categorical rather than ordinal form the
reviewer suggested. We also now treat Ofsted explicitly as a contextual control
rather than a causal factor, since inspection judgements are formed with sight of
results; consistent with this, dropping Ofsted entirely *increases* the absence
coefficient by 13–16%, so its inclusion makes our headline absence estimate
conservative.

**§4.2 — Make the magnitude of the non-linear effects concrete.**
*"Would it be possible to show this graphically or by giving examples at selected points within the scale? It is hard to concretely grasp the magnitude of this."*

We have added a forward reference from §4.2 to Figure 4 (§5.3), which already
translates the coefficients into the expected Attainment 8 gain from a
one-percentage-point change at each point along the absence and disadvantage
distributions --- the concrete, per-point magnitudes the reviewer asks for. The new
per-band Ofsted figures in §4.2 (above) give a further concrete example.

**§4.3 — "Differing effects" versus "differing pupils".**
*"disadvantaged pupils … are a heterogeneous group … Have you any reason to favour 'differing effects, equivalent pupils' over the 'differing pupils' … interpretation?"*

We do not, and we now say so directly in §4.3. Because the FSM binary cannot
distinguish the depth or duration of disadvantage, the disadvantaged pupils in a
high-concentration school may differ systematically from those in a low-concentration
school, and we cannot adjudicate between the two interpretations with aggregate data.
We note the two pieces of partial evidence (robustness across measures makes a pure
measurement artefact less likely; the stronger association on the vocational
component is suggestive of a genuine specialisation mechanism) without overstating
them, and emphasise that our policy argument holds under either interpretation:
there is no evidence that concentrating disadvantaged pupils harms their attainment,
and hence no support for the premise that de-concentration would improve it.

**§5.2 — Is the Brighton and Hove result justifiable when controlling for absence?**
*"the result about B&H performance for disadvantaged pupils … is based on a calculation that has controlled for absence … Would it be possible to report with and without this factor? … Given the very low attendance in the city, this is a pivotal point."*

We are grateful for this comment, which has improved the paper. We now report the
local-authority effect with and without an absence control in a new table (§5.2).
The result is striking and we have made it central: conditional on absence, Brighton
and Hove ranks 7th of 151 for disadvantaged attainment; *without* an absence control
it ranks 46th and is no longer statistically distinguishable from the national
average. Rather than treat this as undermining the finding, we now frame the gap
between the two rankings --- about 1.3 Attainment 8 points per disadvantaged pupil ---
as a direct measure of what the city's severe absence problem costs its most
disadvantaged children. We also show, using the intake-predicted specification
(consistent with the reviewer's observation that absence "picks up both school and
contextual variation"), that the city's excess absence is not explained by its
intake, and that its absence has, almost uniquely, failed to fall since the pandemic.
The knock-on qualifications have been applied to the abstract, introduction and the
retitled §6.5 ("Conditional performance and the cost of absence"). We have also noted
that this reshuffling of local-authority rankings by absence is general, not specific
to Brighton and Hove.

**§5.5 — How amenable is attendance to school/LA action, and is there an internal inconsistency?**
*"it was … controlled for in the estimates … therefore treated as a non-school factor, but in Section 5.5 the discussion is about schools addressing the issue … Could evidence be presented about how amenable to change attendance is … Or are there other (potentially social) interventions that should be mentioned?"*

This is exactly the tension our two-stage decomposition is designed to resolve, and
we have brought it forward so the reader meets it earlier. §2.2 now includes a
paragraph on how far attendance is amenable to school-level action, drawing on
evidence that a substantial share reflects circumstances beyond the school gate
(family poverty, physical and mental ill-health, SEN, and the largely exogenous
post-pandemic rise), with the school-controllable portion the smaller part. This is
consistent with the decomposition's finding, and we have accordingly tempered the
recommendation: authority-wide improvement is unlikely to come from school-led
practice-sharing alone and more probably requires cross-departmental action spanning
health, children's services and family support alongside schools --- a point now
made in §5.5, §6.2 and the conclusion.

**Conclusion — academisation and reduced LA analytical capacity.**
*"academisation and reduced local authority resourcing since 2010 have significantly reduced LA analytical capacity … this leaves a gap for locality-wide analyses … Is this a fair point? Could it be made?"*

It is a fair point and we have made it (§6.6). We now note that the analytical gap
the paper addresses is in part a structural consequence of post-2010 academisation
and reductions in LA central-services funding; that multi-academy trusts have
absorbed some of this function but only for their own schools, leaving no actor with
both the remit and the capacity for locality-wide analysis; and that Brighton and
Hove illustrates the gap precisely. We have accordingly reframed the recommendation
away from "councils should invest" toward closing a *system-level* gap through
central funding or nationally maintained tooling, and positioned the Policy Simulator
as a prototype of exactly that kind of shared, locality-level infrastructure.

**Minor points.**

- *P6: cite an example or review of the "large volume of literature".* Added review
  citations at that point (§2.1).
- *P8: "Simon" Burgess and "S" Burgess both cited.* Corrected --- author names are now
  given in full and consistently throughout the bibliography, and we removed a
  duplicate entry (a working-paper/published-article pair for the same study).
- *P11: "to general" typo.* Corrected (§4.1).

---

We hope the reviewers find the revised manuscript responsive to their comments, and
we thank them again for feedback that has strengthened the paper.
