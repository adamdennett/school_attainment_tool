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

We agree this was under-explained and have added a clarifying passage to section 3.2. The
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

We have done three things. First, in section 4.3 we now state the ordering of predictors
explicitly. Two predictors stand clear of the rest: mean KS2 prior attainment has
the largest standardised coefficient for all pupils and for non-disadvantaged
pupils, while absence is largest for disadvantaged pupils. We draw the
policy-relevant distinction between them --- prior attainment is inherited intake,
fixed for a cohort already in school and included as a control rather than a
lever, so absence is the largest factor genuinely open to intervention --- and
note that concentration of disadvantage is a distant contributor whose sign
depends on the pupil group. We flag this ordering as the central empirical
message of the paper. Second,
in section 6.1 we have added an explicit statement of the two mechanisms by which an
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

**Section 3.2 — Loss of efficiency from a school-level-only prior-attainment control.**
*"there will be a loss of efficiency in correcting for prior attainment … Without it, there is a danger of over-inflating your estimates."*

We accept this and now say so explicitly in section 3.2 and section 6.7: because we lack
pupil-level data, prior attainment is corrected only at the school level, a coarser
adjustment than a pupil-and-school-level correction, and some residual confounding
of the intake coefficients cannot be ruled out. We note NPD/SRS-based replication as
the natural next step. We also address the concern empirically via the robustness
check described in the next response.

**Section 3.2 — Justify the log functional form; per-variable diagnostics.**
*"Please provide further justification of using the logarithmic functional form … A uniform log transformation across all variables assumes each requires the same correction … Reporting variable-specific diagnostics … would help justify this choice."*

We have clarified a point that we suspect caused the concern: the log transformation
is *not* applied uniformly. Only the four skewed, strictly-positive rate/count
predictors (the disadvantage, absence and EAL percentages and teacher sickness
days) are logged; the remaining predictors, *including the prior-attainment control the reviewer
specifically mentions*, enter linearly. Section 3.2 now states this
variable by variable. The supplementary material adds the diagnostics requested: for
each logged predictor we compare logged, linear and natural-spline treatments by
AIC (the log form is at or near best, and the spline offers no material
improvement), and we provide component-plus-residual (partial residual) plots
showing an approximately linear relationship on the log scale for each.

**Section 3.2 — Only low prior attainment is included; why not mean KS2 score?**
*"Am I correct that only low prior attainment is included as a KS2 prior attainment control? This does not strike me as adequate. Is the average KS2 score not included in the school-level data? Why is just the low prior attainment percentage included? Is there a risk of inflated estimates elsewhere without this being well controlled for?"*

The reviewer was right on every count, and pursuing this question changed the
paper. **We have adopted mean KS2 scaled score as the prior-attainment control
throughout, in place of the low-attainment share.**

We should first correct a point of fact that our submitted version obscured. For
2024-25 *no* prior-attainment measure is observed at all: because the KS2
assessments of 2019-20 and 2020-21 were cancelled during the pandemic, the
cohorts reaching the end of KS4 in 2024-25 and 2025-26 have no KS2 results, and
the DfE publishes neither prior-attainment measures nor Progress 8 for those
years. In the raw performance tables, every prior-attainment field is null for
that year. Whichever measure is used therefore has to be carry-forward imputed
for 2024-25, and both are imputable on identical terms (3,221 rows for mean KS2,
3,222 for the low-attainment share). Data availability was consequently never a
reason to prefer one measure over the other, and we should not have implied
otherwise. Section 3.1 and section 3.2 now state this explicitly, with a citation to the DfE's
statement.

On the substance, the reviewer's instinct was correct: the low-attainment share
is an inadequate control, and demonstrably so. It observes only one tail of the
intake distribution --- it records how many pupils arrived behind, but is silent
on how many arrived ahead. Two schools with identical low-attainer shares can
differ substantially in the size of their upper tail, and a model that cannot
distinguish them will under-adjust for schools whose intake advantage sits at the
top. Empirically, mean KS2 improves AIC by over 500 for the all-pupil model, and
once it is included the low-attainment share is indistinguishable from zero
(*t* = −0.45): mean KS2 fully absorbs it.

The consequences, which we now report in section 3.2 and in the supplementary material,
are threefold:

1. **The absence coefficient is essentially unchanged** (under 3% in either pupil
   group). The paper's central finding does not depend on this choice.
2. **The disadvantage-concentration coefficient attenuates by roughly a quarter**
   for all pupils. The low-attainment share had been leaving part of the intake
   difference between high- and low-FSM schools unmeasured, and the FSM term was
   absorbing it. Properly controlled, disadvantage concentration is a weaker
   lever still --- which strengthens rather than weakens our argument.
3. **The contested positive coefficient for disadvantaged pupils becomes much
   clearer**, with *t* rising from 2.1 to 5.6. Better prior-attainment control
   makes that finding more robust, not less (see also our response on section 4.3).
4. **The ordering of standardised coefficients changes, and we have rewritten
   section 4.3 accordingly.** Because mean KS2 captures the whole intake distribution
   rather than one tail, it now carries the largest standardised coefficient for
   all pupils and for non-disadvantaged pupils; absence remains largest for
   disadvantaged pupils. Our earlier statement that absence dominates every group
   is therefore no longer accurate as written, and we have replaced it with an
   explicit distinction between inherited intake and actionable levers: prior
   attainment is fixed for a cohort already in secondary school and enters as a
   control, so absence remains the largest factor open to intervention. The
   paper's argument, and the abstract's description of attendance as the most
   impactful *policy lever*, are unaffected. While making this change we also
   corrected a claim, present in the submitted version, that absence explained
   "nearly half" of the variation for disadvantaged pupils: it accounts for
   roughly a third of the combined standardised effect, under both the old and
   the new specification.

One consequence should be flagged prominently because it revises a headline
figure. Under the improved specification, Brighton and Hove's conditional
local-authority rank for disadvantaged attainment moves from 7th to **15th of
151** (and, without an absence control, from 46th to 69th). The city remains
within the strongest tenth of authorities, and the substantive argument of section 5.2
is unaffected --- indeed the conditional-versus-unconditional gap widens slightly
--- but the earlier "7th" partly reflected the weaker control, and we have
updated the abstract, introduction, section 5.2 and section 6.5 accordingly.

Finally, on the related point about pupil-level correction: we accept that a
school-level aggregate remains a coarser adjustment than a pupil-and-school-level
correction would provide, and we now say so in section 3.2 and section 6.7. That limitation is
inherent to published school-level data and would require NPD access to resolve.

**Section 4.2 — Why is Ofsted a random effect rather than an ordinal fixed effect?**
*"Ofsted rating has only four ordered categories … I would have assumed that an ordinal fixed effect would be more appropriate here."*

This is a fair point. We now justify the choice in section 4.2 and, more importantly, show
in the supplementary material that it does not matter substantively: treating Ofsted
as a categorical fixed effect leaves every other coefficient unchanged to three or
four significant figures and does not move Brighton and Hove's rank. We report the
fixed-effect per-band coefficients in section 4.2 (relative to Outstanding, roughly −2.2,
−4.2 and −4.7 Attainment 8 points for Good, Requires Improvement and Inadequate),
and note that because the scale flattens at the bottom a *linear ordinal* term would
misrepresent it --- an argument for the categorical rather than ordinal form the
reviewer suggested. We also now treat Ofsted explicitly as a contextual control
rather than a causal factor, since inspection judgements are formed with sight of
results; consistent with this, dropping Ofsted entirely *increases* the absence
coefficient by 13–16%, so its inclusion makes our headline absence estimate
conservative.

**Section 4.2 — Make the magnitude of the non-linear effects concrete.**
*"Would it be possible to show this graphically or by giving examples at selected points within the scale? It is hard to concretely grasp the magnitude of this."*

We have added a forward reference from section 4.2 to Figure 4 (section 5.3), which already
translates the coefficients into the expected Attainment 8 gain from a
one-percentage-point change at each point along the absence and disadvantage
distributions --- the concrete, per-point magnitudes the reviewer asks for. The new
per-band Ofsted figures in section 4.2 (above) give a further concrete example.

**Section 4.3 — "Differing effects" versus "differing pupils".**
*"disadvantaged pupils … are a heterogeneous group … Have you any reason to favour 'differing effects, equivalent pupils' over the 'differing pupils' … interpretation?"*

We do not, and we now say so directly in section 4.3. Because the FSM binary cannot
distinguish the depth or duration of disadvantage, the disadvantaged pupils in a
high-concentration school may differ systematically from those in a low-concentration
school, and we cannot adjudicate between the two interpretations with aggregate data.
We note the two pieces of partial evidence (robustness across measures makes a pure
measurement artefact less likely; the stronger association on the vocational
component is suggestive of a genuine specialisation mechanism) without overstating
them, and emphasise that our policy argument holds under either interpretation:
there is no evidence that concentrating disadvantaged pupils harms their attainment,
and hence no support for the premise that de-concentration would improve it.

**Section 5.2 — Is the Brighton and Hove result justifiable when controlling for absence?**
*"the result about B&H performance for disadvantaged pupils … is based on a calculation that has controlled for absence … Would it be possible to report with and without this factor? … Given the very low attendance in the city, this is a pivotal point."*

We are grateful for this comment, which has improved the paper. We now report the
local-authority effect with and without an absence control in a new table (section 5.2).
The result is striking and we have made it central: conditional on absence, Brighton
and Hove ranks 15th of 151 for disadvantaged attainment; *without* an absence control
it ranks 69th and is no longer statistically distinguishable from the national
average. (These rankings reflect the mean KS2 prior-attainment control adopted in
response to the reviewer's earlier point; under the submitted specification they
were 7th and 46th respectively. The substantive result is unchanged, and the gap
between the two rankings is slightly wider.) Rather than treat this as undermining
the finding, we now frame the gap
between the two rankings --- about 1.4 Attainment 8 points per disadvantaged pupil ---
as a direct measure of what the city's severe absence problem costs its most
disadvantaged children. We also show, using the intake-predicted specification
(consistent with the reviewer's observation that absence "picks up both school and
contextual variation"), that the city's excess absence is not explained by its
intake, and that its absence has, almost uniquely, failed to fall since the pandemic.
The knock-on qualifications have been applied to the abstract, introduction and the
retitled section 6.5 ("Conditional performance and the cost of absence"). We have also noted
that this reshuffling of local-authority rankings by absence is general, not specific
to Brighton and Hove.

**Section 5.5 — How amenable is attendance to school/LA action, and is there an internal inconsistency?**
*"it was … controlled for in the estimates … therefore treated as a non-school factor, but in Section 5.5 the discussion is about schools addressing the issue … Could evidence be presented about how amenable to change attendance is … Or are there other (potentially social) interventions that should be mentioned?"*

This is exactly the tension our two-stage decomposition is designed to resolve, and
we have brought it forward so the reader meets it earlier. Section 2.2 now includes a
paragraph on how far attendance is amenable to school-level action, drawing on
evidence that a substantial share reflects circumstances beyond the school gate
(family poverty, physical and mental ill-health, SEN, and the largely exogenous
post-pandemic rise), with the school-controllable portion the smaller part. This is
consistent with the decomposition's finding, and we have accordingly tempered the
recommendation: authority-wide improvement is unlikely to come from school-led
practice-sharing alone and more probably requires cross-departmental action spanning
health, children's services and family support alongside schools --- a point now
made in section 5.5, section 6.2 and the conclusion.

**Conclusion — academisation and reduced LA analytical capacity.**
*"academisation and reduced local authority resourcing since 2010 have significantly reduced LA analytical capacity … this leaves a gap for locality-wide analyses … Is this a fair point? Could it be made?"*

It is a fair point and we have made it (section 6.6). We now note that the analytical gap
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
  citations at that point (section 2.1).
- *P8: "Simon" Burgess and "S" Burgess both cited.* Corrected --- author names are now
  given in full and consistently throughout the bibliography, and we removed a
  duplicate entry (a working-paper/published-article pair for the same study).
- *P11: "to general" typo.* Corrected (section 4.1).

---

## Additional changes not prompted by a specific comment

Three further additions were made in the course of the revision. Each responds to
a line of criticism the work has attracted since submission, and each seemed to us
to strengthen the paper rather than merely defend it.

**1. What the disadvantage coefficient does and does not measure (section 3.2).** Having
adopted mean KS2 as the prior-attainment control at Reviewer 2's prompting, we
became conscious that the improvement carries an interpretive risk. A control
spanning the whole intake distribution absorbs more of the poverty signal than one
capturing a single tail --- which is exactly why the disadvantage coefficient
attenuates when we make the switch. We have therefore added a paragraph stating
plainly that the surviving coefficient is a *within-intake* comparison and must not
be read as an estimate of how much poverty matters to attainment, since prior
attainment is itself in substantial part the accumulated product of disadvantage.
The total contribution of poverty is larger than that residual coefficient implies,
and we say so explicitly. This guards against a misreading our own results might
otherwise invite, and it makes the narrower quantity our models actually identify
--- the marginal association with school-level *concentration*, holding intake
constant --- unambiguous.

**2. The magnitude of school effects in context (section 6.1).** The submitted version
argued that redistribution is a weak lever without situating that claim in the
school-effectiveness literature, where it has a long pedigree. We now note that the
between-school share of variation in pupil outcomes has been put at roughly 8--15
per cent since Coleman (1966) --- around a tenth in Smith and Tomlinson's English
study, a range confirmed internationally by Teddlie and Reynolds --- and that our
own decomposition recovers the same order of magnitude. The point is that a
redistribution policy operates on the smaller term while leaving the larger one
untouched. This converts an assertion of ours into a recovery of settled consensus,
which we think is the stronger and more honest framing.

**3. A note on inference with population data (section 6.7).** Our panel is a population
rather than a random sample, and the manuscript previously said nothing about what
this implies for the standard errors we report. We have added a short passage
setting out that we interpret them as capturing cohort-to-cohort stochastic
variation rather than sampling uncertainty; that the multilevel structure guards
against over-reading the resulting instability, particularly for small schools; and
that, consistent with the American Statistical Association's cautions on p-values,
we treat significance descriptively. No conclusion in the paper turns on a
threshold: the arguments are made in Attainment 8 points, replicate across four
years, and would stand unchanged with every test statistic removed.

---

We hope the reviewers find the revised manuscript responsive to their comments, and
we thank them again for feedback that has strengthened the paper.
