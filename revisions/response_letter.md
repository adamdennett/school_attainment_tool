# Response to reviewers

**Manuscript:** *Pulling the Right Lever: School Attainment, Open Data Analytics and Local Education Policy in England*
**Journal:** Research Papers in Education

We are grateful to both reviewers for reading the paper so carefully. The revision
is, we think, considerably better for their comments --- in particular Reviewer 2's
question about the role of absence in the local-authority effect, which sent us
back to the data and produced a finding that now sits much closer to the centre of
the argument than it did before. We respond to each point in turn below; reviewer
comments are in italics, with our response following. Section references are to the
revised manuscript.

---

## Reviewer 1

**1. Clarity on the "selective admissions" variable.**
*"greater clarity is needed regarding the inclusion of a variable labelled 'selective admissions' … whether this refers to grammar schools, partial selection, banding arrangements, aptitude-based selection, or faith-based oversubscription criteria, and … how prevalent such schools are within the sample."*

We agree this wasn't explained well enough, and we have added a clarifying passage
to section 3.2. The variable is the DfE's own performance-tables admissions
classification, which within our state-funded sample distinguishes three groups:
wholly selective state grammar schools admitting by entrance test (166 schools,
4.8% of the sample); non-selective schools sitting in otherwise selective areas
(234 schools, which form the reference category); and all other non-selective
schools (the remaining ~88%). We now make clear that what we are estimating is
therefore the effect of *full* academic selection, benchmarked against the most
appropriate comparator, and that it does *not* pick up partial selection, banding,
aptitude-based selection or faith-based oversubscription criteria --- all of which
fall into the "other non-selective" group and are consequently unmeasured here.
Faith schools in particular are not separately identified in the main model.

**2. Engage more explicitly with FSM and absence as the strongest predictors, and link to the admissions changes.**
*"the conclusion could reflect more directly on these findings and clarify how, if at all, the observed attainment patterns are linked to the post-2022 admissions policy changes … Greater discussion of the mechanisms through which admissions reforms might interact with FSM composition and attendance would strengthen the interpretation."*

We have done three things here. Firstly, section 4.3 now sets out the ordering of
the predictors explicitly. Two of them stand clear of the rest: mean KS2 prior
attainment carries the largest standardised coefficient for all pupils and for
non-disadvantaged pupils, while absence is largest for disadvantaged pupils. We
then draw what we think is the policy-relevant distinction between the two --- prior
attainment is inherited intake, fixed for a cohort already sitting in the school and
included as a control rather than as a lever, so absence is the largest factor
genuinely open to intervention --- and note that concentration of disadvantage is a
distant contributor whose sign depends on which pupil group you are looking at. We
flag this ordering as the central empirical message of the paper.

Secondly, section 6.1 now states explicitly the two mechanisms by which an admissions
reform actually reaches attainment: the *intended* pathway, in which changing FSM
composition has at most a small and possibly wrong-signed effect, and the
*unintended* pathway, in which longer journeys lead to higher absence and a large
negative effect. This makes concrete why we characterise the policy as pulling the
wrong lever.

Thirdly --- and this was prompted by the reviewer's phrase "observed attainment
patterns" --- we have been careful to say that what we model are *expected* effects
derived from national associations, not the *observed* effects of the 2023 or 2026
local reforms. Those post-date our outcome data, and the council's own commissioned
evaluation has yet to report.

---

## Reviewer 2

**Section 3.2 — Loss of efficiency from a school-level-only prior-attainment control.**
*"there will be a loss of efficiency in correcting for prior attainment … Without it, there is a danger of over-inflating your estimates."*

We accept this, and we now say so plainly in sections 3.2 and 6.7: because we don't
have pupil-level data, prior attainment is corrected only at school level, which is
a coarser adjustment than a pupil-and-school-level correction would be, and we can't
rule out some residual confounding of the intake coefficients as a result. We note
NPD/SRS-based replication as the obvious next step. We also address the concern
empirically through the robustness work described in the next response.

**Section 3.2 — Justify the log functional form; per-variable diagnostics.**
*"Please provide further justification of using the logarithmic functional form … A uniform log transformation across all variables assumes each requires the same correction … Reporting variable-specific diagnostics … would help justify this choice."*

We think we have identified what caused the concern here, and have clarified it: the
log transformation is *not* applied uniformly. Only the four skewed,
strictly-positive rate and count predictors are logged --- the disadvantage, absence
and EAL percentages, and teacher sickness days. The remaining predictors, *including
the prior-attainment control the reviewer specifically mentions*, enter linearly.
Section 3.2 now works through this variable by variable. The supplementary material
adds the diagnostics requested: for each logged predictor we compare logged, linear
and natural-spline treatments by AIC --- the log form is at or near best in each
case, and the spline buys no material improvement --- and we provide
component-plus-residual plots showing an approximately linear relationship on the
log scale.

**Section 3.2 — Only low prior attainment is included; why not mean KS2 score?**
*"Am I correct that only low prior attainment is included as a KS2 prior attainment control? This does not strike me as adequate. Is the average KS2 score not included in the school-level data? Why is just the low prior attainment percentage included? Is there a risk of inflated estimates elsewhere without this being well controlled for?"*

The reviewer was right on every count, and following this question through has
changed the paper. **We have adopted the mean KS2 scaled score as the
prior-attainment control throughout, in place of the low-attainment share.**

Firstly, we should correct a point of fact that our submitted version rather
obscured. For 2024-25 *no* prior-attainment measure is observed at all. Because the
KS2 assessments of 2019-20 and 2020-21 were cancelled during the pandemic, the
cohorts reaching the end of KS4 in 2024-25 and 2025-26 have no KS2 results of any
kind, and the DfE publishes neither prior-attainment measures nor Progress 8 for
those years --- in the raw performance tables every prior-attainment field is null.
Whichever measure one uses therefore has to be carry-forward imputed for that year,
and both are imputable on identical terms (3,221 rows for mean KS2, 3,222 for the
low-attainment share). Data availability was consequently never a reason to prefer
one over the other, and we should not have implied that it was. Sections 3.1 and 3.2
now state this explicitly, with a citation to the DfE's own statement.

On the substance, the reviewer's instinct was correct, and demonstrably so: the
low-attainment share is an inadequate control because it only ever sees one tail of
the intake distribution. It records how many pupils arrived behind, but says nothing
about how many arrived ahead. Two schools with identical low-attainer shares can
differ a great deal in the size of their upper tail, and a model that can't tell them
apart will under-adjust for schools whose intake advantage sits at the top.
Empirically, mean KS2 improves AIC by over 500 for the all-pupil model, and once it
is in, the low-attainment share is indistinguishable from zero (*t* = −0.45) --- mean
KS2 simply absorbs it.

The consequences, which we now report in section 3.2 and in the supplementary
material, are fourfold:

1. **The absence coefficient is essentially unchanged** --- it moves by under 3% in
   either pupil group. The central finding of the paper does not depend on this
   choice at all.
2. **The disadvantage-concentration coefficient attenuates by roughly a quarter**
   for all pupils. The low-attainment share had been leaving part of the intake
   difference between high- and low-FSM schools unmeasured, and the FSM term was
   quietly absorbing it. Properly controlled, disadvantage concentration is a weaker
   lever still, which strengthens rather than weakens our argument.
3. **The contested positive coefficient for disadvantaged pupils becomes much
   clearer**, with *t* rising from 2.1 to 5.6. Controlling prior attainment properly
   makes that finding more robust, not less (see also our response on section 4.3).
4. **The ordering of the standardised coefficients changes, and we have rewritten
   section 4.3 accordingly.** Because mean KS2 captures the whole intake distribution
   rather than one tail, it now carries the largest standardised coefficient for all
   pupils and for non-disadvantaged pupils, while absence remains largest for
   disadvantaged pupils. Our earlier statement that absence dominates in every group
   is therefore no longer accurate as written, and we have replaced it with an
   explicit distinction between inherited intake and actionable levers: prior
   attainment is fixed for a cohort already in secondary school and enters as a
   control, so absence remains the largest factor that is open to intervention. The
   argument of the paper, and the abstract's description of attendance as the most
   impactful *policy lever*, are unaffected. While making this change we also
   corrected a claim present in the submitted version, that absence explained "nearly
   half" of the variation for disadvantaged pupils. It accounts for roughly a third
   of the combined standardised effect, under the old specification as well as the
   new one.

One consequence needs flagging prominently, because it revises a headline figure.
Under the improved specification, Brighton and Hove's conditional local-authority
rank for disadvantaged attainment moves from 7th to **15th of 151** (and, without an
absence control, from 46th to 69th). The city remains within the strongest tenth of
authorities and the substantive argument of section 5.2 is unaffected --- if anything
the gap between the conditional and unconditional rankings widens slightly --- but
the earlier "7th" partly reflected the weaker control, and we have updated the
abstract, introduction, section 5.2 and section 6.5 accordingly rather than quietly
restating it.

Finally, on the related point about pupil-level correction: we accept that a
school-level aggregate remains a coarser adjustment than a pupil-and-school-level
correction would give, and we say so in sections 3.2 and 6.7. That limitation is
inherent to published school-level data and would need NPD access to resolve.

**Section 4.2 — Why is Ofsted a random effect rather than an ordinal fixed effect?**
*"Ofsted rating has only four ordered categories … I would have assumed that an ordinal fixed effect would be more appropriate here."*

This is a fair point. We now justify the choice in section 4.2 and, more usefully,
show in the supplementary material that it makes no substantive difference: treating
Ofsted as a categorical fixed effect leaves every other coefficient unchanged to
three or four significant figures, and does not move Brighton and Hove's rank. We
report the fixed-effect per-band coefficients in section 4.2 --- relative to
Outstanding, roughly 1.9, 4.0 and 4.5 Attainment 8 points lower for Good, Requires
Improvement and Inadequate respectively --- and note that because the scale flattens
markedly at the bottom, a *linear ordinal* term would misrepresent it. That is an
argument for the categorical form rather than the ordinal one the reviewer suggests.
We also now treat Ofsted explicitly as a contextual control rather than a causal
factor, since inspection judgements are formed with sight of a school's results.
Consistent with that, dropping Ofsted entirely *increases* the absence coefficient by
12--15%, so its inclusion makes our headline absence estimate conservative if
anything.

**Section 4.2 — Make the magnitude of the non-linear effects concrete.**
*"Would it be possible to show this graphically or by giving examples at selected points within the scale? It is hard to concretely grasp the magnitude of this."*

We have added a forward reference from section 4.2 to Figure 4 in section 5.3, which
already does exactly this: it translates the coefficients into the expected
Attainment 8 gain from a one-percentage-point change at each point along the absence
and disadvantage distributions. The new per-band Ofsted figures noted above give a
further concrete example.

**Section 4.3 — "Differing effects" versus "differing pupils".**
*"disadvantaged pupils … are a heterogeneous group … Have you any reason to favour 'differing effects, equivalent pupils' over the 'differing pupils' … interpretation?"*

We don't, and we now say so directly in section 4.3. Because the FSM binary can't
distinguish the depth or duration of disadvantage, the disadvantaged pupils in a
high-concentration school may well differ systematically from those in a
low-concentration school, and we cannot adjudicate between the two interpretations
with aggregate data. We set out the two pieces of partial evidence we do have ---
robustness across measures makes a pure measurement artefact less likely, and the
stronger association on the vocational component is suggestive of a genuine
specialisation mechanism --- without overstating either, and we emphasise that our
policy argument holds under either interpretation. There is no evidence that
concentrating disadvantaged pupils harms their attainment, and hence no support for
the premise that de-concentrating them would improve it.

**Section 5.2 — Is the Brighton and Hove result justifiable when controlling for absence?**
*"the result about B&H performance for disadvantaged pupils … is based on a calculation that has controlled for absence … Would it be possible to report with and without this factor? … Given the very low attendance in the city, this is a pivotal point."*

We are grateful for this comment, which has improved the paper more than any other.
We now report the local-authority effect with and without an absence control in a new
table in section 5.2. The result is striking and we have made it central to the
argument: once absence is accounted for, the city's schools rank 15th of 151 for
disadvantaged value added --- that is, results relative to intake --- while without
that control they rank 69th and are statistically ordinary. (These rankings reflect
the mean KS2 control adopted in response to the reviewer's earlier point; under the
submitted specification they were 7th and 46th. The substantive result is unchanged,
and the gap between the two is slightly wider.)

Rather than treating this as something that undermines the finding, we now frame the
distance between the two rankings --- about 1.4 Attainment 8 points per disadvantaged
pupil --- as a direct measure of what the city's absence problem is costing its most
disadvantaged children. We also show, using the intake-predicted specification and
consistent with the reviewer's observation that absence "picks up both school and
contextual variation", that the city's excess absence is not explained by its intake,
and that its absence has, almost uniquely among English authorities, failed to fall
since the pandemic. The knock-on qualifications have been applied to the abstract,
the introduction and the retitled section 6.5, "Conditional performance and the cost
of absence". We have also noted that this reshuffling of local-authority rankings by
absence is a general phenomenon, not something peculiar to Brighton and Hove.

**Section 5.5 — How amenable is attendance to school/LA action, and is there an internal inconsistency?**
*"it was … controlled for in the estimates … therefore treated as a non-school factor, but in Section 5.5 the discussion is about schools addressing the issue … Could evidence be presented about how amenable to change attendance is … Or are there other (potentially social) interventions that should be mentioned?"*

This is exactly the tension that the absence decomposition in section 5.5 is meant to
resolve, and we have brought the discussion forward so that the reader meets it
earlier. Section 2.2 now includes a paragraph on how far attendance is amenable to
school-level action at all, drawing on the evidence that a substantial share of it
reflects circumstances beyond the school gate --- family poverty, physical and mental
ill-health, SEN, and the largely exogenous post-pandemic rise --- with the
school-controllable portion the smaller part. That is consistent with what the
decomposition finds, and we have tempered the recommendation accordingly:
authority-wide improvement is unlikely to come from school-led practice-sharing
alone, and more probably needs cross-departmental action spanning health, children's
services and family support alongside schools. This point is now made in sections
5.5 and 6.2 and again in the conclusion.

**Conclusion — academisation and reduced LA analytical capacity.**
*"academisation and reduced local authority resourcing since 2010 have significantly reduced LA analytical capacity … this leaves a gap for locality-wide analyses … Is this a fair point? Could it be made?"*

It is a fair point, and we have made it in section 6.6. We now note that the
analytical gap the paper describes is in part a structural consequence of post-2010
academisation and of reductions in LA central-services funding; that multi-academy
trusts have absorbed some of this function, but only for their own schools, leaving
no actor with both the remit and the capacity for genuinely locality-wide analysis;
and that Brighton and Hove illustrates the gap rather well. We have accordingly
reframed the recommendation away from "councils should invest" and toward closing a
*system-level* gap, whether through central funding or nationally maintained tooling,
and positioned the Policy Simulator as a prototype of exactly that kind of shared,
locality-level infrastructure.

**Minor points.**

- *P6: cite an example or review of the "large volume of literature".* Added review
  citations at that point (section 2.1).
- *P8: "Simon" Burgess and "S" Burgess both cited.* Corrected --- author names are now
  given in full and consistently throughout the bibliography, and we have removed a
  duplicate entry (a working-paper and published-article pair for the same study).
- *P11: "to general" typo.* Corrected (section 4.1).

---

## Additional changes not prompted by a specific comment

Five further changes were made in the course of the revision. Most respond to lines
of criticism the work has attracted since submission, and each of them seemed to us
to strengthen the paper rather than simply defend it.

**1. What the disadvantage coefficient does and does not measure (section 3.2).**
Having adopted mean KS2 at Reviewer 2's prompting, we became conscious that the
improvement carries an interpretive risk with it. A control spanning the whole intake
distribution absorbs more of the poverty signal than one capturing a single tail,
which is precisely why the disadvantage coefficient attenuates when we make the
switch. We have therefore added a paragraph saying plainly that the surviving
coefficient is a *within-intake* comparison and must not be read as an estimate of
how much poverty matters to attainment, since prior attainment is itself in
substantial part the accumulated product of disadvantage. The total contribution of
poverty is larger than that residual coefficient implies, and we say so. This guards
against a misreading our own results might otherwise invite, and it makes the much
narrower quantity our models actually identify --- the marginal association with
school-level *concentration*, holding intake constant --- unambiguous.

**2. Prior attainment and Ofqual's 'comparable outcomes' (section 4.2).** A reader
might reasonably wonder whether the KS2-to-GCSE association is partly mechanical,
given that Ofqual calibrates national GCSE grade boundaries against the incoming
cohort's KS2 profile. We have added a short passage explaining why it is not a
problem for us here: because that calibration is a national, year-to-year adjustment,
any mechanical scaling is absorbed by our year random effect, leaving the fixed
effect for mean KS2 to isolate the genuine within-year, between-school association
between inherited intake and attainment.

**3. The magnitude of school effects in context (section 6.1).** The submitted version
argued that redistribution is a weak lever without situating that claim in the
school-effectiveness literature, where it has a long pedigree. We now note that the
between-school share of variation in pupil outcomes has been put at roughly 8--15 per
cent ever since Coleman (1966) --- around a tenth in Smith and Tomlinson's English
study, a range confirmed across countries and phases by Teddlie and Reynolds --- and
that our own decomposition recovers the same order of magnitude. The point is that a
redistribution policy works on the smaller term while leaving the larger one
untouched. This turns an assertion of ours into a recovery of settled consensus,
which we think is both stronger and more honest.

**4. Two parallel models in place of a sequential decomposition (section 5.5).** The
submitted version built the value-added axis of Figure 6 from a second-stage model
that used stage 1's *expected* absence in place of raw absence. On re-examination
that specification is not defensible: expected absence is by construction a linear
combination of the intake predictors that already appear in the second stage, and
regressing it on those predictors together with LA and year indicators returns an
$R^2$ of 1.000 exactly. The coefficient on it is therefore only weakly identified, by
an artefact of random-effect shrinkage rather than by any information about absence.

We have replaced it with the simpler specification the design always implied:
adjust for what a school inherits, not for what it does. The absence model is
unchanged, and the attainment model now has no absence control and no workforce
predictors, so that attendance management and workforce decisions sit inside the
school's contribution rather than being stripped out as controls. Because the figure
uses only residuals, and collinearity affects coefficients rather than fitted values,
almost nothing changes: school-level value added correlates at 0.9995 across the two
specifications and 0.9% of schools shift quadrant. The supplementary material now
sets out the algebra, reports that sensitivity comparison in full, and explains why
we do not pursue the apparently cleaner route of giving stage 1 its own exclusion
restrictions --- the obvious candidates, our census neighbourhood measures, predict
attainment directly and so are not valid instruments. We also flag, in the manuscript
and in the code, that the second model's coefficients cannot be read as effects: with
absence omitted the disadvantage term absorbs it, roughly doubling, and every
disadvantage estimate we report comes from the headline specification instead.

**5. A note on inference with population data (section 6.7).** Our panel is a
population rather than a random sample, and the manuscript previously said nothing
about what that implies for the standard errors we report. We have added a short
passage setting out that we interpret them as capturing cohort-to-cohort stochastic
variation rather than sampling uncertainty; that the multilevel structure guards
against over-reading the resulting instability, particularly in smaller schools; and
that, consistent with the American Statistical Association's cautions on p-values, we
treat significance descriptively. No conclusion in the paper turns on a threshold
being crossed: the arguments are made in Attainment 8 points, they replicate across
four separate years, and they would stand unchanged if every test statistic were
removed.

---

We hope the reviewers find the revised manuscript responsive to their comments, and
we thank them again for feedback that has genuinely improved the paper.
