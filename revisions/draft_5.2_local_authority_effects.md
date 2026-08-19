# Draft revision — §5.2 Local authority effects

Addresses **R2.7** (with/without absence control) and folds in the absence-trend
finding. Also carries the knock-on edits to the abstract, intro and §6.5.

This is prose for review, not yet pasted into `JEP_Paper_tight.qmd`. Numbers are
from `revisions/r27_output.txt` and `revisions/r23_output.txt`.

---

## Figure decision first

The reviewer asked for the result "with and without this factor taken into
account". Two ways to give it:

- **Option A (recommended): a small comparison table** in §5.2 giving B&H's
  rank and effect under three specifications, plus a one-line note on how much
  LA rankings move in general. Cheapest, clearest, and the numbers are the point.
- **Option B: a two-panel caterpillar** (with vs without absence control), B&H
  highlighted in each. More visual, but the individual LA labels get busy and
  the story is really two numbers, not 300.

Draft below assumes **Option A** — keep the existing single caterpillar
(`fig-caterpillar`) as the conditional view, add a compact table beneath it.

Proposed table (`tbl-la-absence`):

| Specification | B&H effect | 95% CI | Rank (of 151) | ATT8 points |
|---|---|---|---|---|
| No absence control | +0.018 | −0.022 to 0.058 | 46th (n.s.) | +0.7 |
| Absence controlled (main model) | +0.050 | 0.015 to 0.086 | 7th | +2.0 |
| Intake-predicted absence controlled | +0.017 | −0.023 to 0.057 | 45th (n.s.) | +0.7 |

*(Disadvantaged-pupil outcome. "Intake-predicted absence" is the stage-1
expected-absence measure from §5.5 fitted without an LA term, i.e. the part of
absence that intake alone predicts.)*

---

## Draft prose

> @fig-caterpillar shows the local-authority random intercepts for
> disadvantaged-pupil attainment from the multilevel model. Conditional on the
> structural factors in that model --- including school absence --- Brighton and
> Hove ranks **7th of 151 local authorities**, a positive effect of +0.050 on
> the log scale, equivalent to roughly **+2.0 Attainment 8 points** above what
> those factors would predict. On the same conditional basis the city ranks 5th
> for non-disadvantaged pupils and 4th for all pupils. This was unknown at the
> time of the 2024 consultation and entirely absent from a public narrative
> framed around the raw attainment gap alone.
>
> That conditional ranking, however, depends on an important modelling choice
> that deserves to be made explicit, because it turns out to carry most of the
> result. Controlling for absence estimates the city's performance *given* the
> attendance its pupils actually record --- and Brighton and Hove has the worst
> attendance record of almost any authority in England. @tbl-la-absence reports
> the disadvantaged-attainment effect with and without that control. Without any
> absence term, Brighton and Hove falls from 7th to **46th of 151** and its
> effect is no longer statistically distinguishable from the national average.
> The city's apparent excellence for disadvantaged pupils is, in this precise
> sense, conditional on setting its attendance problem aside.
>
> This is not an artefact peculiar to Brighton and Hove. Controlling for absence
> reshuffles the whole distribution of local-authority effects --- Bradford, for
> example, moves from 113th to 38th, and 14 authorities that appear
> significantly above average once absence is controlled are indistinguishable
> from average without it. Absence does a great deal of work in any model of
> attainment, and where an authority sits on it materially changes how its
> schools appear to perform. But the shift is unusually consequential for
> Brighton and Hove precisely because its attendance is so extreme.
>
> We regard the distance between these two numbers as the finding, rather than
> either number on its own. Conditional on the absence its pupils face, Brighton
> and Hove's schools are among the best in England for disadvantaged attainment;
> unconditionally they are unremarkable. **The gap between the two --- about 1.3
> Attainment 8 points per disadvantaged pupil --- is a direct measure of what the
> city's attendance problem costs its most disadvantaged children.** It is not a
> reason to discount the schools' underlying effectiveness; it is a reason to
> treat attendance as the single most valuable lever available to the city, a
> point we develop in §5.5 and quantify through the accelerating-returns analysis
> above.
>
> Two features of the city's absence confirm that this is a local problem rather
> than an inevitability of its intake. First, Brighton and Hove's disadvantage is
> unexceptional: its mean school-level free-school-meal rate (25.2%) is slightly
> *below* the national average (27.1%). When we model expected absence from
> intake alone (§5.5), the city's schools should record about 8.8% absence; they
> actually record 10.8%. Almost two percentage points of absence sit above what
> the intake predicts. Second, the problem has not been improving. Between
> 2021--22 and 2024--25, absence fell in 94% of English local authorities --- the
> national mean dropping by 0.85 percentage points as attendance recovered from
> the pandemic --- while Brighton and Hove's barely moved (−0.03 points), leaving
> it 143rd of 152 authorities for improvement. The city did not become an outlier
> because its absence rose; it became an outlier because, almost alone, its
> absence did not fall.

---

## Notes on the draft

1. **"46th" is the honest headline for disadvantaged pupils.** I've written it as
   the distance-between-two-numbers story rather than a retreat, per your steer.
   If you'd rather lead with the conditional 7th and treat the unconditional
   figure as a robustness caveat, the paragraphs can be reordered — but I think
   putting the gap front and centre is both more honest and rhetorically
   stronger.

2. **The intake-predicted row (45th) is doing quiet but important work.** It
   pre-empts the obvious rebuttal — "of course B&H looks worse without an absence
   control, all high-absence areas do." The intake-only specification shows the
   city's absence is *not* explained by deprivation, so the reattribution is not
   just mechanical. It also ties directly to the §5.5 decomposition, so it earns
   its place.

3. **151, not 152.** The disadvantaged-pupil model has 151 LAs (one authority has
   no reportable disadvantaged ATT8); the all-pupils model has 152. The current
   text says "152" for the disadvantaged figure — corrected to 151 above. Check
   every "of 152" against which outcome it refers to.

4. **"6% / +0.06 / 2 GCSE points" in the current text.** The existing sentence
   rounds the disadvantaged effect to +0.06; the fitted value is +0.050
   (≈ +2.0 points). I've used the precise figure. If the +0.06 came from a
   different model run, reconcile before finalising.

5. **All-pupils result is more robust — say so if space allows.** For all pupils
   the effect stays significant without the absence control (rank 3rd → 17th,
   still positive and significant). Worth one sentence so the reader sees the
   disadvantaged-specific fragility is not a wholesale collapse of the city's
   standing. Candidate: *"For all pupils the city's advantage is more robust to
   this choice, remaining significantly positive without an absence control
   (17th of 152); it is specifically the disadvantaged-pupil result that rests on
   it."*

---

## Knock-on edits triggered by this revision

- [ ] **Abstract (line 66)** — "second worst absence rate in England" → "second
      worst … in 2024--25" (true only in the latest year). Consider adding the
      trend: absence fell almost everywhere but not here.
- [ ] **Intro (line 220)** — "7th out of 152 … 5th out of 152 … once you account
      for the factors we know influence attainment" — add "conditional on the
      absence its schools record" so the intro doesn't assert a claim the body
      then qualifies. Fix 152 → 151 for the disadvantaged figure.
- [ ] **§6.5 title (line 1123)** — "What is driving Brighton and Hove's
      over-achievement?" presumes the conclusion. Retitle, e.g. *"Conditional
      performance and the cost of absence."*
- [ ] **§6.5 body (line 1125)** — "ranks 7th; 5th; 4th" and (line 1127) "one of
      the highest-performing LEAs in the country" — qualify as conditional, or
      reframe around the gap. This section is the natural home for the fuller
      version of the "cost of absence" argument.
- [ ] **Strategic reframe (your call)** — consider shifting the intro's framing
      from "council wrongly called a high-performing city a failure" to "council
      was right that there is a problem, wrong about its cause." Unconditionally
      the city *is* ordinary for disadvantaged attainment, so the "premise of
      failure" line is now harder to defend and the misdiagnosis story is
      stronger. Bigger change; flagged, not drafted.
- [ ] **Sync `pulling_the_right_lever.qmd` and the slide decks** — the "7th best"
      claim appears there too and must not contradict the paper.
- [ ] **Add the general LA-ranking-sensitivity finding** either here or in §6.5 —
      it is a genuine methodological contribution (caterpillar rankings are
      absence-sensitive) and worth a sentence beyond the B&H case.
