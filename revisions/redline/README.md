# Regenerating the tracked-changes document

`RPE_Paper_tracked_changes.docx` shows RPE_Paper_V1.qmd (as submitted) against
the current RPE_Paper.qmd as real Word tracked changes (accept/reject works).

Word's own Compare hangs on this document, so the redline is built with pandoc,
which writes genuine `w:ins` / `w:del` OOXML from `.insertion` / `.deletion`
spans.

## Steps (from project root)

1. `RPE_Paper_V1_render.qmd` is a copy of `RPE_Paper_V1.qmd` with
   `output-file: RPE_Paper_V1` and its two `readRDS` calls repointed at
   `data/_backup_pre_ks2switch/`. That backup holds the panel and models from
   *before* the switch from the low-attainer share to mean KS2, so V1 renders
   with the numbers it was submitted with rather than the current ones.

   `quarto render revisions/redline/RPE_Paper_V1_render.qmd --to docx`
   `quarto render RPE_Paper.qmd --to docx`

2. Convert both to one-paragraph-per-line markdown with Quarto's bundled pandoc
   (`C:/Program Files/Quarto/bin/tools/pandoc.exe`), `-t markdown-smart
   --wrap=none`, into `v1.md` / `v2.md`. Extract figure media from the new docx
   and renumber image paths to `media/fig1..6.png` in both files, so identical
   figures do not register as changes.

3. `Rscript redline.R <dir>` — paragraph-level LCS alignment, then word-level
   LCS within each changed paragraph, emitting `.insertion` / `.deletion` spans.
   Tables, figures and display maths pass through untracked (a word-level
   redline through a results table is unreadable, and spans break TeX).

4. `pandoc redline.md -f markdown -t docx --resource-path=<dir>
   --reference-doc=Manuscript_Anonymous.docx -o RPE_Paper_tracked_changes.docx`

## Current output

99 modified paragraphs, 22 inserted, 1 deleted — 197 insertions and 176
deletions at word level.

## Deliberately untracked changes

Two classes of difference are suppressed from the redline, so they appear as
plain new text with no revision marks:

1. **Dash-style changes** (em/en dash vs hyphen). The diff matches on a
   normalised key in which `—`, `–`, `--` and `---` all collapse to `-`, while
   the text emitted is always the raw text. For unchanged and matched
   paragraphs the *new* document's text is emitted, so the current dash style
   survives into the output.

2. **Anything on the old side carrying a disclosive URL** (`DISCLOSIVE` in
   `redline.R`, currently `adamdennett.github.io`). This is a double-blind
   submission: the submitted manuscript has already been anonymised, so a
   tracked deletion would show the reviewer the very URL that was removed. A
   changed paragraph whose old side carries the URL is emitted as new text
   only; a wholly deleted one is dropped. The run reports these as
   `untracked(disclosive)=N` — check that count matches the number of
   identifying footnotes in the old version (currently 7).

Verify after each build:

```bash
unzip -p RPE_Paper_tracked_changes.docx word/document.xml | grep -c adamdennett
```

This must return 0.

## Footnotes and tables

Two problems in the docx -> markdown -> docx round trip needed handling, both
now solved in `redline.R`:

**Footnotes were silently dropped.** Both versions number their notes from
`[^1]`, so interleaving them produced duplicate definitions. Worse, a
definition caught by the paragraph diff was wrapped in a revision span —
`[[^b1]: text]{.insertion}` — which is no longer a footnote definition at all,
so pandoc discarded it and every reference rendered as stray text. Labels are
now prefixed per source (`a` for the old version, `b` for the new), definitions
are treated as literal blocks that are never span-wrapped, and a final pass
strips references whose definition has gone (which happens when a disclosive
note is suppressed) and definitions nothing references. The output now carries
the same 23 footnote entries and 21 references as the manuscript itself.

**Tables came through as an unreadable wall of pipes.** Pandoc renders the
manuscript's tables as grid tables — ASCII art spanning many lines — and the
diff reassembles elements with a blank line between them, which split every row
into its own paragraph. Contiguous table lines (rows starting `|`, borders
starting `+-` or `+:`) are now collapsed into a single indivisible block. Since
table content is not tracked either way, each block is replaced by a short
italic note naming the table, e.g.

> *[Table 1: Stepwise progression to the full multilevel model for overall
> Attainment 8. — table content is not tracked; see the manuscript for the
> current version.]*

The body still carries the float placeholder showing where each table belongs,
and the manuscript has the real thing.

Verify after each build:

```bash
unzip -p RPE_Paper_tracked_changes.docx word/footnotes.xml | grep -c "<w:footnote "
unzip -p RPE_Paper_tracked_changes.docx word/document.xml | sed 's/<[^>]*>//g' | grep -c "———————"
```

The first should match the manuscript's own count; the second must be 0.

## Bibliography indent

Reference entries carry the `Bibliography` paragraph style in the manuscript,
which is where their hanging indent comes from. That style marker does not
survive the docx -> markdown conversion, so the entries were being written back
as ordinary body text with the indent lost. The reference block is now wrapped
in a div with `custom-style="Bibliography"`, which makes pandoc's docx writer
apply the style again and pick up the indent defined in `RPE_reference.docx`.

The redline's count runs one or two above the manuscript's, which is expected:
references deleted between versions appear in the redline as tracked deletions.

```bash
unzip -p RPE_Paper_tracked_changes.docx word/document.xml | grep -c 'w:pStyle w:val="Bibliography"'
```
