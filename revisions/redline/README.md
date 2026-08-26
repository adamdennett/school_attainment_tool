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
