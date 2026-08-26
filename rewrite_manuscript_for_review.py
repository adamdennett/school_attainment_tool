"""
Rewrite the identifying URLs in Manuscript_Anonymous.docx so the document
is safe for double-blind submission. URLs to the author's GitHub-hosted
supplementary material are replaced with an OSF view-only-link placeholder
that the author can find-replace once the OSF link is generated.

Specifically:
  - https://adamdennett.github.io/school_attainment_tool/...   -> OSF placeholder
  - https://adamdennett.github.io/BH_Schools_Consultation/...  -> OSF placeholder
  - https://adam-dennett.shinyapps.io/School_Attainment_Policy_Simulator/
                                                              -> visible text only,
                                                                 with link removed

Run from project root:
    python rewrite_manuscript_for_review.py
"""

import re
import shutil
import zipfile
from pathlib import Path

SRC = Path("Manuscript_Anonymous.docx")
DST = Path("Manuscript_Anonymous_OSF.docx")
WORK = Path("C:/temp/manuscript_unpacked")  # re-extracted fresh each run below

# Anonymised OSF project URL — the actual view-only/public landing page
# that reviewers visit. All identifying links in the manuscript are
# retargeted to this URL.
OSF_URL = "https://osf.io/x82j3/overview"

# Mapping for the relationship Targets (footnotes.xml.rels). Anything in the
# manuscript NOT in this map is left alone (so doi.org, gov.uk, council
# links, etc. all stay intact).
TARGET_REWRITES = {
    # School attainment tool — points all paths at the OSF project root.
    # The visible footnote text gets a per-file hint added separately so
    # reviewers know which file inside `supplementary.zip` to open.
    "https://adamdennett.github.io/school_attainment_tool/":
        OSF_URL,
    "https://adamdennett.github.io/school_attainment_tool/data_overview.html":
        OSF_URL,
    "https://adamdennett.github.io/school_attainment_tool/model_results.html":
        OSF_URL,
    "https://adamdennett.github.io/school_attainment_tool/model_experiments.html":
        OSF_URL,
    "https://adamdennett.github.io/BH_Schools_Consultation/about.html":
        OSF_URL,
    "https://adamdennett.github.io/BH_Schools_Consultation/absence.html":
        OSF_URL,
    # Live Shiny tool — break the link; the visible text becomes the
    # withholding note.
    "https://adam-dennett.shinyapps.io/School_Attainment_Policy_Simulator/":
        "#",
}

# Visible-text URL replacements (inside <w:t> ... </w:t> in footnotes.xml).
# Each adamdennett URL is replaced with the OSF project URL plus a
# per-file hint so reviewers know which file inside supplementary.zip to
# open. Anchors in the original URL (e.g. #sec-two-stage-absence) are
# preserved as part of the filename hint — they'll resolve once the HTML
# file is opened in a browser after extracting the zip.

def _supplementary_hint(filename: str, anchor: str) -> str:
    """Build the OSF URL + per-file hint appended to a footnote."""
    if anchor:
        return f'{OSF_URL} (see `{filename}{anchor}` in `supplementary.zip`)'
    return f'{OSF_URL} (see `{filename}` in `supplementary.zip`)'


def _sat_replacer(match):
    filename = match.group(1)  # e.g. "model_experiments.html"
    anchor = match.group(2) or ""  # e.g. "#sec-two-stage-absence" or ""
    return _supplementary_hint(filename, anchor)


def _sat_index_replacer(_match):
    return _supplementary_hint("index.html", "")


def _bh_replacer(match):
    filename = match.group(1)
    anchor = match.group(2) or ""
    return _supplementary_hint(filename, anchor)


VISIBLE_URL_PATTERNS = [
    # School attainment tool — capture filename and optional anchor
    (re.compile(
        r'https?://adamdennett\.github\.io/school_attainment_tool/'
        r'([A-Za-z0-9_-]+\.html)(#[A-Za-z0-9_-]+)?'
    ), _sat_replacer),
    # School attainment tool index (no filename in URL)
    (re.compile(
        r'https?://adamdennett\.github\.io/school_attainment_tool/?(?=[\s<])'
    ), _sat_index_replacer),
    # BH_Schools_Consultation — same pattern
    (re.compile(
        r'https?://adamdennett\.github\.io/BH_Schools_Consultation/'
        r'([A-Za-z0-9_-]+\.html)(#[A-Za-z0-9_-]+)?'
    ), _bh_replacer),
    # Shiny app — replace the URL with the richer withholding note
    (re.compile(r'https?://adam-dennett\.shinyapps\.io/School_Attainment_Policy_Simulator/?'),
     'an interactive R Shiny web tool developed as part of this work. '
     'URL withheld for double-blind review; tool details, a walkthrough and '
     'access can be made available on request from the corresponding author '
     'via the editorial office.'),
]

# Footnote-text small fix: the rId3 footnote currently reads
# "...with links through to the source code on Github." The github link
# itself isn't in the footnote rels (it lives only as visible text), but
# the wording would lead a reviewer to look for an author-named GitHub
# repo. Soften.
PROSE_FIXES = [
    ("with links through to the source code on Github.",
     "with links through to the underlying source code (available on request)."),
]


def edit_footnotes_rels(path: Path) -> None:
    text = path.read_text(encoding="utf-8")
    changed = []
    for old, new in TARGET_REWRITES.items():
        if old in text:
            text = text.replace(f'Target="{old}"', f'Target="{new}"')
            changed.append(old)
    path.write_text(text, encoding="utf-8")
    print(f"  rels rewritten: {len(changed)} targets")
    for u in changed:
        print(f"    {u}")


def edit_footnotes_xml(path: Path) -> None:
    text = path.read_text(encoding="utf-8")
    visible_changes = 0
    for pat, repl in VISIBLE_URL_PATTERNS:
        new_text, n = pat.subn(repl, text)
        if n:
            visible_changes += n
            text = new_text
    prose_changes = 0
    for old, new in PROSE_FIXES:
        if old in text:
            text = text.replace(old, new)
            prose_changes += 1
    path.write_text(text, encoding="utf-8")
    print(f"  visible URL substitutions: {visible_changes}")
    print(f"  prose fixes:               {prose_changes}")


def repack(work_dir: Path, out_docx: Path) -> None:
    if out_docx.exists():
        out_docx.unlink()
    with zipfile.ZipFile(out_docx, "w", zipfile.ZIP_DEFLATED) as zf:
        for path in work_dir.rglob("*"):
            if path.is_file():
                arcname = path.relative_to(work_dir).as_posix()
                zf.write(path, arcname)
    print(f"  wrote {out_docx} ({out_docx.stat().st_size:,} bytes)")


def audit_output(out_docx: Path) -> None:
    """Sanity check: open the new docx and confirm no identifying URLs remain."""
    with zipfile.ZipFile(out_docx) as zf:
        for name in ("word/footnotes.xml", "word/_rels/footnotes.xml.rels",
                     "word/document.xml", "word/_rels/document.xml.rels"):
            try:
                data = zf.read(name).decode("utf-8", errors="ignore")
            except KeyError:
                continue
            hits = []
            for needle in ("adamdennett", "adam-dennett"):
                n = data.count(needle)
                if n:
                    hits.append(f"{needle}={n}")
            status = "  CLEAN" if not hits else "  ATTENTION: " + ", ".join(hits)
            print(f"  {name:35s}{status}")


def reextract_original(src_docx: Path, work_dir: Path) -> None:
    """Wipe the working directory and re-extract the original docx so each
    run of this script starts from a clean source — important when the
    replacement text changes between runs."""
    if work_dir.exists():
        shutil.rmtree(work_dir)
    work_dir.mkdir(parents=True, exist_ok=True)
    with zipfile.ZipFile(src_docx) as zf:
        zf.extractall(work_dir)


def main():
    print(f"Source docx:           {SRC.resolve()}")
    print(f"Working dir (refresh): {WORK}")
    print(f"Output docx:           {DST.resolve()}")
    print()
    print("0. Re-extracting original docx (clean slate)")
    reextract_original(SRC, WORK)
    print()
    print("1. Editing footnotes.xml.rels")
    edit_footnotes_rels(WORK / "word" / "_rels" / "footnotes.xml.rels")
    print()
    print("2. Editing footnotes.xml")
    edit_footnotes_xml(WORK / "word" / "footnotes.xml")
    print()
    print("3. Repacking docx")
    repack(WORK, DST)
    print()
    print("4. Post-rewrite audit on output docx")
    audit_output(DST)


if __name__ == "__main__":
    main()
