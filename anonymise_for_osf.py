"""
Anonymise selected docs/*.html files for upload to an OSF supplementary project
during double-blind peer review.

Strips:
  - meta author / title bylines that name the author
  - visible 'Adam Dennett' text
  - institutional identifiers (UCL, Bartlett CASA, University College London)
  - the header/footer logo blocks (UCL/AI4CI/UKRI) that brand the docs site
  - the 'View source on GitHub' footer link
  - cross-links to adamdennett.github.io -> rewritten to relative paths so the
    OSF-hosted files work as a flat set

Run from project root:
    python anonymise_for_osf.py
"""

import re
import shutil
from pathlib import Path

DST = Path("osf_supplementary")
DST.mkdir(parents=True, exist_ok=True)

# Files that need anonymising (referenced from the manuscript).
# Most live in docs/ but the two BH_Schools_Consultation pages are dropped
# into osf_supplementary/ by the user (they live in a separate repo).
# The dict maps each filename to the directory we should read from.
FILES = [
    ("index.html",            "docs"),
    ("data_overview.html",    "docs"),
    ("model_results.html",    "docs"),
    ("model_experiments.html", "docs"),
    # Already in osf_supplementary/; we read from there and write back in place.
    ("about.html",            "osf_supplementary"),
    ("absence.html",          "osf_supplementary"),
]

# Visible-text replacements (applied as plain substring substitutions).
# Order matters — longer/more specific phrases first so they aren't broken
# by earlier replacements catching a sub-phrase.
TEXT_REPLACEMENTS = [
    # Full author meta-content from BH_Schools_Consultation
    ("Professor Adam Dennett FRGS FAcSS, Professor of Urban Analytics, "
     "Bartlett Centre for Advanced Spatial Analysis, "
     "University College London - a.dennett@ucl.ac.uk",
     "[Author redacted for double-blind review]"),
    # Direct name mentions
    ("Adam Dennett with some assistance from Claude Code", "[Author redacted for review]"),
    ("Adam Dennett", "[Author]"),
    # Direct email
    ("a.dennett@ucl.ac.uk", "[email redacted]"),
    # Institutional identifiers
    ("Bartlett Centre for Advanced Spatial Analysis", "[Research Centre]"),
    ("UCL Centre for Advanced Spatial Analysis", "[Research Centre]"),
    ("University College London", "[Institution]"),
    # Author shorthand
    ("by Adam", "by [Author]"),
]

# Regex replacements for cross-link URLs — rewrite to relative paths
URL_REWRITES = [
    # adamdennett.github.io/school_attainment_tool/X.html -> X.html
    (re.compile(
        r'https?://adamdennett\.github\.io/school_attainment_tool/'
        r'([A-Za-z0-9_./-]+\.html)'
    ), r'\1'),
    # adamdennett.github.io/school_attainment_tool/ index -> index.html
    (re.compile(
        r'https?://adamdennett\.github\.io/school_attainment_tool/?(?=["\'>\s])'
    ), 'index.html'),
    # Personal site link from the docs homepage by-line
    (re.compile(r'https?://adamdennett\.co\.uk/?'), '#'),
    # Quarto-generated GitHub source/issue links in BH_Schools_Consultation
    (re.compile(r'https?://github\.com/adamdennett/[A-Za-z0-9_./#?=&-]*'), '#'),
    # Author's UCL profile page
    (re.compile(r'https?://profiles\.ucl\.ac\.uk/[A-Za-z0-9_.#?=&-]+'), '#'),
    # Cross-links to the author's earlier analysis projects (separate repos)
    (re.compile(
        r'https?://adamdennett\.github\.io/'
        r'(?:BH_Secondary_Admissions_Analysis|BH_Schools_2)/[A-Za-z0-9_./#-]*'
    ), '#'),
    # Within-set: BH_Schools_Consultation cross-links rewritten to relative
    # paths so the two pages still link to each other inside the OSF folder.
    (re.compile(
        r'https?://adamdennett\.github\.io/BH_Schools_Consultation/'
        r'([A-Za-z0-9_./#-]+\.html)'
    ), r'\1'),
    (re.compile(
        r'https?://adamdennett\.github\.io/BH_Schools_Consultation/?(?=["\'>\s])'
    ), 'about.html'),
    # adam-dennett.shinyapps.io -> remove Shiny app references entirely (live
    # tool deferred until after review)
    (re.compile(
        r'https?://adam-dennett\.shinyapps\.io/School_Attainment_Policy_Simulator/?'
    ), '[Policy Simulator tool — link withheld for review]'),
    # 'UCL Sans' is a UCL-branded typeface; rename the family so the embedded
    # base64 font data still renders but the family name no longer brands the
    # institution.
    (re.compile(r'"UCL Sans"'), '"Body Sans"'),
    (re.compile(r"'UCL Sans'"), "'Body Sans'"),
    # Logo image alt-text and src filenames that name the institution
    (re.compile(r'alt="UCL[^"]*"', flags=re.IGNORECASE), 'alt="logo"'),
    (re.compile(r'fonts/ucl-logo[^"\']*', flags=re.IGNORECASE), ''),
]

# Block-level scrub: remove the header-logos and footer-logos includes that
# brand the docs site with UCL / AI4CI / UKRI logos. These vary slightly
# between files but always sit between recognisable comment markers.
HEADER_SCRUB = [
    # Strip the entire UCL/AI4CI/UKRI logo strip wherever it appears.
    # Different files use different class names: header-logos (index),
    # page-header-logos and page-footer-logos (Quarto-generated pages).
    (re.compile(
        r'<div class="(?:page-)?(?:header|footer)-logos">.*?</div>',
        flags=re.DOTALL | re.IGNORECASE
    ), ''),
    # 'View source on GitHub' footer link
    (re.compile(
        r'<a[^>]*github\.com/adamdennett[^>]*>.*?</a>',
        flags=re.DOTALL | re.IGNORECASE
    ), ''),
    # Any remaining anchor whose href points at adamdennett.github.io but
    # that the relative-link rewrite missed (e.g. mailto-style or fragment)
    (re.compile(
        r'href="https?://adamdennett\.github\.io[^"]*"'
    ), 'href="#"'),
    # aria-label or alt naming the UCL logo directly (in case any survived
    # the outer-div strip)
    (re.compile(
        r'aria-label="UCL[^"]*"',
        flags=re.IGNORECASE
    ), 'aria-label="logo"'),
    # BH_Schools_Consultation: full meta author tag (Quarto puts this in
    # every rendered page). Stripped to a generic empty author.
    (re.compile(
        r'<meta name="author" content="[^"]*"\s*/?>',
        flags=re.IGNORECASE
    ), '<meta name="author" content="" />'),
    # Quarto "View source / Report an issue" toolbar block — Quarto inserts
    # these at the top of the page nav and at the foot. Both contain GitHub
    # repo URLs that name the author.
    (re.compile(
        r'<div class="toc-actions">.*?</div>',
        flags=re.DOTALL | re.IGNORECASE
    ), ''),
    # Sidebar "View source / Report an issue" dropdown items
    (re.compile(
        r'<a[^>]*class="dropdown-item sidebar-tools-collapse-item"[^>]*>.*?</a>',
        flags=re.DOTALL | re.IGNORECASE
    ), ''),
    # Quarto navigation tool linking to the personal site (bi-person-circle)
    (re.compile(
        r'<a[^>]*class="quarto-navigation-tool[^"]*"[^>]*>.*?</a>',
        flags=re.DOTALL | re.IGNORECASE
    ), ''),
    # Author bio paragraph in about.html — first-person and names
    # the institution explicitly. Replace the whole <p>...</p> block.
    (re.compile(
        r'<p>I am Professor of Urban Analytics.*?</p>',
        flags=re.DOTALL
    ), '<p>[Author bio redacted for double-blind review]</p>'),
    # Same bio reproduced inside the rendered code-listing on about.html
    (re.compile(
        r'<span id="cb1-25"[^>]*>.*?</span>',
        flags=re.DOTALL
    ), '<span id="cb1-25">[Author bio redacted for double-blind review]</span>'),
]


def anonymise(text: str) -> str:
    # Block-level scrubs first so we don't waste passes on text inside
    # removed sections
    for pattern, repl in HEADER_SCRUB:
        text = pattern.sub(repl, text)
    # URL rewrites — relative paths for sibling pages
    for pattern, repl in URL_REWRITES:
        text = pattern.sub(repl, text)
    # Plain substring replacements for visible text
    for needle, repl in TEXT_REPLACEMENTS:
        text = text.replace(needle, repl)
    return text


def main():
    print(f"Writing all outputs to {DST.resolve()}")
    print()
    for name, src_dir in FILES:
        src_path = Path(src_dir) / name
        if not src_path.exists():
            print(f"  SKIP {name} (not found at {src_path})")
            continue
        text = src_path.read_text(encoding="utf-8", errors="ignore")
        before_size = len(text)
        clean = anonymise(text)
        after_size = len(clean)
        dst_path = DST / name
        dst_path.write_text(clean, encoding="utf-8")
        print(f"  WROTE {name:30s}  {before_size:>10,}  ->  {after_size:>10,} bytes")

    # Audit: anything left that still names the author?
    print()
    print("Post-anonymisation audit:")
    audit_terms = ["Adam Dennett", "adamdennett", "adam-dennett",
                   "University College London", "Bartlett",
                   "a.dennett@ucl.ac.uk", "profiles.ucl.ac.uk",
                   "Professor of Urban Analytics"]
    for name, _ in FILES:
        dst_path = DST / name
        if not dst_path.exists():
            continue
        text = dst_path.read_text(encoding="utf-8", errors="ignore")
        hits = []
        for term in audit_terms:
            n = text.count(term)
            if n > 0:
                hits.append(f"{term}={n}")
        status = "  CLEAN" if not hits else "  ATTENTION: " + ", ".join(hits)
        print(f"  {name:30s}{status}")

    # Generate a small README to accompany the OSF upload
    readme = DST / "README.md"
    readme.write_text(
        "# Supplementary materials\n\n"
        "Anonymised HTML supplementary materials accompanying the manuscript\n"
        "submission to peer review.\n\n"
        "The footnotes in the manuscript reference specific files in this\n"
        "supplementary archive (e.g. *\"see `model_experiments.html`\"*).\n"
        "This README describes how to access them.\n\n"
        "## How to view\n\n"
        "OSF cannot render interactive HTML files in its built-in preview.\n"
        "To view the supplementary material:\n\n"
        "1. Download **`supplementary.zip`** from the Files list on this OSF project page.\n"
        "2. Extract the archive on your local machine.\n"
        "3. Open **`index.html`** in a modern web browser (Chrome, Firefox, Edge, Safari).\n\n"
        "The other HTML files are cross-linked from `index.html` and from each\n"
        "other. Interactive plots and filters (plotly + crosstalk) require a\n"
        "real browser environment — they will not render in OSF's built-in\n"
        "viewer, which shows raw HTML source only.\n\n"
        "## Files inside `supplementary.zip`\n\n"
        "| File | Description |\n"
        "|---|---|\n"
        "| `index.html` | Project overview and navigation |\n"
        "| `data_overview.html` | Descriptive statistics and data preparation |\n"
        "| `model_results.html` | Full multilevel model fits and diagnostics |\n"
        "| `model_experiments.html` | Sensitivity analyses, robustness checks, the two-stage absence decomposition, and the reviewer-response diagnostics |\n"
        "| `about.html` | Background notes on the earlier analysis project |\n"
        "| `absence.html` | Earlier descriptive analysis of city-level absence and the Gorard Segregation Index |\n"
        "| `absence_files/figure-html/` | Image assets for `absence.html` |\n\n"
        "## Reviewer-response diagnostics\n\n"
        "The revised manuscript references three additional analyses, all in\n"
        "`model_experiments.html` under **Reviewer-response diagnostics**:\n\n"
        "- **Prior-attainment robustness** — refits adding mean KS2 scaled score\n"
        "  on the three-year sub-sample where it is published.\n"
        "- **Ofsted specification** — random effect versus categorical fixed\n"
        "  effect, with per-band coefficients.\n"
        "- **Functional-form diagnostics** — AIC comparison of logged, linear and\n"
        "  spline treatments, plus component-plus-residual plots.\n\n"
        "## Policy Simulator tool\n\n"
        "References to the live Policy Simulator tool (an interactive R Shiny\n"
        "web application developed as part of this work) have been withheld\n"
        "from this anonymised version. Tool details, a walkthrough and access\n"
        "can be made available on request from the corresponding author via\n"
        "the editorial office.\n",
        encoding="utf-8",
    )
    print(f"\n  WROTE README.md")


if __name__ == "__main__":
    main()
