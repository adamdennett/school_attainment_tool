#!/usr/bin/env bash
# render_and_deploy.sh
# ---------------------------------------------------------------------------
# Renders all Quarto (.qmd) reports in output/ and copies the resulting
# HTML files to docs/ for GitHub Pages.
#
# Usage:
#   bash render_and_deploy.sh            # render all .qmd files
#   bash render_and_deploy.sh --quick    # skip rendering, just copy HTML
# ---------------------------------------------------------------------------

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
OUTPUT_DIR="$SCRIPT_DIR/output"
DOCS_DIR="$SCRIPT_DIR/docs"

# Ensure docs/ exists (with .nojekyll for GitHub Pages)
mkdir -p "$DOCS_DIR"
[[ -f "$DOCS_DIR/.nojekyll" ]] || touch "$DOCS_DIR/.nojekyll"

# ------------------------------------------------------------------
# Step 1: Render .qmd files (unless --quick)
# ------------------------------------------------------------------
if [[ "${1:-}" != "--quick" ]]; then
  echo "=== Rendering Quarto reports ==="
  QMD_FILES=("$OUTPUT_DIR"/*.qmd)

  if [[ ${#QMD_FILES[@]} -eq 0 ]]; then
    echo "No .qmd files found in $OUTPUT_DIR"
    exit 1
  fi

  for qmd in "${QMD_FILES[@]}"; do
    echo "  Rendering $(basename "$qmd") ..."
    quarto render "$qmd" --to html
  done
  echo ""
else
  echo "=== Skipping render (--quick mode) ==="
  echo ""
fi

# ------------------------------------------------------------------
# Step 2: Copy HTML files from output/ to docs/
# ------------------------------------------------------------------
echo "=== Syncing HTML to docs/ ==="

copied=0
for html in "$OUTPUT_DIR"/*.html; do
  fname="$(basename "$html")"
  # Skip the standalone map (not a report page)
  if [[ "$fname" == "la_cluster_map.html" ]]; then
    echo "  Skipping $fname (standalone map)"
    continue
  fi
  cp "$html" "$DOCS_DIR/$fname"
  echo "  Copied $fname"
  copied=$((copied + 1))
done

echo ""
echo "=== Done: $copied file(s) copied to docs/ ==="
echo ""
echo "Next steps:"
echo "  git add docs/"
echo "  git commit -m 'Update GitHub Pages reports'"
echo "  git push"
