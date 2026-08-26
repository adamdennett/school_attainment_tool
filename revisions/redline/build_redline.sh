#!/usr/bin/env bash
# Build RPE_Paper_tracked_changes.docx: RPE_Paper_V1 (as submitted) vs current RPE_Paper.
# Run from the project root:  bash revisions/redline/build_redline.sh [new_docx]
set -euo pipefail

PD="/c/Program Files/Quarto/bin/tools/pandoc.exe"
RS="/c/Program Files/R/R-4.5.2/bin/Rscript.exe"
DIR="revisions/redline"
WORK="${TMPDIR:-/tmp}/redline_work"
NEW_DOCX="${1:-RPE_Paper.docx}"      # allow a temp name when Word holds the real one
OLD_DOCX="RPE_Paper_V1.docx"

mkdir -p "$WORK"
rm -rf "$WORK/media"

[ -f "$OLD_DOCX" ] || { echo "Missing $OLD_DOCX - render $DIR/RPE_Paper_V1_render.qmd first"; exit 1; }
[ -f "$NEW_DOCX" ] || { echo "Missing $NEW_DOCX"; exit 1; }

echo "==> converting to markdown"
"$PD" "$OLD_DOCX" -t markdown-smart --wrap=none -o "$WORK/v1.md"
"$PD" "$NEW_DOCX" -t markdown-smart --wrap=none --extract-media="$WORK" -o "$WORK/v2.md"

n1=$(grep -o 'media/[a-zA-Z0-9]*\.png' "$WORK/v1.md" | wc -l)
n2=$(grep -o 'media/[a-zA-Z0-9]*\.png' "$WORK/v2.md" | wc -l)
echo "    figures: v1=$n1 v2=$n2"
[ "$n1" = "$n2" ] || echo "    WARNING: figure counts differ - image paths may misalign"

# Normalise image paths to fig1..figN in order of appearance so identical
# figures do not register as changes; take the actual files from the new docx.
i=0
for f in $(grep -o 'media/[a-zA-Z0-9]*\.png' "$WORK/v2.md"); do
  i=$((i+1)); cp "$WORK/$f" "$WORK/media/fig$i.png"
done
for v in v1 v2; do
  awk '{ while (match($0, /media\/rId[0-9]+\.png/)) { n++; $0 = substr($0,1,RSTART-1) "media/fig" n ".png" substr($0,RSTART+RLENGTH) } print }' \
    "$WORK/$v.md" > "$WORK/$v.tmp" && mv "$WORK/$v.tmp" "$WORK/$v.md"
done

echo "==> diffing"
"$RS" "$DIR/redline.R" "$WORK"

echo "==> rendering tracked-changes docx"
"$PD" "$WORK/redline.md" -f markdown -t docx \
  --resource-path="$WORK" --reference-doc=RPE_reference.docx \
  -o RPE_Paper_tracked_changes.docx

ins=$(unzip -p RPE_Paper_tracked_changes.docx word/document.xml | grep -o '<w:ins ' | wc -l)
del=$(unzip -p RPE_Paper_tracked_changes.docx word/document.xml | grep -o '<w:del ' | wc -l)
echo "==> done: $ins insertions, $del deletions"
