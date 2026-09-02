# Build a real Word tracked-changes document from two markdown renderings.
#
# Two classes of difference are deliberately NOT tracked:
#   * dash-style changes (em/en dash vs hyphen) -- cosmetic noise;
#   * anything on the old side carrying a DISCLOSIVE url. This is a
#     double-blind submission, and a tracked deletion would show the reviewer
#     the very URL that was removed to anonymise the paper.
# In both cases the new text is emitted plainly, with no revision marks.

SP   <- commandArgs(trailingOnly = TRUE)[1]
AUTH <- "Revision"
DATE <- "2026-08-25T00:00:00Z"
DISCLOSIVE <- "adamdennett.github.io"

# Marker holding a multi-line block together as a single diff element.
NL <- "\001"

# Read one converted document into diff elements.
#
# Two things need care:
#   * Footnotes. Both versions number their notes from [^1], so interleaving
#     them would give duplicate definitions and misresolved references. Each
#     document's labels are prefixed with a per-source tag to keep them apart.
#   * Tables. Pandoc emits pipe tables one row per line, and the output below
#     is reassembled with a blank line between elements -- which would split
#     every row into its own paragraph and destroy the table. Contiguous table
#     rows are collapsed into a single element here and expanded again at the
#     end, so a table travels through the diff as one indivisible unit.
read_blocks <- function(f, tag) {
  x <- readLines(f, warn = FALSE)
  x <- gsub("\\[\\^([A-Za-z0-9_.-]+)\\]", paste0("[^", tag, "\\1]"), x)

  out <- character(0)
  i <- 1L
  n <- length(x)
  # A table line is either a row (starts "|") or a grid border ("+---", "+:--").
  is_row <- function(k) k >= 1L && k <= n &&
    grepl("^[[:space:]]*([|]|[+][-:=])", x[k])
  while (i <= n) {
    if (is_row(i)) {
      # Absorb contiguous table lines only. A blank line separates one table
      # from the next, so tolerating blanks here would merge every table in
      # the floats section into a single block.
      j <- i
      while (is_row(j + 1L)) j <- j + 1L
      out <- c(out, paste(x[i:j], collapse = NL))
      i <- j + 1L
    } else {
      if (trimws(x[i]) != "") out <- c(out, x[i])
      i <- i + 1L
    }
  }
  out
}
a <- read_blocks(file.path(SP, "v1.md"), "a")
b <- read_blocks(file.path(SP, "v2.md"), "b")

# ---- normalisation used for MATCHING only -----------------------------
# Text is always emitted in its raw form; only the comparison key is
# normalised, so dash-only differences compare equal and the new document's
# dash style survives into the output.
norm <- function(s) {
  s <- gsub("—|–|---|--", "-", s)   # em dash, en dash, -- , ---
  s <- gsub("[[:space:]]+", " ", s)
  trimws(s)
}

is_disclosive <- function(s) any(grepl(DISCLOSIVE, s, fixed = TRUE))

# ---- LCS returning index ops: c(op, i, j); op 0 = same, -1 = del, 1 = ins
lcs_idx <- function(kx, ky) {
  n <- length(kx); m <- length(ky)
  ops <- list()
  if (n == 0 && m == 0) return(ops)
  L <- matrix(0L, n + 1, m + 1)
  if (n > 0 && m > 0) {
    for (i in n:1) for (j in m:1)
      L[i, j] <- if (identical(kx[i], ky[j])) L[i+1, j+1] + 1L
                 else max(L[i+1, j], L[i, j+1])
  }
  i <- 1; j <- 1
  while (i <= n && j <= m) {
    if (identical(kx[i], ky[j])) { ops[[length(ops)+1]] <- c(0, i, j); i <- i+1; j <- j+1 }
    else if (L[i+1, j] >= L[i, j+1]) { ops[[length(ops)+1]] <- c(-1, i, 0); i <- i+1 }
    else { ops[[length(ops)+1]] <- c(1, 0, j); j <- j+1 }
  }
  while (i <= n) { ops[[length(ops)+1]] <- c(-1, i, 0); i <- i+1 }
  while (j <= m) { ops[[length(ops)+1]] <- c(1, 0, j); j <- j+1 }
  ops
}

span <- function(txt, cls)
  sprintf('[%s]{.%s author="%s" date="%s"}', txt, cls, AUTH, DATE)

# Tables survive the docx -> markdown -> docx round trip only as ASCII art
# inside a line block, which renders as an unreadable wall of pipes rather
# than a table. Since table content is not tracked either way, we substitute a
# short note naming the table. The body still carries the float placeholder
# showing where each table belongs, and the manuscript itself has the real one.
table_note <- function(s) {
  rows <- strsplit(s, NL, fixed = TRUE)[[1]]
  # The caption sits in the first cell, after the grid border, so scan for it
  # rather than assuming it is the first line of the block.
  cand <- gsub(" ", " ", rows)   # captions use a non-breaking space
  cand <- trimws(gsub("^[[:space:]]*[|][[:space:]]*", "", cand))
  cand <- trimws(gsub("[[:space:]]+", " ", cand))
  hit  <- which(grepl("^(Table|Figure)[[:space:]]*[0-9]", cand))
  cap  <- if (length(hit)) cand[hit[1]] else "Table"
  cap  <- trimws(gsub("[[:space:]]*[|][[:space:]]*$", "", cap))  # trailing cell edge
  kind <- if (grepl("^Figure", cap)) "figure" else "table"
  if (nchar(cap) > 110) cap <- paste0(substr(cap, 1, 107), "...")
  paste0("*[", cap, " — ", kind, " content is not tracked; see the manuscript ",
         "for the current version.]*")
}

# ---- word-level diff of one paragraph --------------------------------
word_diff <- function(s1, s2) {
  t1 <- strsplit(s1, " ", fixed = TRUE)[[1]]
  t2 <- strsplit(s2, " ", fixed = TRUE)[[1]]
  ops <- lcs_idx(norm(t1), norm(t2))
  out <- character(0); buf <- character(0); mode <- 0L
  flush <- function() {
    if (!length(buf)) return(invisible())
    txt <- paste(buf, collapse = " ")
    out <<- c(out, if (mode == 0L) txt
                   else if (mode == 1L) span(txt, "insertion")
                   else span(txt, "deletion"))
    buf <<- character(0)
  }
  for (o in ops) {
    if (o[1] != mode) { flush(); mode <- o[1] }
    # for unchanged tokens emit the NEW text, so the new dash style wins
    buf <- c(buf, if (o[1] == -1L) t1[o[2]] else t2[o[3]])
  }
  flush()
  paste(out, collapse = " ")
}

# Blocks we never word-diff: tables, images, fenced divs, display maths, and
# footnote definitions. A tracked redline through a results table is
# unreadable; revision spans break TeX; and a definition wrapped in a span --
# [[^b1]: text]{.insertion} -- is no longer a footnote definition at all, so
# pandoc drops it and every reference to it renders as stray text. These all
# pass through as whole units showing the new version.
is_literal <- function(s) grepl("^[[:space:]]*([|]|:::|![[]|[{]|---|[+]-|[$]|<table)", s) ||
                          grepl("^[[:space:]]*\\[\\^[A-Za-z0-9_.-]+\\]:", s) ||
                          grepl("[$][$]|varepsilon", s) ||
                          grepl(NL, s, fixed = TRUE)
split_prefix <- function(s) {
  m <- regmatches(s, regexpr("^[[:space:]]*((#+|[-*+]|[0-9]+[.)]|>)[[:space:]]+)?", s))
  list(prefix = m, rest = substring(s, nchar(m) + 1))
}

# ---- paragraph-level alignment ---------------------------------------
pops <- lcs_idx(norm(a), norm(b))
res <- character(0)
n_mod <- 0; n_ins <- 0; n_del <- 0; n_quiet <- 0; n_tbl <- 0

emit_changed_block <- function(di, si) {
  k <- min(length(di), length(si))
  for (n in seq_len(k)) {
    d <- a[di[n]]; s <- b[si[n]]
    # Never reveal a removed identifying URL: emit the new text untracked.
    if (is_disclosive(d)) { res <<- c(res, s); n_quiet <<- n_quiet + 1; next }
    if (is_literal(d) || is_literal(s)) {
      if (grepl(NL, s, fixed = TRUE)) {
        res <<- c(res, table_note(s)); n_tbl <<- n_tbl + 1
      } else {
        res <<- c(res, s); n_mod <<- n_mod + 1
      }
      next
    }
    pd <- split_prefix(d); ps <- split_prefix(s)
    res <<- c(res, paste0(ps$prefix, word_diff(pd$rest, ps$rest)))
    n_mod <<- n_mod + 1
  }
  if (length(di) > k) for (n in (k+1):length(di)) {
    d <- a[di[n]]
    if (is_literal(d)) next
    if (is_disclosive(d)) { n_quiet <<- n_quiet + 1; next }   # drop silently
    p <- split_prefix(d)
    res <<- c(res, paste0(p$prefix, span(p$rest, "deletion"))); n_del <<- n_del + 1
  }
  if (length(si) > k) for (n in (k+1):length(si)) {
    s <- b[si[n]]
    if (is_literal(s)) {
      if (grepl(NL, s, fixed = TRUE)) { res <<- c(res, table_note(s)); n_tbl <<- n_tbl + 1 }
      else res <<- c(res, s)
      next
    }
    p <- split_prefix(s)
    res <<- c(res, paste0(p$prefix, span(p$rest, "insertion"))); n_ins <<- n_ins + 1
  }
}

di <- integer(0); si <- integer(0)
for (o in pops) {
  if (o[1] == 0L) {
    if (length(di) || length(si)) { emit_changed_block(di, si); di <- integer(0); si <- integer(0) }
    blk <- b[o[3]]                  # unchanged: emit the NEW text
    if (grepl(NL, blk, fixed = TRUE)) { res <- c(res, table_note(blk)); n_tbl <- n_tbl + 1 }
    else res <- c(res, blk)
  } else if (o[1] == -1L) di <- c(di, o[2])
  else si <- c(si, o[3])
}
if (length(di) || length(si)) emit_changed_block(di, si)

# ---- reassemble -------------------------------------------------------
txt <- paste(res, collapse = "\n\n")
txt <- gsub(NL, "\n", txt, fixed = TRUE)      # restore table rows

# Drop orphaned footnotes. Suppressing a disclosive note removes its
# definition, and a reference left without one renders as stray text in Word;
# a definition nobody references makes pandoc warn and emit nothing.
def_pat <- "^\\[\\^([A-Za-z0-9_.-]+)\\]:"
ref_pat <- "\\[\\^([A-Za-z0-9_.-]+)\\]"

lines   <- strsplit(txt, "\n", fixed = TRUE)[[1]]
is_def  <- grepl(def_pat, lines)
defined <- sub("\\].*$", "", sub("^\\[\\^", "", lines[is_def]))

refs <- unlist(regmatches(lines[!is_def], gregexpr(ref_pat, lines[!is_def])))
referenced <- unique(gsub("^\\[\\^|\\]$", "", refs))

orphan_refs <- setdiff(referenced, defined)
unused_defs <- setdiff(defined, referenced)

if (length(unused_defs)) {
  drop <- is_def & (sub("\\].*$", "", sub("^\\[\\^", "", lines)) %in% unused_defs)
  lines <- lines[!drop]
}

# Restore the bibliography style. Reference entries carry the "Bibliography"
# paragraph style in the manuscript, which is where their hanging indent comes
# from, but that marker is lost converting docx -> markdown and they would
# otherwise be written back as ordinary body text. Wrapping the block in a div
# with custom-style makes pandoc's docx writer apply the style again, picking
# up the indent defined in RPE_reference.docx.
hdr <- grep("^#+[[:space:]]+References", lines)
if (length(hdr)) {
  h    <- hdr[1]
  nxt  <- grep("^#+[[:space:]]", lines)
  nxt  <- nxt[nxt > h]
  end  <- if (length(nxt)) nxt[1] - 1L else length(lines)
  body <- lines[(h + 1L):end]
  if (any(trimws(body) != "")) {
    lines <- c(lines[seq_len(h)],
               "",
               "::: {custom-style=\"Bibliography\"}",
               body,
               ":::",
               "",
               if (end < length(lines)) lines[(end + 1L):length(lines)] else character(0))
    n_refs <- sum(trimws(body) != "")
    cat(sprintf("bibliography: %d reference entries wrapped in the Bibliography style\n", n_refs))
  }
}

txt <- paste(lines, collapse = "\n")
for (lab in orphan_refs) txt <- gsub(paste0("[^", lab, "]"), "", txt, fixed = TRUE)

writeLines(txt, file.path(SP, "redline.md"))

cat(sprintf("blocks: v1=%d v2=%d | modified=%d inserted=%d deleted=%d | tables passed through=%d | untracked(disclosive)=%d\n",
            length(a), length(b), n_mod, n_ins, n_del, n_tbl, n_quiet))
cat(sprintf("footnotes: %d defined, %d referenced | %d orphan refs stripped, %d unused defs dropped\n",
            length(defined), length(referenced), length(orphan_refs), length(unused_defs)))
