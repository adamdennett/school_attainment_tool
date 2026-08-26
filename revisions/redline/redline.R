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

read_paras <- function(f) {
  x <- readLines(f, warn = FALSE)
  x[trimws(x) != ""]
}
a <- read_paras(file.path(SP, "v1.md"))
b <- read_paras(file.path(SP, "v2.md"))

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

# lines we do not word-diff (tables, images, fenced divs, maths)
is_literal <- function(s) grepl("^[[:space:]]*([|]|:::|![[]|[{]|---|[+]-|[$])", s) ||
                          grepl("[$][$]|varepsilon", s)
split_prefix <- function(s) {
  m <- regmatches(s, regexpr("^[[:space:]]*((#+|[-*+]|[0-9]+[.)]|>)[[:space:]]+)?", s))
  list(prefix = m, rest = substring(s, nchar(m) + 1))
}

# ---- paragraph-level alignment ---------------------------------------
pops <- lcs_idx(norm(a), norm(b))
res <- character(0)
n_mod <- 0; n_ins <- 0; n_del <- 0; n_quiet <- 0

emit_changed_block <- function(di, si) {
  k <- min(length(di), length(si))
  for (n in seq_len(k)) {
    d <- a[di[n]]; s <- b[si[n]]
    # Never reveal a removed identifying URL: emit the new text untracked.
    if (is_disclosive(d)) { res <<- c(res, s); n_quiet <<- n_quiet + 1; next }
    if (is_literal(d) || is_literal(s)) { res <<- c(res, s); n_mod <<- n_mod + 1; next }
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
    if (is_literal(s)) { res <<- c(res, s); next }
    p <- split_prefix(s)
    res <<- c(res, paste0(p$prefix, span(p$rest, "insertion"))); n_ins <<- n_ins + 1
  }
}

di <- integer(0); si <- integer(0)
for (o in pops) {
  if (o[1] == 0L) {
    if (length(di) || length(si)) { emit_changed_block(di, si); di <- integer(0); si <- integer(0) }
    res <- c(res, b[o[3]])          # unchanged: emit the NEW text
  } else if (o[1] == -1L) di <- c(di, o[2])
  else si <- c(si, o[3])
}
if (length(di) || length(si)) emit_changed_block(di, si)

writeLines(paste(res, collapse = "\n\n"), file.path(SP, "redline.md"))
cat(sprintf("paragraphs: v1=%d v2=%d | modified=%d inserted=%d deleted=%d | untracked(disclosive)=%d\n",
            length(a), length(b), n_mod, n_ins, n_del, n_quiet))
