# 16_mobile_safe_html.R - Make a self-contained Quarto HTML survive a phone
# ---------------------------------------------------------------------------
# With `embed-resources: true`, Quarto inlines the Bootstrap stylesheet as a
# percent-encoded data URL on a <link> tag:
#
#   <link href="data:text/css,%3Aroot%2C%5Bdata%2Dbs..." rel="stylesheet">
#
# For these reports that single attribute is over a million characters. Two
# things follow, and both of them bite on mobile rather than on a desktop:
#
#   1. Percent-encoding inflates the CSS by about 30%, because every colon,
#      comma, brace and hyphen becomes three characters.
#   2. The browser has to hold the whole URL as a string, decode it, and then
#      parse the result - all before it can style anything. Mobile Chrome
#      gives a renderer far less memory than the desktop build, and when it
#      runs out the tab goes blank rather than erroring.
#
# Rewriting the link as a plain <style> block removes both problems: the CSS
# is smaller and it is parsed directly, with no URL decode step. The page
# stays fully self-contained.
#
# Usage:
#   source("R/16_mobile_safe_html.R")
#   mobile_safe("exclusions_and_absence.html")      # rewrite in place
#   mobile_safe_report("exclusions_and_absence.html")  # just measure
#
# Run from project root. Standalone execution rewrites every report named in
# REPORTS that is present and still carries a data:text/css link.
# ---------------------------------------------------------------------------

REPORTS <- c("exclusions_and_absence.html",
             "ks4_2025_26_update.html")


#' Percent-decode without going character by character
#'
#' utils::URLdecode builds the result one character at a time, which on an
#' 800 KB string takes minutes. This works on the raw bytes: each "%" is
#' overwritten with the byte its two hex digits name and the digits are
#' dropped. Multi-byte UTF-8 survives because every byte is decoded in
#' place and the sequence is never re-interpreted.
fast_urldecode <- function(x) {
  b <- charToRaw(x)
  pct <- which(b == charToRaw("%"))
  if (!length(pct)) return(x)
  hex <- paste0(rawToChar(b[pct + 1L], multiple = TRUE),
                rawToChar(b[pct + 2L], multiple = TRUE))
  b[pct] <- as.raw(strtoi(hex, 16L))
  rawToChar(b[-c(pct + 1L, pct + 2L)])
}


#' Turn every <link href="data:text/css,..."> into an inline <style>
#'
#' Attributes other than href are carried across onto the <style> tag, so
#' Quarto's own `id` and `data-mode` hooks keep working - the dark/light
#' toggle reads them.
inline_data_css <- function(html) {
  pat <- '<link href="data:text/css,([^"]*)"([^>]*)>'
  m <- gregexpr(pat, html, perl = TRUE)
  hits <- regmatches(html, m)[[1]]
  if (!length(hits)) return(list(html = html, n = 0L, saved = 0))

  saved <- 0
  for (h in hits) {
    enc  <- sub(pat, "\\1", h, perl = TRUE)
    attr <- sub(pat, "\\2", h, perl = TRUE)
    attr <- trimws(sub('\\s*rel="stylesheet"', "", attr))
    css  <- fast_urldecode(enc)
    rep  <- paste0("<style", if (nzchar(attr)) paste0(" ", attr) else "",
                   ">", css, "</style>")
    saved <- saved + (nchar(h) - nchar(rep))
    html <- sub(h, rep, html, fixed = TRUE)
  }
  list(html = html, n = length(hits), saved = saved)
}


#' What a file is actually made of
mobile_safe_report <- function(path) {
  html <- readChar(path, file.size(path), useBytes = TRUE)
  count <- function(p) {
    m <- gregexpr(p, html, perl = TRUE)
    h <- regmatches(html, m)[[1]]
    c(n = length(h), chars = sum(nchar(h)))
  }
  css <- count('data:text/css,[^"]*')
  png <- count('data:image/png;base64,[A-Za-z0-9+/=]*')
  fnt <- count('data:font/[a-z0-9]+;base64,[A-Za-z0-9+/=]*')
  js  <- count('data:application/javascript;base64,[A-Za-z0-9+/=]*')
  message(sprintf(
    "%s\n  total %s bytes\n  css   %2d, %s chars\n  png   %2d, %s chars\n  font  %2d, %s chars\n  js    %2d, %s chars",
    basename(path), format(file.size(path), big.mark = ","),
    css[["n"]], format(css[["chars"]], big.mark = ","),
    png[["n"]], format(png[["chars"]], big.mark = ","),
    fnt[["n"]], format(fnt[["chars"]], big.mark = ","),
    js[["n"]],  format(js[["chars"]],  big.mark = ",")))
  invisible(list(css = css, png = png, font = fnt, js = js,
                 size = file.size(path)))
}


#' Rewrite one rendered report in place
#'
#' Idempotent: a file with no data:text/css link left in it is reported and
#' left alone, so this can be run after every render without checking first.
mobile_safe <- function(path, quiet = FALSE) {
  if (!file.exists(path)) {
    message("Not found, skipping: ", path)
    return(invisible(FALSE))
  }
  before <- file.size(path)
  html <- readChar(path, before, useBytes = TRUE)
  out <- inline_data_css(html)
  if (out$n == 0L) {
    if (!quiet) message(basename(path), ": no data:text/css links, unchanged")
    return(invisible(FALSE))
  }
  con <- file(path, "wb")
  on.exit(close(con))
  writeBin(charToRaw(out$html), con)
  close(con); on.exit()
  after <- file.size(path)
  if (!quiet) {
    message(sprintf("%s: inlined %d stylesheet%s, %s -> %s bytes (-%.1f%%)",
                    basename(path), out$n, if (out$n == 1) "" else "s",
                    format(before, big.mark = ","),
                    format(after, big.mark = ","),
                    100 * (before - after) / before))
  }
  invisible(TRUE)
}


mobile_safe_all <- function(reports = REPORTS) {
  for (r in reports) mobile_safe(here::here(r))
  invisible(TRUE)
}


if (sys.nframe() == 0) mobile_safe_all()
