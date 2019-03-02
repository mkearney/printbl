cat_line <- function (...) {
  cat(paste0(..., "\n"), sep = "")
}

tbl_print <- function(x, ..., n = NULL, width = NULL, n_extra = NULL) {
  capture.output(
    cat_line(tibble:::format.tbl(x, ..., n = n, width = width,
      n_extra = n_extra))
  )
}
col_pat <- function(o, pat, col = "#999999", ..., bold = FALSE) {
  n <- grep(pat, o, ...)
  pgray <- function(x) sprintf("\033[38;5;246m%s\033[39m", as.character(x))
  if (bold) {
    pcol <- function(x)
      crayon::bold(sprintf(crayon::make_style(col)("%s"), as.character(x)))
  } else {
    pcol <- function(x) sprintf(crayon::make_style(col)("%s"), as.character(x))
  }
  nn <- pcol(tfse::regmatches_first(o[n], pat))
  o[n] <- unlist(dapr::ilap(nn, ~ sub(pat, nn[.i], o[n[.i]], ...)))
  o
}
col_pats <- function(o, pat, col = "#999999", ..., bold = FALSE) {
  n <- grep(pat, o, ...)
  if (bold) {
    pcol <- function(x)
      crayon::bold(sprintf(crayon::make_style(col)("%s"), as.character(x)))
  } else {
    pcol <- function(x) sprintf(crayon::make_style(col)("%s"), as.character(x))
  }
  m <- tfse::gregexpr_(o[n], pat)
  regmatches(o[n], m) <- dapr::lap(regmatches(o[n], m), pcol)
  o
}

otbl <- function(o) {
  if (length(o) == 1) cat_line(o)
  o <- gsub("<NA>", "NA  ", o)
  o <- col_pat(o, "^\\s{0,}\\d+(?= )", perl = TRUE)
  o <- col_pats(o, "(?<= )-\\d[\\d|\\.]{0,}\\b", perl = TRUE, col = "#880000")
  o <- col_pats(o, "(?<=  )0\\b", perl = TRUE, col = "#880066")
  o <- col_pats(o, "(?<=  )\\d[\\d|\\.]{0,}\\b", perl = TRUE, col = "#000066")
  o <- col_pats(o, "\\bTRUE\\b", perl = TRUE, col = "goldenrod")
  o <- col_pats(o, "\\bFALSE\\b", perl = TRUE, col = "goldenrod")
  o <- col_pats(o, "\\bNA\\b", col = "#CC7500")
  varlines <- grep("^\\# [^A]", o)
  o[varlines] <- col_pats(o[varlines], "\\S+(?= <[[:alpha:]])", perl = TRUE,
    col = "#888888")
  o <- col_pats(o, "<[^>]+>", col = "#888888")
  if (grepl("A tibble", o[1])) {
    o2 <- 2
  } else {
    o2 <- 1
  }
  o[o2] <- col_pat(o[o2], ".*", col = "#888888")
  o <- col_pat(o, "^#.*")
  cat_line(o)
}

#' @export
print.tbl_df <- function(x, ...) {
  d <- x
  o <- tbl_print(x, ...)
  ctr <- 1
  sp <- as.integer(((getOption("width", 80) - 21) / 2))
  if (sp < 1) sp <- 1
  sp <- paste(rep("-", sp), collapse = "")
  pgray <- function(x) sprintf(crayon::make_style("#888888")("%s"),
    as.character(x))
  plgray <- function(x) sprintf(crayon::make_style("#cccccc")("%s"),
    as.character(x))
  cat(pgray(o[1]), "\n")
  cat("   " %P% plgray(sp) %P% crayon::bold(
    crayon::magenta("printing row #" %P% ctr)) %P%
      plgray(sp), fill = TRUE)
  otbl(o[-c(1, grep("with \\d+ more rows", o)[1]:length(o))])
  while (any(grep("more variables?: \\S+", o))) {
    ctr <- ctr + 1
    cat("\n")
    cat("   " %P% plgray(sp) %P% crayon::bold(
      crayon::magenta("printing row #" %P% ctr)) %P%
        plgray(sp), fill = TRUE)
    vars <- o[grep("more variables?: \\S+", o):length(o)]
    vars <- tfse::regmatches_(vars, "\\S+(?= \\<[[:alpha:]])", drop = TRUE)
    x <- x[, names(x) %in% vars]
    o <- tbl_print(x, ...)
    if (any(grep("more variables?: \\S+", o))) {
      otbl(o[-c(1, grep("with \\d+ more rows", o)[1]:length(o))])
    } else {
      otbl(o[-c(1)])
    }
  }
  invisible(d)
}
