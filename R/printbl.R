cat_line <- function (...) {
  cat(paste0(..., "\n"), sep = "")
}

tbl_print <- function(x, ..., n = NULL, width = NULL, n_extra = NULL) {
  x[dapr::vap_lgl(x, is.character)] <- dapr::lap(
    x[dapr::vap_lgl(x, is.character)], iconv, to = "ascii", sub = ""
  )
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
  if (grepl("A tibble", o[1])) {
    o2 <- 2
  } else {
    o2 <- 1
  }
  gr <- grep("(^\\s+<\\S+)|(^#)", o)
  o[gr] <- col_pat(o[gr], ".*", col = "#888888")
  dg <- grep("^\\s+[[:alpha:]]", o)
  o[dg] <- crayon::make_style("#444444")(o[dg])
  cat_line(o)
}

mmap <- function(.f, ...) {
  dots <- eval(substitute(alist(list(...))))
  stopifnot(
    is.function(.f),
    length(dots) > 0
  )
  dots <- gsub("\\bc\\(", "list(", dots)
  dots <- eval(parse(text = dots))
  lens <- lengths(dots)
  if (any(lens == 0)) {
    dots[lens == 0] <-list(rep(list(NULL), max(lens)))
  }
  .Internal(mapply(.f, dots, NULL))
}
catp <- function(x) cat(paste(x, collapse = "\n"))
clean_dots <- function(x) {
  x <- iconv(gsub("\u2026", "{dots}", enc2utf8(x)), to = "ascii", sub = "")
  enc2utf8(gsub("\\{dots\\}", "\u2026", x))
}
colorize <- function(o) {
  lo <- grep("^\\s?\\d+ ", o)
  o[lo] <- gsub("<NA>", "NA  ", o[lo])
  ml <- max(nchar(o[lo]))
  end <- unique(c(as.integer(gregexpr(" ", o[2])[[1]])[
    diff(as.integer(gregexpr(" ", o[2])[[1]])) > 1][-1], ml))
  start <- c(4, end[-length(end)] + 1)
  m <- tfse::gregexpr_(o[3], "<[^>]+>")
  cls <- gsub("^<|>$", "", regmatches(o[3], m)[[1]])
  e <- vector("list", length(start))
  for (i in seq_along(start)) {
    if (cls[i] == "chr") {
      pc <- function(x) sprintf(crayon::make_style("#884488")("%s"),
        as.character(x))
    } else if (cls[i] == "dbl") {
      pc <- function(x) sprintf(crayon::make_style("#dd4444")("%s"),
        as.character(x))
    } else if (cls[i] == "int") {
      pc <- function(x) sprintf(crayon::make_style("#224488")("%s"),
        as.character(x))
    } else if (cls[i] == "lgl") {
      pc <- function(x) sprintf(crayon::make_style("#cc8800")("%s"),
        as.character(x))
    } else if (cls[i] == "list") {
      pc <- function(x) sprintf(crayon::make_style("#785027")("%s"),
        as.character(x))
    } else if (cls[i] == "dttm") {
      pc <- function(x) sprintf(crayon::make_style("#448844")("%s"),
        as.character(x))
    } else if (cls[i] == "fct") {
      pc <- function(x) sprintf(crayon::make_style("#000099")("%s"),
        as.character(x))
    } else {
      pc <- function(x) sprintf(crayon::make_style("#999999")("%s"),
        as.character(x))
    }
    e[[i]] <- pc(substr(o[lo], start[i], end[i]))
  }
  e <- dapr::vap_chr(seq_along(lo), ~ paste0(lapply(e, function(.y) .y[[.x]]),
    collapse = ""))
  pc <- function(x) sprintf(crayon::make_style("#999999")("%s"),
    as.character(x))
  for (i in seq_along(e)) {
    o[lo][i] <- paste0(
      pc(substr(o[lo][i], 1, start[1] - 1)), e[[i]]
    )
  }
  o
}

#' @export
print_tbl_df <- function(x, ...) {
  op <- getOption("encoding")
  on.exit(options(encoding = op))
  options(encoding = "UTF-8")
  d <- x
  o <- tbl_print(x, ...)
  o <- colorize(o)
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
    o <- colorize(o)
    if (any(grep("more variables?: \\S+", o))) {
      otbl(o[-c(1, grep("with \\d+ more rows", o)[1]:length(o))])
    } else {
      otbl(o[-c(1)])
    }
  }
  invisible(d)
}
