# Source: <https://github.com/mlr-org/mlr3misc/blob/main/R/format_bib.R>
# by Michel Lang (copied here May 2024)
format_bib <- function(..., bibentries = NULL, envir = parent.frame()) {
  if (is.null(bibentries)) {
    bibentries <- get("bibentries", envir = envir)
  }
  stopifnot(is.list(bibentries), anyDuplicated(names(bibentries)) == 0L)
  str <- vapply(
    list(...), function(entry) tools::toRd(bibentries[[entry]]), NA_character_
  )
  paste0(str, collapse = "\n\n")
}

is_number <- function(x) {
  is.numeric(x) && length(x) == 1L && !is.na(x)
}

is_count <- function(x) {
  is_number(x) &&  x > 0L && (is.infinite(x) || as.integer(x) == x)
}
