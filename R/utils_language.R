embody_expr <- function(expr) {
  fun <- function() {
  }
  body(fun) <- expr
  fun
}

deps_function <- function(x) {
  codetools::findGlobals(x)
}

deparse_language <- function(x) {
  trn(!is.character(x) && !is.null(x), safe_deparse(x), x)
}

safe_deparse <- function(x, collapse = "\n", backtick = TRUE) {
  out <- direct_deparse(
    x,
    control = deparse_control_custom,
    backtick = backtick
  )
  if (length(out) > 1L) {
    out <- paste(out, collapse = collapse)
  }
  out
}

deparse_control_custom <- .deparseOpts(c("keepNA", "keepInteger"))

direct_deparse <- function(...) {
  produce_direct_deparse()(...)
}

produce_direct_deparse <- function() {
  .deparseOpts <- identity
  environment(deparse) <- environment()
  deparse
}

safe_parse <- function(text) {
  out <- parse(text = text, keep.source = FALSE)
  trn(length(out), out[[1]], out)
}
