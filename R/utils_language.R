deps_function <- function(fun) {
  env <- list(`~` = `identity`)
  body(fun) <- eval(call("substitute", body(fun), env), envir = baseenv())
  codetools::findGlobals(fun)
}

deparse_language <- function(x) {
  trn(!is.character(x) && !is.null(x), deparse_safe(x), x)
}

deparse_safe <- function(x, collapse = "\n", backtick = TRUE) {
  out <- deparse_direct(
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

deparse_direct <- function(...) {
  produce_deparse_direct()(...)
}

embody_expr <- function(expr) {
  fun <- function() {
  }
  body(fun) <- expr
  fun
}

produce_deparse_direct <- function() {
  .deparseOpts <- identity
  environment(deparse) <- environment()
  deparse
}

tidy_eval <- function(expr, envir, tidy_eval) {
  if (tidy_eval) {
    expr <- as.call(c(quote(rlang::expr), expr))
    expr <- rlang::quo_squash(eval(expr, envir = envir))
  }
  expr
}
