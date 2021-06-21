deparse_script_code <- function(code) {
  if_any(
    length(code) > 1L && identical(deparse_safe(code[[1]]), "`{`"),
    map_chr(code[-1], deparse_safe),
    deparse_safe(code)
  )
}

deps_function <- function(fun) {
  env <- list(`~` = `identity`)
  body(fun) <- eval(call("substitute", body(fun), env), envir = baseenv())
  codetools::findGlobals(fun)
}

deparse_language <- function(x) {
  if_any(!is.character(x) && !is.null(x), deparse_safe(x), x)
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

tar_tidy_eval <- function(expr, envir, tidy_eval) {
  if (tidy_eval) {
    expr <- as.call(c(quote(rlang::expr), expr))
    expr <- rlang::quo_squash(eval(expr, envir = envir))
  }
  expr
}

tar_tidyselect_eval <- function(names_quosure, choices) {
  if (is.null(rlang::quo_squash(names_quosure))) {
    return(NULL)
  }
  if (!length(choices)) {
    return(NULL)
  }
  names(choices) <- choices
  out <- tidyselect::eval_select(names_quosure, data = choices, strict = FALSE)
  out <- names(out)
  tar_assert_chr(
    out %|||% character(0),
    paste(
      "the names arg of tar_make() and friends supports tidyselect syntax",
      "but must resolve to a character vector in the end."
    )
  )
  out
}