#' @title Language
#' @name tar_language
#' @family utilities to extend targets
#' @description These functions help with metaprogramming in
#'   packages built on top of `targets`.
#' @details
#'   * `tar_deparse_language()` is a wrapper around `tar_deparse_safe()`
#'     which leaves character vectors and `NULL` objects alone,
#'     which helps with subsequent user input validation.
#'   * `tar_deparse_safe()` is a wrapper around `base::deparse()`
#'     with a custom set of fast default settings and guardrails
#'     to ensure the output always has length 1.
#'   * `tar_tidy_eval()` applies tidy evaluation to a language object
#'     and returns another language object.
#'   * `tar_tidyselect_eval()` applies `tidyselect` selection with
#'     some special guardrails around `NULL` inputs.
#' @inheritParams base::deparse
#' @param expr A language object to modify or deparse.
#' @param collapse Character of length 1, delimiter in deparsing.
#' @param envir An environment to find objects for tidy evaluation.
#' @param tidy_eval Logical of length 1, whether to apply tidy evaluation.
#' @param names_quosure An `rlang` quosure with `tidyselect` expressions.
#' @param choices A character vector of choices for character elements
#'   returned by tidy evaluation.
#' @examples
#' tar_deparse_language(quote(run_model()))
NULL

deparse_script_code <- function(code) {
  if_any(
    length(code) > 1L && identical(tar_deparse_safe(code[[1]]), "`{`"),
    map_chr(code[-1], tar_deparse_safe),
    tar_deparse_safe(code)
  )
}

deps_function <- function(fun) {
  env <- list(`~` = `identity`)
  body(fun) <- eval(call("substitute", body(fun), env), envir = baseenv())
  sort_chr(codetools::findGlobals(fun))
}

#' @export
#' @rdname tar_language
tar_deparse_language <- function(expr) {
  if_any(!is.character(expr) && !is.null(expr), tar_deparse_safe(expr), expr)
}

#' @export
#' @rdname tar_language
tar_deparse_safe <- function(expr, collapse = "\n", backtick = TRUE) {
  out <- deparse_direct(
    expr,
    control = deparse_control_custom,
    backtick = backtick
  )
  if (length(out) > 1L) {
    out <- paste(out, collapse = collapse)
  }
  out
}

deparse_control_custom <- .deparseOpts(
  c("keepInteger", "showAttributes", "keepNA", "niceNames")
)

deparse_direct <- function(...) {
  produce_deparse_direct()(...)
}

embody_expr <- function(expr) {
  fun <- function() {}
  body(fun) <- expr
  fun
}

produce_deparse_direct <- function() {
  .deparseOpts <- identity
  environment(deparse) <- environment()
  deparse
}

#' @export
#' @rdname tar_language
tar_tidy_eval <- function(expr, envir, tidy_eval) {
  if (tidy_eval) {
    expr <- as.call(c(quote(rlang::expr), expr))
    expr <- rlang::quo_squash(eval(expr, envir = envir))
  }
  expr
}

#' @export
#' @rdname tar_language
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

tar_sub_body <- function(fun, values) {
  body(fun) <- eval(
    substitute(
      substitute(expr = expr, env = env),
      env = list(expr = body(fun), env = values)
    )
  )
  fun
}
