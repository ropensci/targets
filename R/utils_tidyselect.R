#' @aliases tar_tidyselect
#' @export
tidyselect::all_of
#' @export
tidyselect::any_of
#' @export
tidyselect::contains
#' @export
tidyselect::ends_with
#' @export
tidyselect::everything
#' @export
tidyselect::last_col
#' @export
tidyselect::matches
#' @export
tidyselect::num_range
#' @export
tidyselect::one_of
#' @export
tidyselect::starts_with

tar_tidyselect <- function(names_quosure, choices) {
  if (is.null(rlang::quo_squash(names_quosure))) {
    return(NULL)
  }
  if (!length(choices)) {
    return(NULL)
  }
  names(choices) <- choices
  out <- tidyselect::eval_select(names_quosure, data = choices, strict = FALSE)
  out <- names(out)
  assert_chr(
    out %||% character(0),
    paste(
      "the names arg of tar_make() and friends supports tidyselect syntax",
      "but must resolve to a character vector in the end."
    )
  )
  out
}
