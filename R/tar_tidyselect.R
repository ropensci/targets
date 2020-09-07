#' @export
#' @keywords internal
tidyselect::all_of
#' @export
#' @keywords internal
tidyselect::any_of
#' @export
#' @keywords internal
tidyselect::contains
#' @export
#' @keywords internal
tidyselect::ends_with
#' @export
#' @keywords internal
tidyselect::everything
#' @export
#' @keywords internal
tidyselect::last_col
#' @export
#' @keywords internal
tidyselect::matches
#' @export
#' @keywords internal
tidyselect::num_range
#' @export
#' @keywords internal
tidyselect::one_of
#' @export
#' @keywords internal
tidyselect::starts_with

#' @title Internal infrastructure function.
#' @export
#' @keywords internal
#' @description Not a user-side function.
#'   Only use for developing external HPC backend packages.
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
