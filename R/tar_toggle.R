#' @title Choose code to run based on Target Markdown mode.
#' @export
#' @family Target Markdown
#' @description Run one piece of code if Target Markdown mode
#'   interactive mode is turned on and another piece of code otherwise.
#' @details Visit <books.ropensci.org/targets/literate-programming.html>
#'   to learn about Target Markdown and interactive mode.
#' @return If Target Markdown interactive mode is not turned on,
#'   the function returns the result of running the code.
#'   Otherwise, the function invisibly returns `NULL`.
#' @param interactive R code to run if Target Markdown interactive mode is
#'   activated.
#' @param noninteractive R code to run if Target Markdown interactive mode is
#'   not activated.
#' @examples
#' tar_toggle(
#'   message("In interactive mode."),
#'   message("Not in interactive mode.")
#' )
tar_toggle <- function(interactive, noninteractive) {
  if_any(
    engine_knitr_is_interactive(),
    force(interactive),
    force(noninteractive)
  )
}
