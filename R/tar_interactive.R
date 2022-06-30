#' @title Run if Target Markdown interactive mode is on.
#' @export
#' @family Target Markdown
#' @description In Target Markdown, run the enclosed code
#'   only if interactive mode is activated. Otherwise,
#'   do not run the code.
#' @details Visit <books.ropensci.org/targets/literate-programming.html>
#'   to learn about Target Markdown and interactive mode.
#' @return If Target Markdown interactive mode is turned on,
#'   the function returns the result of running the code.
#'   Otherwise, the function invisibly returns `NULL`.
#' @param code R code to run if Target Markdown interactive mode
#'   is turned on.
#' @examples
#' tar_interactive(message("In interactive mode."))
tar_interactive <- function(code) {
  if_any(
    engine_knitr_is_interactive(),
    force(code),
    invisible()
  )
}
