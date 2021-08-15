#' @title Run if Target Markdown interactive mode is on.
#' @export
#' @family utilities
#' @description In Target Markdown, run the enclosed code
#'   only if interactive mode is activated. Otherwise,
#'   do not run the code.
#' @details Visit <books.ropensci.org/targets/markdown.html>
#'   to learn about Target Markdown and interactive mode.
#' @return If Target Markdown interactive mode is turned on,
#'   the function returns the result of running the code.
#'   Otherwise, the function invisibly returns `NULL`.
#' @examples
#' tar_interactive(1 + 1)
tar_interactive <- function(code) {
  if_any(
    engine_knitr_is_interactive(),
    force(code),
    invisible()
  )
}
