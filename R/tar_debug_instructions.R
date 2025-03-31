#' @title Print instructions for debugging a target.
#' @export
#' @keywords internal
#' @description Not a user-side function. Do not call directly.
#' @return `NULL` (invisibly). Messages are printed out.
#' @examples
#' tar_debug_instructions()
tar_debug_instructions <- function() {
  name <- targets::tar_name()
  expr <- targets::tar_definition()$command$expr
  expr <- if_any(length(expr) >= 3L, expr[[3L]], NULL)
  if (is.expression(expr)) {
    expr <- expr[[1]]
  }
  deparsed <- targets::tar_deparse_safe(expr)
  text <- paste("    ", deparsed)
  cli::cli_alert(
    sprintf(
      "You are now running an interactive debugger in target %s.",
      name
    )
  )
  cli::cli_inform(
    "You can enter code and print objects as with the normal R console."
  )
  cli::cli_inform("How to use: https://adv-r.hadley.nz/debugging.html#browser")
  message()
  cli::cli_alert(
    paste0(
      "The debugger is poised to run the command of target ",
      name, ":"
    )
  )
  message()
  message(text)
  message()
  cli::cli_alert("Tip: run debug(your_function) and then enter \"c\"")
  cli::cli_inform("to move the debugger inside your_function(),")
  cli::cli_inform(
    sprintf(
      "where your_function() is called from the command of target %s.",
      name
    )
  )
  cli::cli_inform(
    "Then debug the function as you would normally (without `targets`)."
  )
}
