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
  text <- paste("    ", targets::tar_deparse_safe(expr))
  cli_mark_info(
    sprintf(
      "You are now running an interactive debugger in target %s.",
      name
    )
  )
  cli_blank(
    "You can enter code and print objects as with the normal R console."
  )
  cli_blank("How to use: https://adv-r.hadley.nz/debugging.html#browser")
  message()
  cli_mark_info(
    paste0(
      "The debugger is poised to run the command of target ",
      name, ":"
    )
  )
  message()
  message(text)
  message()
  if (is.call(expr)) {
    cli_mark_info(
      paste0(
        "Tip: run ",
        sprintf("debug(%s)", deparse(expr)),
        " and then enter \"c\""
      )
    )
    cli_blank(
      paste0(
        "to move the debugger inside function ",
        deparse(expr),
        "()."
      )
    )
  } else {
    cli_mark_info("Tip: run debug(your_function) and then enter \"c\"")
    cli_blank("to move the debugger inside your_function(),")
    cli_blank(
      sprintf(
        "where your_function() is called from the command of target %s.",
        name
      )
    )
  }
  cli_blank(
    "Then debug the function as you would normally (without {targets})."
  )
}
