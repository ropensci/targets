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
  old <- getOption("width")
  on.exit(options(width = old))
  options(width = old - 4L)
  deparsed <- targets::tar_deparse_safe(expr)
  text <- unlist(strsplit(trimws(deparsed), split = "\n"))
  text <- paste(paste0("    ", text), collapse = "\n")
  lines <- paste(
    sprintf(
      "\n\n  You are now running an interactive debugger in target %s.",
      name
    ),
    "  You can enter code and print objects as with the normal R console.",
    "  How to use: https://adv-r.hadley.nz/debugging.html#browser\n",
    paste0(
      "  The debugger is poised to run the command of target ",
      name,
      ":"
    ),
    sprintf("\n%s\n", text),
    "  Tip: run debug(your_function) and then enter \"c\"",
    "  to move the debugger inside your_function(),",
    sprintf(
      "  where your_function() is called from the command of target %s.",
      name
    ),
    "  Then debug the function as you would normally (without `targets`).\n",
    sep = "\n"
  )
  tar_message_run(lines)
}
