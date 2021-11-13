#' @title Reproducible example of `targets` with `reprex`
#' @export
#' @family help
#' @description Create a reproducible example of a `targets`
#'   pipeline with the `reprex` package.
#' @details The best way to get help with an issue is to
#'   create a reproducible example of the problem
#'   and post it to <https://github.com/ropensci/targets/discussions>
#'   `tar_reprex()` facilitates this process. It is like
#'   `reprex::reprex({targets::tar_script(...); tar_make()})`,
#'   but more convenient.
#' @param code_pipeline R code for the target script file `_targets.R`.
#'   `library(targets)` is automatically written at the top.
#' @param code_run R code to inspect and run the pipeline.
#' @param ... Named arguments passed to `reprex::reprex()`.
#' @examples
#' if (identical(Sys.getenv("TAR_INTERACTIVE_EXAMPLES"), "true")) {
#' tar_reprex()
#' }
# Tested in tests/testthat/test-tar_reprex.R
# nocov start
tar_reprex <- function(
  code_pipeline = {
    list(
      tar_target(data, airquality),
      tar_target(summary, mean(data$Ozone, na.rm = TRUE))
    )
  },
  code_run = {
    tar_visnetwork()
    tar_make()
  },
  ...
) {
  tar_assert_package("reprex")
  code_library <- "library(targets)"
  code_pipeline <- as.call(list(quote(tar_script), substitute(code_pipeline)))
  code_pipeline <- deparse_script_code(code_pipeline)
  code_run <- deparse_script_code(substitute(code_run))
  lines <- c(code_library, code_pipeline, code_run)
  dir <- tempfile()
  dir_create(dir)
  path <- file.path(dir, basename(tempfile()))
  writeLines(text = lines, con = path)
  args <- list(...)
  args$x <- NULL
  args$input <- path
  do.call(what = reprex::reprex, args = args)
}
# nocov end
