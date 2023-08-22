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
#' @return A character vector of rendered the reprex, invisibly.
#' @param pipeline R code for the target script file `_targets.R`.
#'   `library(targets)` is automatically written at the top.
#' @param run R code to inspect and run the pipeline.
#' @param ... Named arguments passed to `reprex::reprex()`.
#' @examples
#' if (identical(Sys.getenv("TAR_INTERACTIVE_EXAMPLES"), "true")) {
#' tar_reprex(
#'   pipeline = {
#'     list(
#'       tar_target(data, data.frame(x = sample.int(1e3))),
#'       tar_target(summary, mean(data$x, na.rm = TRUE))
#'     )
#'   },
#'   run = {
#'     tar_visnetwork()
#'     tar_make()
#'   }
#' )
#' }
# Tested in tests/testthat/test-tar_reprex.R
# nocov start
tar_reprex <- function(
  pipeline = tar_target(example_target, 1),
  run = tar_make(),
  ...
) {
  tar_assert_allow_meta("tar_reprex")
  tar_assert_package("reprex")
  library <- "library(targets)"
  pipeline <- as.call(list(quote(tar_script), substitute(pipeline)))
  pipeline <- deparse_script_code(pipeline)
  run <- deparse_script_code(substitute(run))
  lines <- c(library, pipeline, run)
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
