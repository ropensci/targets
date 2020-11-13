#' @title Dynamic cross
#' @export
#' @aliases cross
#' @family Dynamic branching
#' @description Branch over all combinations of elements in a pattern.
#' @details In [tar_target()], `pattern = cross(x, y)`
#'   creates one dynamic branch
#'   for each combination of elements of `x` and `y`.
#'   To test and experiment with this behavior outside
#'   the pipeline, use the `tar_dynamic_cross()` function.
#' @inheritParams tar_dynamic_map
#' @examples
#' # To use dynamic cross for real in a pipeline,
#' # call cross() in a target's pattern.
#' # The following code goes at the bottom of _targets.R.
#' tar_pipeline(
#'   tar_target(x, seq_len(2)),
#'   tar_target(y, head(letters, 2)),
#'   tar_target(dynamic, c(x, y), pattern = cross(x, y)) # 4 branches
#' )
#' # To explore dynamic cross() outside a pipeline,
#' # use the tar_dynamic_cross() helper function.
#' x <- data.frame(x = seq_len(2))
#' y <- data.frame(y = head(letters, 2))
#' tar_dynamic_cross(x, y)
#' # You can experiment with more complicated patterns.
#' # The following is a representation of pattern = cross(z, map(x, y)).
#' z <- data.frame(z = tail(LETTERS, 2))
#' tar_dynamic_cross(z, tar_dynamic_map(x, y))
tar_dynamic_cross <- function(...) {
  assert_child_dfs(...)
  dynamic_methods$cross(...)
}
