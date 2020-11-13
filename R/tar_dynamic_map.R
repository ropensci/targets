#' @title Dynamic map
#' @export
#' @alias map
#' @family Dynamic branching
#' @description Iterate over one or more targets in sequence.
#' @details In [tar_target()], `pattern = map(x, y)` creates one dynamic branch
#'   for the first element of `x` and the first element of `y`,
#'   another for the second element of `x` and the second element of `y`,
#'   and so on. To test and experiment with this behavior outside
#'   the pipeline, use the `tar_dynamic_map()` function.
#' @param ... data frames with columns to represent targets and
#'   elements to represent branches or buds. Column names must
#'   be unique across all the data frames.
#' @examples
#' # To use dynamic map for real in a pipeline,
#' # call map() in a target's pattern.
#' # The following code goes at the bottom of _targets.R.
#' tar_pipeline(
#'   tar_target(x, seq_len(2)),
#'   tar_target(y, head(letters, 2)),
#'   tar_target(dynamic, c(x, y), pattern = map(x, y)) # 2 branches
#' )
#' # To explore dynamic map() outside a pipeline,
#' # use the tar_dynamic_map() helper function.
#' x <- data.frame(x = seq_len(2))
#' y <- data.frame(y = head(letters, 2))
#' tar_dynamic_map(x, y)
#' # You can experiment with more complicated patterns.
#' # The following is a representation of pattern = cross(z, map(x, y)).
#' z <- data.frame(z = tail(LETTERS, 2))
#' tar_dynamic_cross(z, tar_dynamic_map(x, y))
tar_dynamic_map <- function(...) {
  assert_child_dfs(...)
  dynamic_methods$map(...)
}
