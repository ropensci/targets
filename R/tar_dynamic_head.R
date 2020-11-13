#' @title Dynamic head
#' @export
#' @aliases head
#' @family Dynamic branching
#' @description Branch over the first few elements in a pattern.
#' @details In [tar_target()], `pattern = head(x, n = 5)`
#'   branches over the first 5 elements of `x`. `head()` can be
#'   composed with other kinds of patterns: for example,
#'   `pattern = head(map(x, y), n = 5)`.
#'   To test and experiment with this behavior outside
#'   the pipeline, use the `tar_dynamic_head()` function.
#' @param x Data frame with columns to represent targets and
#'   elements to represent branches or buds. Column names must
#'   be unique.
#' @param n Number of branches to create. Cannot exceed `nrow(x)`.
#' @examples
#' # To use dynamic head for real in a pipeline,
#' # call head() in a target's pattern.
#' # The following code goes at the bottom of _targets.R.
#' tar_pipeline(
#'   tar_target(x, seq_len(26)),
#'   tar_target(dynamic, x + 1, pattern = head(x, n = 2)) # 2 branches
#' )
#' # To explore dynamic head() outside a pipeline,
#' # use the tar_dynamic_head() helper function.
#' x <- data.frame(x = seq_len(26))
#' tar_dynamic_head(x, n = 2)
#' # You can experiment with more complicated patterns.
#' # The following is a representation of pattern = head(map(x, y), n = 2).
#' y <- data.frame(y = letters)
#' tar_dynamic_head(tar_dynamic_map(x, y), n = 2)
tar_dynamic_head <- function(x, n = 1L) {
  n <- as.integer(n)
  assert_child_dfs(x)
  assert_scalar(n)
  assert_int(n)
  dynamic_methods$head(x = x, n = n)
}
