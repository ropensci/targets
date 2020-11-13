#' @title Dynamic tail
#' @export
#' @aliases tail
#' @family Dynamic branching
#' @description Branch over the last few elements of a pattern.
#' @details In [tar_target()], `pattern = tail(x, n = 5)`
#'   branches over the last 5 elements of `x`. `tail()` can be
#'   composed with other kinds of patterns: for example,
#'   `pattern = tail(map(x, y), n = 5)`.
#'   To test and experiment with this behavior outside
#'   the pipeline, use the `tar_dynamic_tail()` function.
#' @inheritParams tar_dynamic_head
#' @examples
#' # To use dynamic tail for real in a pipeline,
#' # call tail() in a target's pattern.
#' # The following code goes at the bottom of _targets.R.
#' tar_pipeline(
#'   tar_target(x, seq_len(26)),
#'   tar_target(dynamic, c(x, y), pattern = tail(x, n = 2)) # 2 branches
#' )
#' # To explore dynamic tail() outside a pipeline,
#' # use the tar_dynamic_tail() helper function.
#' x <- data.frame(x = seq_len(26))
#' tar_dynamic_tail(x, n = 2)
#' # You can experiment with more complicated patterns.
#' # The following is a representation of pattern = tail(map(x, y), n = 2).
#' y <- data.frame(y = letters)
#' tar_dynamic_tail(tar_dynamic_map(x, y), n = 2)
tar_dynamic_tail <- function(x, n = 1L) {
  n <- as.integer(n)
  assert_child_dfs(x)
  assert_scalar(n)
  assert_int(n)
  dynamic_methods$tail(x = x, n = n)
}
