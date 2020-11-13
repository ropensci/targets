#' @title Dynamic sample
#' @export
#' @aliases sample
#' @family Dynamic branching
#' @description Branch over randomly sampled elements in a pattern.
#' @details In [tar_target()], `pattern = sample(x, n = 5)` branches over
#'   a random 5 elements of `x`. The number random seed is set to the seed
#'   of the target so branch selection is reproducible.
#'   `sample()` can be composed with other kinds of patterns: for example,
#'   `pattern = sample(map(x, y), n = 5)`.
#'   To test and experiment with this behavior outside
#'   the pipeline, use the `tar_dynamic_sample()` function.
#' @inheritParams tar_dynamic_head
#' @param seed Integer of length 1, random number generator seed.
#'   Within a target's pattern, e.g. `pattern = sample(x, n = 5)`
#'   in [tar_target()], there is not an option to supply a seed.
#'   Instead, the seed is generated from the target name in
#'   a deterministic manner for the sake of reproducibility.
#' @examples
#' # To use dynamic sample for real in a pipeline,
#' # call sample() in a target's pattern.
#' # The following code goes at the bottom of _targets.R.
#' tar_pipeline(
#'   tar_target(x, seq_len(26)),
#'   tar_target(dynamic, x, pattern = sample(x, n = 2)) # 2 branches
#' )
#' # To explore dynamic sample() outside a pipeline,
#' # use the tar_dynamic_sample() helper function.
#' x <- data.frame(x = seq_len(26))
#' tar_dynamic_sample(x, n = 2)
#' # You can experiment with more complicated patterns.
#' # The following is a representation of pattern = sample(map(x, y), n = 2).
#' y <- data.frame(y = letters)
#' tar_dynamic_sample(tar_dynamic_map(x, y), n = 2)
tar_dynamic_sample <- function(x, n = 1L, seed = 0L) {
  n <- as.integer(n)
  assert_child_dfs(x)
  assert_scalar(n)
  assert_int(n)
  assert_scalar(seed)
  assert_int(seed)
  withr::with_seed(seed, dynamic_methods$sample(x = x, n = n))
}
