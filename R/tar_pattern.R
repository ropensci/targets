#' @title Emulate dynamic branching.
#' @export
#' @family branching
#' @aliases map cross head tail sample
#' @description Emulate the dynamic branching process outside a pipeline.
#'   `tar_pattern()` can help you understand the overall branching structure
#'   that comes from the `pattern` argument of [tar_target()].
#' @details Dynamic branching is a way to programmatically
#'   create multiple new targets based on the values of other targets,
#'   all while the pipeline is running. Use the `pattern` argument of
#'   [tar_target()] to get started. `pattern` accepts a function call
#'   composed of target names and any of the following patterns:
#'   * `map()`: iterate over one or more targets in sequence.
#'   * `cross()`: iterate over combinations of slices of targets.
#'   * `slice()`: select one or more slices by index, e.g.
#'     `slice(x, index = c(3, 4))` selects the third and fourth
#'     slice or branch of `x`.
#'   * `head()`: restrict branching to the first few elements.
#'   * `tail()`: restrict branching to the last few elements.
#'   * `sample()`: restrict branching to a random subset of elements.
#' @return A `tibble` showing the kinds of dynamic branches that
#'   [tar_target()] would create in a real pipeline with the given `pattern`.
#'   Each row is a dynamic branch, each column is a dependency target,
#'   and each element is the name of an upstream bud or branch that the
#'   downstream branch depends on. Buds are pieces of non-branching targets
#'   ("stems") and branches are pieces of patterns. The returned bud and branch
#'   names are not the actual ones you will see when you run the pipeline,
#'   but they do communicate the branching structure of the pattern.
#' @param pattern Function call with the pattern specification.
#' @param ... Named integers, each of length 1.
#'   Each name is the name of a dependency target,
#'   and each integer is the length of the target
#'   (number of branches or slices). Names must be unique.
#' @param seed Integer of length 1, random number generator seed to
#'   emulate the pattern reproducibly. (The `sample()` pattern is random).
#'   In a real pipeline, the seed is automatically generated
#'   from the target name in deterministic fashion.
#' @examples
#' # To use dynamic map for real in a pipeline,
#' # call map() in a target's pattern.
#' # The following code goes at the bottom of
#' # your target script file (default: `_targets.R`).
#' list(
#'   tar_target(x, seq_len(2)),
#'   tar_target(y, head(letters, 2)),
#'   tar_target(dynamic, c(x, y), pattern = map(x, y)) # 2 branches
#' )
#' # Likewise for more complicated patterns.
#' list(
#'   tar_target(x, seq_len(2)),
#'   tar_target(y, head(letters, 2)),
#'   tar_target(z, head(LETTERS, 2)),
#'   tar_target(dynamic, c(x, y, z), pattern = cross(z, map(x, y))) #4 branches
#' )
#' # But you can emulate dynamic branching without running a pipeline
#' # in order to understand the patterns you are creating. Simply supply
#' # the pattern and the length of each dependency target.
#' # The returned data frame represents the branching structure of the pattern:
#' # One row per new branch, one column per dependency target, and
#' # one element per bud/branch in each dependency target.
#' tar_pattern(
#'   cross(x, map(y, z)),
#'   x = 2,
#'   y = 3,
#'   z = 3
#' )
#' tar_pattern(
#'   head(cross(x, map(y, z)), n = 2),
#'   x = 2,
#'   y = 3,
#'   z = 3
#' )
tar_pattern <- function(pattern, ..., seed = 0L) {
  pattern <- as.expression(substitute(pattern))
  tar_assert_expr(pattern, "pattern must be language.")
  lengths <- list(...)
  tar_pattern_tar_assert_lengths(lengths)
  lengths <- lapply(lengths, as.integer)
  niblings <- map(names(lengths), ~ tar_pattern_nibling(.x, lengths[[.x]]))
  niblings <- set_names(niblings, names(lengths))
  methods <- dynamic_init()
  out <- pattern_produce_grid(
    pattern = pattern,
    niblings = niblings,
    seed = seed,
    methods = methods
  )
  tibble::as_tibble(out)
}

tar_pattern_tar_assert_lengths <- function(lengths) {
  msg <- "all arguments in ... must be named and have unique names."
  names <- names(lengths)
  tar_assert_nonempty(names, msg)
  tar_assert_chr(names, msg)
  tar_assert_nzchar(names, msg)
  tar_assert_none_na(names, msg)
  tar_assert_unique(names)
  tar_assert_identical(length(names), length(lengths), msg)
  msg <- "... must be integers of length 1."
  map(lengths, tar_assert_dbl, msg = msg)
  map(lengths, tar_assert_scalar, msg = msg)
}

tar_pattern_nibling <- function(name, length) {
  set_names(data_frame(x = paste(name, seq_len(length), sep = "_")), name)
}
