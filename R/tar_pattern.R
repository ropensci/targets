#' @title Emulate dynamic branching.
#' @export
#' @aliases map cross head tail sample
#' @description Emulate the dynamic branching process outside a pipeline.
#'   `tar_pattern()` can help you understand which branches
#'   will be created when you set the `pattern` argument of [tar_target()].
#' @details Dynamic branching is a way to programmatically
#'   create multiple new targets based on the values of other targets,
#'   all while the pipeline is running. Use the `pattern` argument of
#'   [tar_target()] to get started. `pattern` accepts a function call
#'   composed of target names and any of the following patterns:
#'   * `map()`: iterate over one or more targets in sequence.
#'   * `cross()`: iterate over combinations of slices of targets.
#'   * `head()`: restrict branching to the first few elements.
#'   * `tail()`: restrict branching to the last few elements.
#'   * `sample()`: restrict branching to a random subset of elements.
#' @return A `tibble` showing the kinds of dynamic branches that
#'   [tar_target()] would create in a real pipeline with the given `pattern`.
#'   Each row is a dynamic branch, each column is a dependency target,
#'   and each element is the name of an upstream bud or branch that the
#'   downstream branch depends on. Buds are pieces of non-branching targets
#'   ("stems") and branches are pieces of patterns.
#' @param pattern Function call with the pattern specification.
#' @param ... Named vectors to represent upstream targets.
#'   Each name is the name of a whole stem or pattern,
#'   and each vector element is a character with the name of an
#'   upstream bud or branch. Buds are pieces of non-branching targets
#'   ("stems") and branches are pieces of patterns. Names must be unique.
#'   If you do not supply character vectors, they will be coerced to
#'   character vectors.
#' @param seed Integer of length 1, random number generator seed to
#'   emulate the pattern reproducibly. (The `sample()` pattern is random).
#'   In a real pipeline, the seed is automatically generated
#'   from the target name in deterministic fashion.
#' @examples
#' # To use dynamic map for real in a pipeline,
#' # call map() in a target's pattern.
#' # The following code goes at the bottom of _targets.R.
#' tar_pipeline(
#'   tar_target(x, seq_len(2)),
#'   tar_target(y, head(letters, 2)),
#'   tar_target(dynamic, c(x, y), pattern = map(x, y)) # 2 branches
#' )
#' # Likewise for more complicated patterns.
#' tar_pipeline(
#'   tar_target(x, seq_len(2)),
#'   tar_target(y, head(letters, 2)),
#'   tar_target(z, head(LETTERS, 2)),
#'   tar_target(dynamic, c(x, y, z), pattern = cross(z, map(x, y))) #4 branches
#' )
#' # But you can emulate dynamic branching without running a pipeline
#' # in order to understand the patterns you are creating.
#' # First, you have to make up some names for pieces of targets
#' # ("buds" and branches). The following are reasonably realistic branch names
#' # that `targets` might generate during [tar_make()], but they need not be
#' # totally realistic for emulation purposes.
#' x <- c("x_550d7456", "x_a20cadbf", "x_eeb00f1b")
#' y <- c("x_42f35290", "x_f95ad1c7")
#' z <- c("x_67188e74", "x_5512ec97")
#' # Then, call `tar_pattern()` to emulate the pattern.
#' tar_pattern(
#'   cross(x, map(y, z)),
#'   x = x,
#'   y = y,
#'   z = z
#' )
#' tar_pattern(
#'   head(cross(x, map(y, z)), n = 2),
#'   x = x,
#'   y = y,
#'   z = z
#' )
tar_pattern <- function(pattern, ..., seed = 0L) {
  pattern <- as.expression(substitute(pattern))
  assert_expr(pattern, "pattern must be language.")
  args <- lapply(list(...), as.character)
  tar_pattern_assert_args(args)
  niblings <- map(names(args), ~set_names(data_frame(x = args[[.x]]), .x))
  niblings <- set_names(niblings, names(args))
  methods <- dynamic_init()
  pattern_produce_grid(
    pattern = pattern,
    niblings = niblings,
    seed = seed,
    methods = methods
  )
}

tar_pattern_assert_args <- function(args) {
  msg <- "all arguments in ... must be named and have unique names."
  names <- names(args)
  assert_nonempty(names, msg)
  assert_chr(names, msg)
  assert_nzchar(names, msg)
  assert_nonmissing(names, msg)
  assert_unique(names)
  assert_identical(length(names), length(args), msg)
  map(args, assert_chr, "... must be character vectors of bud/branch names.")
}
