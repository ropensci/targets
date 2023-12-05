#' @title Create a seed for a target.
#' @export
#' @family pseudo-random number generation
#' @description Create a seed for a target.
#' @section Seeds:
#'   A target's random number generator seed
#'   is a deterministic function of its name and the global pipeline seed
#'   from `tar_option_get("seed")`. Consequently,
#'
#'     1. Each target runs with a reproducible seed so that
#'        different runs of the same pipeline in the same computing
#'        environment produce identical results.
#'     2. No two targets in the same pipeline share the same seed.
#'        Even dynamic branches have different names and thus different seeds.
#'
#'   You can retrieve the seed of a completed target
#'   with `tar_meta(your_target, seed)`
#'   and run [tar_seed_set()] on the result to locally
#'   recreate the target's initial RNG state. [tar_workspace()]
#'   does this automatically as part of recovering a workspace.
#' @section RNG overlap:
#'   In theory, there is a risk that the pseudo-random number generator
#'   streams of different targets will overlap and produce statistically
#'   correlated results. (For a discussion of the motivating problem,
#'   see the Section 6: "Random-number generation" in the `parallel`
#'   package vignette: `vignette(topic = "parallel", package = "parallel")`.)
#'   However, this risk is extremely small in practice, as shown by
#'   L'Ecuyer et al. (2017) \doi{10.1016/j.matcom.2016.05.005}
#'   under "A single RNG with a 'random' seed for each stream" (Section 4:
#'   under "How to produce parallel streams and substreams").
#'
#'   `targets` and `tarchetypes` take the approach discussed in the
#'   aforementioned section of the paper, where [tar_seed_create()] plays the
#'   role of the upstream pseudo-random number generator (RNG) that produces
#'   seeds for the subsequent parallel streams. Specifically,
#'   [tar_seed_create()] acts as a counter-based RNG,
#'   where the output function is the SHA512 hash algorithm.
#'   (See "counter-based RNGs" under Section 3: "Main classes of RNGs
#'   for simulation".) There are two notable quirks, but they are innocuous:
#'
#'     1. Instead of a recursive integer counter, [tar_seed_create()] accepts
#'        the name of the target as the argument.
#'        Because target names map to small integers, this is equivalent to
#'        traversing the RNG sequence in variable small jumps. This does not
#'        affect the period length, and the argument in Section 4 of
#'        L'Ecuyer et al. (2017) still holds.
#'     2. [tar_seed_create()] converts the SHA512 hash into an
#'        integer using `digest::digest2int()`. This is necessary
#'        because integers in R can only be 32 bits. `set.seed()`
#'        cannot accept anything greater.
#' @return Integer of length 1, the target seed.
#' @param name Character of length 1, target name.
#' @param global_seed Integer of length 1, the overarching global
#'   pipeline seed which governs the seeds of all the targets.
#'   Set to `NULL` to default to `tar_option_get("seed")`.
#'   Set to `NA` to disable seed setting in `targets` and make
#'   [tar_seed_create()] return `NA_integer_`.
#' @references
#'   Pierre L'Ecuyer, David Munger, Boris Oreshkin, and Richard Simard
#'   (2017). Random numbers for parallel computers: Requirements and methods,
#'   with emphasis on GPUs. Mathematics and Computers in Simulation,
#'   135, 3-17. \doi{10.1016/j.matcom.2016.05.005}.
tar_seed_create <- function(name, global_seed = NULL) {
  if (is.null(global_seed)) {
    global_seed <- tar_options$get_seed()
  }
  if (is.null(global_seed) || anyNA(global_seed)) {
    return(NA_integer_)
  }
  name <- as.character(name)
  hash <- digest::digest(
    object = name,
    algo = "sha512",
    serialize = FALSE,
    file = FALSE,
    seed = 0L
  )
  digest::digest2int(x = hash, seed = global_seed)
}
