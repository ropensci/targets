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
#'   `targets` and `tarchetypes` take the approach discussed in the
#'   aforementioned section of the paper using the
#'   `secretbase` package by Charlie Gao (2024) \doi{10.5281/zenodo.10553140}.
#'   To generate the 32-bit integer `seed` argument of `set.seed()`
#'   for each target, `secretbase` generates a cryptographic SHA3 hash
#'   and robustly converts it to 32-bit output using the SHAKE256
#'   extendable output function (XOF). `secretbase` uses algorithms from
#'   the `Mbed TLS` C library.
#' @return Integer of length 1, the target seed.
#' @param name Character of length 1, target name.
#' @param global_seed Integer of length 1, the overarching global
#'   pipeline seed which governs the seeds of all the targets.
#'   Set to `NULL` to default to `tar_option_get("seed")`.
#'   Set to `NA` to disable seed setting in `targets` and make
#'   [tar_seed_create()] return `NA_integer_`.
#' @references
#'   * Gao C (2024). `secretbase`: Cryptographic Hash and
#'     Extendable-Output Functions. R package version 0.1.0,
#'     \doi{10.5281/zenodo.10553140}.
#'   * Pierre L'Ecuyer, David Munger, Boris Oreshkin, and Richard Simard
#'     (2017). Random numbers for parallel computers: Requirements and methods,
#'     with emphasis on GPUs. Mathematics and Computers in Simulation,
#'     135, 3-17. \doi{10.1016/j.matcom.2016.05.005}.
tar_seed_create <- function(name, global_seed = NULL) {
  if (is.null(global_seed)) {
    global_seed <- tar_options$get_seed()
  }
  if (is.null(global_seed) || anyNA(global_seed)) {
    return(NA_integer_)
  }
  x <- list(as.character(name), as.integer(global_seed))
  secretbase::sha3(x = x, bits = 32L, convert = NA)
}
