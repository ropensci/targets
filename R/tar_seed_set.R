#' @title Set a seed to run a target.
#' @export
#' @family pseudo-random number generation
#' @description `targets` generates its own target-specific seeds
#'   using [tar_seed_create()]. Use [tar_seed_set()] to set one of
#'   these seeds in R.
#' @details [tar_seed_set()] gives the user-supplied `seed` to
#'   `set.seed()` and sets arguments `kind = "default"`,
#'   `normal.kind = "default"`, and `sample.kind = "default"`.
#' @inheritSection tar_seed_create Seeds
#' @inheritSection tar_seed_create RNG overlap
#' @references
#'   * Gao C (2024). `secretbase`: Cryptographic Hash and
#'     Extendable-Output Functions. R package version 0.1.0,
#'     \doi{10.5281/zenodo.10553140}.
#'   * Pierre L'Ecuyer, David Munger, Boris Oreshkin, and Richard Simard
#'     (2017). Random numbers for parallel computers: Requirements and methods,
#'     with emphasis on GPUs. Mathematics and Computers in Simulation,
#'     135, 3-17. \doi{10.1016/j.matcom.2016.05.005}.
#' @return `NULL` (invisibly).
#' @param seed Integer of length 1, value of the seed to set
#'   with `set.seed()`.
#' @examples
#' seed <- tar_seed_create("target_name")
#' seed
#' sample(10)
#' tar_seed_set(seed)
#' sample(10)
#' tar_seed_set(seed)
#' sample(10)
tar_seed_set <- function(seed) {
  if (!is.null(seed) && !anyNA(seed)) {
    set.seed(
      seed = seed,
      kind = "default",
      normal.kind = "default",
      sample.kind = "default"
    )
  }
  invisible()
}
