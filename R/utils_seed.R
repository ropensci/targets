# Borrowed from https://github.com/r-lib/withr/blob/main/R/seed.R
# under the MIT license. See the NOTICE file in the targets package source.
restore_seed <- function(old_seed) {
  if_any(
    is.null(old_seed), {
      set.seed(seed = NULL)
      rm(list = ".Random.seed", envir = .GlobalEnv)
    },
    assign(x = ".Random.seed", value = old_seed, envir = .GlobalEnv)
  )
}
