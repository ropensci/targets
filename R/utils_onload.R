.onLoad <- function(libname, pkgname) {
  if (requireNamespace("knitr", quietly = TRUE)) {
    knitr::opts_hooks$set(pattern = function(options) {
      browser()
      options$pattern <- deparse(substitute(options$pattern))
    })
    knitr::knit_engines$set(tar_target = tar_engine_tar_target)
  }
}