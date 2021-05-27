.onLoad <- function(libname, pkgname) {
  if (requireNamespace("knitr", quietly = TRUE)) {
    knitr::knit_engines$set(targets = tar_engine)
  }
}