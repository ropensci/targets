.onLoad <- function(libname, pkgname) {
  if (requireNamespace("knitr", quietly = TRUE)) {
    eval_after <- knitr::opts_knit$get("eval.after")
    knitr::opts_knit$set(eval.after = c(eval_after, "pattern", "cue"))
    knitr::knit_engines$set(tar_target = tar_engine_tar_target)
  }
}