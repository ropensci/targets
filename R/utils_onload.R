.onLoad <- function(libname, pkgname) {
  # Cannot cover .onLoad().
  # nocov start
  # TODO: remove when knitr 1.34 is super old.
  if (utils::packageVersion("knitr") < "1.34") {
    engine_knitr_set()
  }
  # nocov end
}
