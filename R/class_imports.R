imports_init <- function(envir) {
  UseMethod("imports_init")
}

#' @export
imports_init.tar_imports <- function(envir) {
  envir
}

#' @export
imports_init.default <- function(envir) {
  imports <- new.env(parent = emptyenv())
  packages <- rev(tar_option_get("imports"))
  map(packages, ~import_envir(from = getNamespace(.x), into = imports))
  import_envir(from = envir, into = imports)
  enclass(imports, "tar_imports")
}

import_envir <- function(from, into) {
  lapply(names(from), import_object, from = from, into = into)
}

import_object <- function(name, from, into) {
  assign(
    x = name,
    value = get(name, envir = from, inherits = FALSE),
    envir = into,
    inherits = FALSE
  )
}
