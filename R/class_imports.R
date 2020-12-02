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
  lapply(packages, imports_set_package, imports = imports)
  imports_set_envir(imports = imports, envir = envir)
  imports_new(imports)
}

imports_new <- function(envir) {
  enclass(envir, "tar_imports")
}

imports_set_package <- function(imports, package) {
  envir <- getNamespace(package)
  imports_set_envir(imports, envir)
}

imports_set_envir <- function(imports, envir) {
  lapply(names(envir), imports_set_object, imports = imports, envir = envir)
}

imports_set_object <- function(imports, name, envir) {
  assign(
    x = name,
    value = get(name, envir = envir, inherits = FALSE),
    envir = imports,
    inherits = FALSE
  )
}

imports_validate <- function(imports) {
  assert_inherits(imports, "tar_imports")
  assert_envir(imports)
}
