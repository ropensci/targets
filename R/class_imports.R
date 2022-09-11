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
  imports_set_namespace(imports, package)
  imports_set_datasets(imports, package)
}

imports_set_datasets <- function(imports, package) {
  names <- as.character(utils::data(package = package)$results[, "Item"])
  lapply(
    names,
    imports_set_dataset_object,
    imports = imports,
    package = package
  )
}

imports_set_dataset_object <- function(name, imports, package) {
  utils::data(
    list = name,
    package = package,
    lib.loc = tar_options$get_library(),
    envir = imports
  )
}

imports_set_namespace <- function(imports, package) {
  envir <- getNamespace(package)
  exclude <- c(
    ".packageName",
    ".__NAMESPACE__.",
    ".__S3MethodsTable__."
  )
  names <- setdiff(names(envir), exclude)
  lapply(
    names,
    imports_set_envir_object,
    imports = imports,
    envir = envir
  )
}

imports_set_envir <- function(imports, envir) {
  lapply(
    names(envir),
    imports_set_envir_object,
    imports = imports,
    envir = envir
  )
}

imports_set_envir_object <- function(imports, name, envir) {
  value <- base::get(name, envir = envir, inherits = FALSE)
  assign(
    x = name,
    value = value,
    envir = imports,
    inherits = FALSE
  )
}

imports_validate <- function(imports) {
  tar_assert_inherits(imports, "tar_imports")
  tar_assert_envir(imports)
}
