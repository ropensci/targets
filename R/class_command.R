command_init <- function(
  expr = expression(NULL),
  packages = character(0),
  library = NULL,
  seed = 0L,
  deps = NULL,
  string = NULL
) {
  expr <- as.expression(expr)
  deps <- deps %||% deps_function(embody_expr(expr))
  string <- string %||% mask_pointers(safe_deparse(expr))
  hash <- digest_chr64(string)
  command_new(expr, packages, library, deps, seed, string, hash)
}

command_new <- function(
  expr = NULL,
  packages = NULL,
  library = NULL,
  deps = NULL,
  seed = NULL,
  string = NULL,
  hash = NULL
) {
  force(expr)
  force(packages)
  force(library)
  force(deps)
  force(seed)
  force(string)
  force(hash)
  environment()
}

command_load_packages <- function(command) {
  lapply(
    command$packages,
    require,
    lib.loc = command$library,
    quietly = TRUE,
    character.only = TRUE
  )
}

command_produce_build <- function(command, envir) {
  build_init(
    command$expr,
    new.env(parent = envir),
    command$seed
  )
}

command_clone <- function(command) {
  out <- command_new(
    command$expr,
    command$packages,
    command$library,
    command$deps,
    command$seed
  )
  out$string <- command$string
  out$hash <- command$hash
  out
}

command_validate_packages <- function(command) {
  packages <- command$packages
  assert_chr(packages)
  library <- command$library
  package_info <- utils::installed.packages(lib.loc = library)
  installed <- package_info[, "Package", drop = TRUE]
  if (!all(packages %in% installed)) {
    missing <- paste(setdiff(packages, installed), collapse = ", ")
    throw_validate("packages not installed: ", missing)
  }
}

command_validate_library <- function(library) {
  if (!is.null(library)) {
    assert_chr(library)
  }
}

command_validate <- function(command) {
  assert_correct_fields(command, command_new)
  assert_expr(command$expr)
  command_validate_library(command$library)
  command_validate_packages(command)
  assert_chr(command$deps)
  assert_int(command$seed)
  assert_scalar(command$seed)
  assert_chr(command$string)
  assert_scalar(command$string)
  assert_chr(command$hash)
  assert_scalar(command$hash)
}

command_null <- command_new(
  expr = expression(NULL),
  packages = character(0),
  deps = character(0),
  seed = 0L,
  string = "",
  hash = ""
)
