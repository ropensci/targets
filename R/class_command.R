command_init <- function(
  expr = expression(NULL),
  packages = character(0),
  library = NULL,
  string = NULL
) {
  expr <- as.expression(expr)
  string <- string %|||% mask_pointers(tar_deparse_safe(expr))
  hash <- hash_object(string)
  command_new(expr, packages, library, string, hash)
}

command_new <- function(
  expr = NULL,
  packages = NULL,
  library = NULL,
  string = NULL,
  hash = NULL
) {
  out <- new.env(parent = emptyenv(), hash = FALSE)
  out$expr <- expr
  out$packages <- packages
  out$library <- library
  out$string <- string
  out$hash <- hash
  out
}

command_produce_build <- function(command, seed, envir) {
  build_init(
    expr = command$expr,
    envir = envir,
    seed = seed,
    packages = command$packages,
    library = command$library
  )
}

command_validate <- function(command) {
  tar_assert_correct_fields(command, command_new)
  tar_assert_expr(command$expr)
  tar_assert_chr(command$packages)
  tar_assert_chr(command$library %|||% character(0))
  tar_assert_chr(command$string)
  tar_assert_scalar(command$string)
  tar_assert_chr(command$hash)
  tar_assert_scalar(command$hash)
}

command_null <- command_new(
  expr = expression(NULL),
  packages = character(0),
  string = "",
  hash = ""
)
