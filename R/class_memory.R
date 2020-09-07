#' @title Internal infrastructure function.
#' @export
#' @keywords internal
#' @description Not a user-side function.
#'   Only use for developing external HPC backend packages.
#' @param envir Environment for the data.
memory_init <- function(envir = new.env(parent = emptyenv())) {
  names <- names(envir)
  memory_new(envir, names, length(names))
}

memory_new <- function(envir = NULL, names = NULL, count = NULL) {
  force(envir)
  force(names)
  force(count)
  environment()
}

#' @title Internal infrastructure function.
#' @export
#' @keywords internal
#' @description Not a user-side function.
#'   Only use for developing external HPC backend packages.
#' @param memory Memory object.
#' @param name Name of the data object to check.
memory_exists_object <- function(memory, name) {
  exists(name, envir = memory$envir, inherits = FALSE)
}

#' @title Internal infrastructure function.
#' @export
#' @keywords internal
#' @description Not a user-side function.
#'   Only use for developing external HPC backend packages.
#' @param memory Memory object.
#' @param name Name of the data object to get.
memory_get_object <- function(memory, name) {
  get(x = name, envir = memory$envir, inherits = FALSE)
}

#' @title Internal infrastructure function.
#' @export
#' @keywords internal
#' @description Not a user-side function.
#'   Only use for developing external HPC backend packages.
#' @param memory Memory object.
#' @param name Name of the data object to set.
#' @param object Data object to set.
memory_set_object <- function(memory, name, object) {
  if (!exists(name, envir = memory$envir, inherits = FALSE)) {
    memory$names <- c(memory$names, name)
    memory$count <- memory$count + 1L
  }
  assign(
    x = name,
    value = object,
    envir = memory$envir,
    inherits = FALSE,
    immediate = TRUE
  )
}

#' @title Internal infrastructure function.
#' @export
#' @keywords internal
#' @description Not a user-side function.
#'   Only use for developing external HPC backend packages.
#' @param memory Memory object.
#' @param names Names of the data objects to delete.
memory_del_objects <- function(memory, names) {
  names <- intersect(memory$names, names)
  remove(list = names, envir = memory$envir, inherits = FALSE)
  memory$names <- setdiff(memory$names, names)
  memory$count <- memory$count - length(names)
  invisible()
}

#' @title Internal infrastructure function.
#' @export
#' @keywords internal
#' @description Not a user-side function.
#'   Only use for developing external HPC backend packages.
#' @param memory Memory object.
memory_clear_objects <- function(memory) {
  memory_del_objects(memory, memory$names)
}

#' @title Internal infrastructure function.
#' @export
#' @keywords internal
#' @description Not a user-side function.
#'   Only use for developing external HPC backend packages.
#' @param memory Memory object.
memory_validate <- function(memory) {
  assert_correct_fields(memory, memory_new)
  assert_chr(memory$names)
  assert_envir(memory$envir)
  assert_in(sort(memory$names), sort(names(memory$envir)))
  assert_identical(length(memory$names), memory$count)
}
