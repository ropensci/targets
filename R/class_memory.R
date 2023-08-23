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

memory_exists_object <- function(memory, name) {
  exists(name, envir = memory$envir, inherits = FALSE)
}

memory_get_object <- function(memory, name) {
  base::get(x = name, envir = memory$envir, inherits = FALSE)
}

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

memory_del_objects <- function(memory, names) {
  names <- intersect(memory$names, names)
  remove(list = names, envir = memory$envir, inherits = FALSE)
  memory$names <- setdiff(memory$names, names)
  memory$count <- memory$count - length(names)
  invisible()
}

memory_validate <- function(memory) {
  tar_assert_correct_fields(memory, memory_new)
  tar_assert_chr(memory$names)
  tar_assert_envir(memory$envir)
  tar_assert_in(memory$names, names(memory$envir))
  tar_assert_identical(length(memory$names), memory$count)
}
