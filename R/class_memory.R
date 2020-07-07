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
  get(x = name, envir = memory$envir, inherits = FALSE)
}

memory_set_object <- function(memory, name, object) {
  assign(
    x = name,
    value = object,
    envir = memory$envir,
    inherits = FALSE,
    immediate = TRUE
  )
  memory_append_names(memory, name)
  memory$count <- memory$count + 1L
}

memory_del_objects <- function(memory, names) {
  names <- intersect(memory$names, names)
  remove(list = names, envir = memory$envir, inherits = FALSE)
  memory_del_names(memory, names)
  memory$count <- memory$count - length(names)
  invisible()
}

memory_clear_objects <- function(memory) {
  memory_del_objects(memory, memory$names)
}

memory_append_names <- function(memory, names) {
  memory$names <- base::union(memory$names, names)
}

memory_del_names <- function(memory, names) {
  memory$names <- setdiff(memory$names, names)
}

memory_validate <- function(memory) {
  assert_correct_fields(memory, memory_new)
  assert_chr(memory$names)
  assert_envir(memory$envir)
  assert_identical(sort(memory$names), sort(names(memory$envir)))
  assert_identical(length(memory$names), memory$count)
}
