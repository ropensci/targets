#' @title Create a counter object.
#' @export
#' @keywords internal
#' @description Internal function. Not for users.
#' @param names Character vector of names to set in the counter.
#' @examples
#' tar_counter(names = "x")
tar_counter <- function(names = NULL) {
  counter_init(names = names)
}

counter_init <- function(names = NULL) {
  count <- length(names)
  envir <- new.env(hash = TRUE, parent = emptyenv())
  map(names, assign, value = TRUE, envir = envir, inherits = FALSE)
  counter_new(count = count, envir = envir)
}

counter_new <- function(count = NULL, envir = NULL) {
  out <- new.env(parent = emptyenv(), hash = FALSE)
  out$count <- count
  out$envir <- envir
  out
}

counter_get_names <- function(counter) {
  names(counter$envir)
}

counter_exists_name <- function(counter, name) {
  !is.null(.subset2(.subset2(counter, "envir"), name))
  exists(name, envir = counter$envir, inherits = FALSE)
}

counter_exist_names <- function(counter, names) {
  envir <- .subset2(counter, "envir")
  as.logical(
    lapply(
      names,
      function(name) {
        !is.null(envir[[name]])
      }
    )
  )
}

counter_filter_exists <- function(counter, names) {
  names[counter_exist_names(counter, names)]
}

counter_set_name <- function(counter, name) {
  is_new <- !counter_exists_name(counter, name)
  assign(x = name, value = TRUE, envir = counter$envir)
  counter$count <- counter$count + is_new
}

counter_set_names <- function(counter, names) {
  lapply(names, counter_set_name, counter = counter)
}

counter_del_name <- function(counter, name) {
  if (counter_exists_name(counter, name)) {
    remove(list = name, envir = counter$envir)
    counter$count <- counter$count - 1L
  }
}

counter_del_names <- function(counter, names) {
  lapply(names, counter_del_name, counter = counter)
}

counter_validate <- function(counter) {
  tar_assert_correct_fields(counter, counter_new)
  tar_assert_int(counter$count)
  tar_assert_scalar(counter$count)
  tar_assert_envir(counter$envir)
  if (!identical(length(names(counter$envir)), counter$count)) {
    tar_throw_validate("envir does not match count.")
  }
}
