value_init <- function(object = NULL, iteration = "vector") {
  switch(
    iteration,
    vector = vector_new(object),
    list = list_new(object),
    group = group_new(object),
    tar_throw_validate("unsupported iteration mode: ", iteration)
  )
}

value_new <- function(object = NULL) {
  enclass(environment(), "tar_value")
}

value_hash_slice <- function(value, index) {
  hash_object(value_produce_slice_kernel(value, index))
}

value_hash_slices <- function(value) {
  map_chr(seq_len(value_count_slices(value)), value_hash_slice, value = value)
}

value_count_slices <- function(value) {
  UseMethod("value_count_slices")
}

value_produce_slice <- function(value, index) {
  UseMethod("value_produce_slice")
}

value_produce_slice_kernel <- function(value, index) {
  UseMethod("value_produce_slice_kernel")
}

value_produce_aggregate <- function(value, objects) {
  UseMethod("value_produce_aggregate")
}

value_validate <- function(value) {
  UseMethod("value_validate")
}

#' @export
value_produce_slice_kernel.default <- function(value, index) { # nolint
  value_produce_slice(value = value, index = index)
}

#' @export
value_validate.tar_value <- function(value) {
  tar_assert_correct_fields(value, value_new)
  tar_assert_int(value_count_slices(value))
}
