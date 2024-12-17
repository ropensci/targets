vector_new <- function(object = NULL) {
  out <- new.env(parent = emptyenv(), hash = FALSE)
  out$object <- object
  enclass(out, vector_s3_class)
}

vector_s3_class <- c("tar_vector", "tar_value")

#' @export
value_count_slices.tar_vector <- function(value) {
  vctrs::vec_size(value$object)
}

#' @export
value_produce_slice.tar_vector <- function(value, index) { # nolint
  vctrs::vec_slice(x = value$object, i = index)
}

#' @export
value_produce_aggregate.tar_vector <- function(value, objects) { # nolint
  tar_vec_c(objects)
}

tar_vec_c <- function(objects) {
  tryCatch(
    vec_c_spec(objects),
    # Covered in tests/testthat/test-class_vector.R
    # but might not register in covr.
    # nocov start
    error = function(condition) {
      do.call(vctrs::vec_c, unname(objects))
    }
    # nocov end
  )
}

vec_c_spec <- function(objects) {
  objects$.name_spec <- "{outer}_{inner}"
  do.call(vctrs::vec_c, objects)
}
