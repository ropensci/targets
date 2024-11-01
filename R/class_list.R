list_new <- function(object = NULL) {
  out <- new.env(parent = emptyenv(), hash = FALSE)
  out$object <- object
  enclass(out, list_s3_class)
}

list_s3_class <- c("tar_list", "tar_value")

#' @export
value_count_slices.tar_list <- function(value) {
  length(value$object)
}

#' @export
value_produce_slice.tar_list <- function(value, index) {
  value$object[[index]]
}

#' @export
value_produce_aggregate.tar_list <- function(value, objects) { #nolint
  objects
}
