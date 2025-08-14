group_new <- function(object = NULL) {
  out <- new.env(parent = emptyenv(), hash = FALSE)
  out$object <- object
  enclass(out, group_s3_class)
}

group_s3_class <- c("tar_group", "tar_value")

#' @export
value_count_slices.tar_group <- function(value) {
  value_validate(value)
  max(value$object$tar_group)
}

#' @export
value_produce_slice.tar_group <- function(value, index) {
  # nolint
  which <- as.integer(value$object$tar_group) == as.integer(index)
  value$object[which, , drop = FALSE] # nolint
}

#' @export
value_produce_slice_kernel.tar_group <- function(value, index) {
  # nolint
  out <- value_produce_slice(value = value, index = index)
  out$tar_group <- NULL
  attr(out, "out.attrs") <- NULL
  out
}

#' @export
value_produce_aggregate.tar_group <- function(value, objects) {
  # nolint
  do.call(vctrs::vec_rbind, objects)
}

#' @export
value_validate.tar_group <- function(value) {
  tar_assert_df(
    value$object,
    msg = paste(
      "The pipeline cannot continue because",
      "it reached a target it cannot branch over:",
      "iteration = \"group\" requires the target to return a data frame,",
      "but the target either never completed or it returned a different",
      "kind of object.",
      "Check the errors and warnings in tar_meta() to troubleshoot."
    )
  )
  tar_assert_in(
    "tar_group",
    choices = colnames(value$object),
    msg = paste(
      "The pipeline cannot continue because",
      "it reached a target it cannot branch over:",
      "iteration = \"group\" requires the target to return a data frame",
      "with a special tar_group column.",
      "See the iteration argument in the tar_target() help file",
      "for details."
    )
  )
}
