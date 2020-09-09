group_new <- function(object = NULL) {
  enclass(environment(), c("tar_group", "tar_value"))
}

#' @export
value_count_slices.tar_group <- function(value) {
  value_validate(value)
  max(value$object$tar_group)
}

#' @export
value_produce_slice.tar_group <- function(value, index) { # nolint
  which <- as.integer(value$object$tar_group) == as.integer(index)
  value$object[which,, drop = FALSE] # nolint
}

#' @export
value_produce_aggregate.tar_group <- function(value, objects) { # nolint
  do.call(vctrs::vec_rbind, objects)
}

#' @export
value_validate.tar_group <- function(value) {
  assert_df(
    value$object,
    msg = "iteration = \"group\" requires data frame targets."
  )
  assert_in(
    "tar_group",
    choices = colnames(value$object),
    msg = paste(
      "iteration = \"group\" requires a special tar_group column.",
      "See the iteration argument in the tar_target() help file",
      "for details."
    )
  )
}
