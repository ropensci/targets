bud_new <- function(
  name = NULL,
  settings = NULL,
  index = NULL
) {
  out <- new.env(parent = emptyenv(), hash = FALSE)
  out$name <- name
  out$settings <- settings
  out$index <- index
  enclass(out, bud_s3_class)
}

bud_s3_class <- c("tar_bud", "tar_target")

#' @export
target_read_value.tar_bud <- function(target, pipeline) {
  parent <- pipeline_get_target(pipeline, target_get_parent(target))
  target_ensure_value(parent, pipeline)
  index <- target$index
  object <- value_produce_slice(parent$value, index)
  value_init(object, parent$settings$iteration)
}

#' @export
target_validate.tar_bud <- function(target) {
  tar_assert_correct_fields(target, bud_new, optional = "value")
  NextMethod()
  tar_assert_int(target$index)
  tar_assert_scalar(target$index)
  tar_assert_finite(target$index)
  tar_assert_ge(target$index, 1L)
}
