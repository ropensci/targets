bud_init <- function(
  settings = settings_init(),
  child = character(0),
  index = integer(0)
) {
  parent <- settings$name
  command <- command_null
  settings <- settings_clone(settings)
  settings$name <- child
  bud_new(
    command = command,
    settings = settings,
    cue = NULL,
    value = NULL,
    pedigree = pedigree_new(parent, child, index)
  )
}

bud_new <- function(
  command = NULL,
  settings = NULL,
  cue = NULL,
  value = NULL,
  pedigree = NULL
) {
  force(command)
  force(settings)
  force(cue)
  force(value)
  force(pedigree)
  enclass(environment(), c("tar_bud", "tar_target"))
}

#' @export
target_get_parent.tar_bud <- function(target) {
  target$pedigree$parent
}

#' @export
target_read_value.tar_bud <- function(target, pipeline) {
  parent <- pipeline_get_target(pipeline, target_get_parent(target))
  target_ensure_value(parent, pipeline)
  index <- target$pedigree$index
  object <- value_produce_slice(parent$value, index)
  value_init(object, parent$settings$iteration)
}

#' @export
target_validate.tar_bud <- function(target) {
  tar_assert_correct_fields(target, bud_new)
  pedigree_validate(target$pedigree)
  NextMethod()
}
