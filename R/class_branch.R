branch_init <- function(
  command = NULL,
  settings = NULL,
  cue = NULL,
  value = NULL,
  deps = character(0),
  child = character(0),
  index = integer(0)
) {
  command <- command_clone(command)
  deps <- union(command$deps, deps)
  command$deps <- setdiff(deps, settings$dimensions)
  command$seed <- tar_seed_create(child)
  pedigree <- pedigree_new(settings$name, child, index)
  settings <- settings_clone(settings)
  settings$name <- child
  store <- settings_produce_store(settings)
  branch_new(
    command = command,
    settings = settings,
    cue = cue,
    value = value,
    metrics = NULL,
    store = store,
    subpipeline = NULL,
    pedigree = pedigree
  )
}

branch_new <- function(
  command = NULL,
  settings = NULL,
  cue = NULL,
  value = NULL,
  metrics = NULL,
  store = NULL,
  subpipeline = NULL,
  pedigree = NULL
) {
  force(command)
  force(settings)
  force(cue)
  force(value)
  force(metrics)
  force(store)
  force(subpipeline)
  force(pedigree)
  enclass(environment(), c("tar_branch", "tar_builder", "tar_target"))
}

#' @export
target_get_parent.tar_branch <- function(target) {
  target$pedigree$parent
}

#' @export
target_get_type.tar_branch <- function(target) {
  "branch"
}

#' @export
target_produce_record.tar_branch <- function(target, pipeline, meta) {
  file <- target$store$file
  record_init(
    name = target_get_name(target),
    parent = target_get_parent(target),
    type = "branch",
    command = target$command$hash,
    seed = target$command$seed,
    depend = meta$get_depend(target_get_name(target)),
    path = file$path,
    data = file$hash,
    time = file$time %||nf% file_time_now(),
    size = file$size,
    bytes = file$bytes,
    format = target$settings$format,
    repository = target$settings$repository,
    iteration = target$settings$iteration,
    seconds = target$metrics$seconds,
    warnings = target$metrics$warnings,
    error = target$metrics$error
  )
}

#' @export
target_ensure_buds.tar_branch <- function(target, pipeline, scheduler) {
}

#' @export
target_restore_buds.tar_branch <- function(target, pipeline, scheduler, meta) {
}

#' @export
target_validate.tar_branch <- function(target) {
  tar_assert_correct_fields(target, branch_new)
  pedigree_validate(target$pedigree)
  NextMethod()
}

#' @export
target_patternview_meta.tar_branch <- function(target, pipeline, meta) {
  name <- target_get_parent(target)
  parent <- pipeline_get_target(pipeline, name)
  record <- meta$get_record(target_get_name(target))
  patternview_register_meta(parent$patternview, record)
}

#' @export
target_patternview_dispatched.tar_branch <- function(
  target,
  pipeline,
  scheduler
) {
  parent <- pipeline_get_target(pipeline, target_get_parent(target))
  patternview_register_dispatched(parent$patternview, parent, scheduler)
}

#' @export
target_patternview_canceled.tar_branch <- function(
  target,
  pipeline,
  scheduler
) {
  parent <- pipeline_get_target(pipeline, target_get_parent(target))
  patternview_register_canceled(parent$patternview, parent, scheduler)
}

#' @export
target_patternview_errored.tar_branch <- function(
  target,
  pipeline,
  scheduler
) {
  parent <- pipeline_get_target(pipeline, target_get_parent(target))
  patternview_register_errored(parent$patternview, parent, scheduler)
}
