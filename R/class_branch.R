branch_init <- function(
  name = NULL,
  command = NULL,
  deps_parent = character(0L),
  deps_child = character(0L),
  settings = NULL,
  cue = NULL
) {
  deps <- setdiff(unique(c(deps_parent, deps_child)), settings$dimensions)
  store <- settings_produce_store(settings)
  branch_new(
    name = name,
    command = command,
    seed = tar_seed_create(name),
    deps = deps,
    settings = settings,
    cue = cue,
    store = store
  )
}

branch_new <- function(
  name = NULL,
  command = NULL,
  seed = NULL,
  deps = NULL,
  settings = NULL,
  cue = NULL,
  store = NULL
) {
  out <- new.env(parent = emptyenv(), hash = FALSE)
  out$name <- name
  out$command <- command
  out$seed <- seed
  out$deps <- deps
  out$settings <- settings
  out$cue <- cue
  out$store <- store
  enclass(out, branch_s3_class)
}

branch_s3_class <- c("tar_branch", "tar_builder", "tar_target")

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
    seed = target$seed,
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
  tar_assert_correct_fields(
    target,
    branch_new,
    optional = c("value", "metrics", "subpipeline")
  )
  NextMethod()
  command_validate(target$command)
  tar_assert_dbl(target$seed)
  tar_assert_scalar(target$seed)
  tar_assert_none_na(target$seed)
  tar_assert_chr(target$deps)
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
