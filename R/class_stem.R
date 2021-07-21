stem_new <- function(
  command = NULL,
  settings = NULL,
  cue = NULL,
  value = NULL,
  metrics = NULL,
  store = NULL,
  subpipeline = NULL,
  junction = NULL
) {
  force(command)
  force(settings)
  force(cue)
  force(value)
  force(metrics)
  force(store)
  force(subpipeline)
  force(junction)
  enclass(environment(), c("tar_stem", "tar_builder", "tar_target"))
}

#' @export
target_get_children.tar_stem <- function(target) {
  if_any(
    is.null(target$junction),
    character(0),
    target$junction$splits
  )
}

#' @export
target_get_type.tar_stem <- function(target) {
  "stem"
}

#' @export
target_get_type_cli.tar_stem <- function(target) {
  "target"
}

#' @export
target_produce_junction.tar_stem <- function(target, pipeline) {
  target_ensure_value(target, pipeline)
  stem_tar_assert_nonempty(target)
  hashes <- value_hash_slices(target$value)
  names <- paste0(target_get_parent(target), "_", hashes)
  junction_init(target_get_parent(target), names)
}

#' @export
target_produce_record.tar_stem <- function(target, pipeline, meta) {
  file <- target$store$file
  record_init(
    name = target_get_name(target),
    type = "stem",
    command = target$command$hash,
    seed = target$command$seed,
    depend = meta$get_depend(target_get_name(target)),
    path = file$path,
    data = file$hash,
    time = file$time %||nf% file_time_now(),
    size = file$size,
    bytes = file$bytes,
    format = target$settings$format,
    iteration = target$settings$iteration,
    children = target_get_children(target),
    seconds = target$metrics$seconds,
    warnings = target$metrics$warnings,
    error = target$metrics$error
  )
}

#' @export
target_skip.tar_stem <- function(target, pipeline, scheduler, meta, active) {
  NextMethod()
  stem_restore_buds(target, pipeline, scheduler, meta)
}

#' @export
target_ensure_buds.tar_stem <- function(target, pipeline, scheduler) {
  stem_ensure_buds(target, pipeline, scheduler)
}

#' @export
target_restore_buds.tar_stem <- function(target, pipeline, scheduler, meta) {
  stem_restore_buds(target, pipeline, scheduler, meta)
}

#' @export
target_is_branchable.tar_stem <- function(target) {
  !identical(target$settings$format, "file")
}

#' @export
target_validate.tar_stem <- function(target) {
  tar_assert_correct_fields(target, stem_new)
  NextMethod()
  if (!is.null(target$junction)) {
    junction_validate(target$junction)
  }
}

#' @export
target_bootstrap.tar_stem <- function(target, pipeline, meta) {
  NextMethod()
  stem_restore_junction(target, pipeline, meta)
  stem_insert_buds(target, pipeline)
  invisible()
}

stem_tar_assert_nonempty <- function(target) {
  if (value_count_slices(target$value) < 1L) {
    tar_throw_run(
      "cannot branch over empty target (",
      target_get_name(target),
      ")"
    )
  }
}

stem_produce_buds <- function(target) {
  settings <- target$settings
  names <- target_get_children(target)
  map(seq_along(names), ~bud_init(settings, names[.x], .x))
}

stem_insert_buds <- function(target, pipeline) {
  map(stem_produce_buds(target), pipeline_set_target, pipeline = pipeline)
}

stem_ensure_buds <- function(target, pipeline, scheduler) {
  if (length(target_downstream_branching(target, pipeline, scheduler))) {
    stem_ensure_junction(target, pipeline)
    stem_insert_buds(target, pipeline)
  }
}

stem_restore_buds <- function(target, pipeline, scheduler, meta) {
  if (length(target_downstream_branching(target, pipeline, scheduler))) {
    stem_restore_junction(target, pipeline, meta)
    stem_insert_buds(target, pipeline)
  }
}

stem_update_junction <- function(target, pipeline) {
  target$junction <- target_produce_junction(target, pipeline)
}

stem_ensure_junction <- function(target, pipeline) {
  if (is.null(target$junction)) {
    stem_update_junction(target, pipeline)
  }
}

stem_restore_junction <- function(target, pipeline, meta) {
  name <- target_get_name(target)
  if (!meta$exists_record(name)) {
    return()
  }
  children <- meta$get_record(name)$children
  junction <- if_any(
    anyNA(children),
    target_produce_junction(target, pipeline),
    junction_init(nexus = name, splits = children)
  )
  target$junction <- junction
}

#' @export
print.tar_stem <- function(x, ...) {
  cat(
    "<tar_stem>",
    "\n  name:", target_get_name(x),
    "\n  command:\n   ",
    produce_lines(string_sub_expression(x$command$string)),
    "\n  format:", x$settings$format,
    "\n  iteration method:", x$settings$iteration,
    "\n  error mode:", x$settings$error,
    "\n  memory mode:", x$settings$memory,
    "\n  storage mode:", x$settings$storage,
    "\n  retrieval mode:", x$settings$retrieval,
    "\n  deployment mode:", x$settings$deployment,
    "\n  priority:", x$settings$priority,
    "\n  resources:\n   ",
    produce_lines(paste_list(x$settings$resources)),
    "\n  cue:\n   ",
    produce_lines(paste_list(as.list(x$cue))),
    "\n  packages:\n   ", produce_lines(x$command$packages),
    "\n  library:\n   ", produce_lines(x$command$library)
  )
}
