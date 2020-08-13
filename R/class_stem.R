stem_new <- function(
  command = NULL,
  settings = NULL,
  cue = NULL,
  cache = NULL,
  value = NULL,
  metrics = NULL,
  store = NULL,
  subpipeline = NULL,
  junction = NULL
) {
  force(command)
  force(settings)
  force(cue)
  force(cache)
  force(value)
  force(metrics)
  force(store)
  force(subpipeline)
  force(junction)
  enclass(environment(), c("tar_stem", "tar_builder", "tar_target"))
}

#' @export
target_get_parent.tar_stem <- function(target) {
  target_get_name(target)
}

#' @export
target_get_children.tar_stem <- function(target) {
  trn(
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
target_produce_junction.tar_stem <- function(target, pipeline) {
  target_ensure_value(target, pipeline)
  stem_assert_nonempty(target)
  hashes <- value_hash_slices(target$value)
  names <- paste0(target_get_parent(target), "_", hashes)
  junction_init(target_get_parent(target), names)
}

#' @export
target_produce_record.tar_stem <- function(target, meta) {
  file <- target$store$file
  record_init(
    name = target_get_name(target),
    type = "stem",
    command = target$command$hash,
    seed = target$command$seed,
    depend = meta$get_depend(target_get_name(target)),
    path = file$path,
    data = file$hash,
    bytes = file$bytes,
    time = file$time,
    format = target$settings$format,
    iteration = target$settings$iteration,
    children = as.character(target_get_children(target)),
    seconds = as.numeric(target$metrics$seconds),
    warnings = as.character(target$metrics$warnings),
    error = as.character(target$metrics$error)
  )
}

#' @export
target_skip.tar_stem <- function(target, pipeline, scheduler, meta) {
  NextMethod()
  stem_restore_buds(target, pipeline, scheduler, meta)
}

#' @export
target_ensure_buds.tar_stem <- function(target, pipeline, scheduler) {
  if (!metrics_terminated_early(target$metrics)) {
    stem_ensure_buds(target, pipeline, scheduler)
  }
}

#' @export
target_is_branchable.tar_stem <- function(target) {
  target$settings$format != "file"
}

#' @export
target_validate.tar_stem <- function(target) {
  assert_correct_fields(target, stem_new)
  NextMethod()
  if (!is.null(target$junction)) {
    junction_validate(target$junction)
  }
}

stem_assert_nonempty <- function(target) {
  if (value_count_slices(target$value) < 1L) {
    throw_pattern(
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

stem_insert_buds <- function(target, pipeline, scheduler) {
  map(stem_produce_buds(target), pipeline_set_target, pipeline = pipeline)
}

stem_ensure_buds <- function(target, pipeline, scheduler) {
  if (length(target_downstream_branching(target, pipeline, scheduler))) {
    stem_ensure_junction(target, pipeline)
    stem_insert_buds(target, pipeline, scheduler)
  }
}

stem_restore_buds <- function(target, pipeline, scheduler, meta) {
  if (length(target_downstream_branching(target, pipeline, scheduler))) {
    stem_restore_junction(target, pipeline, meta)
    stem_insert_buds(target, pipeline, scheduler)
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
  children <- meta$get_record(target_get_name(target))$children
  junction <- trn(
    anyNA(children),
    target_produce_junction(target, pipeline),
    junction_init(nexus = target_get_name(target), splits = children)
  )
  target$junction <- junction
}

#' @export
print.tar_stem <- function(x, ...) {
  cat(
    "<stem target>",
    "\n  name:", target_get_name(x),
    "\n  command:\n   ",
    produce_lines(string_sub_expression(x$command$string)),
    "\n  format:", x$settings$format,
    "\n  iteration method:", x$settings$iteration,
    "\n  error mode:", x$settings$error,
    "\n  memory mode:", x$settings$memory,
    "\n  storage mode:", x$settings$storage,
    "\n  retrieval mode:", x$settings$retrieval,
    "\n  deploy to:", x$settings$deployment,
    "\n  template (clustermq):\n   ",
    produce_lines(paste_list(x$settings$template)),
    "\n  resources (future):\n   ",
    produce_lines(paste_list(x$settings$resources)),
    "\n  cue:\n   ",
    produce_lines(paste_list(as.list(x$cue))),
    "\n  packages:\n   ", produce_lines(x$command$packages),
    "\n  library:\n   ", produce_lines(x$command$library)
  )
}
