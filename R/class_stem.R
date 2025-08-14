stem_init <- function(
  name = NULL,
  command = NULL,
  seed = NULL,
  deps = NULL,
  settings = NULL,
  cue = NULL
) {
  stem_new(
    name = name,
    command = command,
    seed = seed,
    deps = deps,
    settings = settings,
    cue = cue,
    store = settings_produce_store(settings),
    file = file_init()
  )
}

stem_new <- function(
  name = NULL,
  command = NULL,
  seed = NULL,
  deps = NULL,
  settings = NULL,
  cue = NULL,
  store = NULL,
  file = NULL
) {
  out <- new.env(parent = emptyenv(), hash = FALSE)
  out$name <- name
  out$command <- command
  out$seed <- seed
  out$deps <- deps
  out$settings <- settings
  out$cue <- cue
  out$store <- store
  out$file <- file
  enclass(out, stem_s3_class)
}

stem_s3_class <- c("tar_stem", "tar_builder", "tar_target")

#' @export
target_get_children.tar_stem <- function(target) {
  if_any(
    is.null(target$junction),
    character(0),
    junction_splits(target$junction)
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
  file <- target$file
  record_init(
    name = target_get_name(target),
    type = "stem",
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
    children = target_get_children(target),
    seconds = target$metrics$seconds,
    warnings = target$metrics$warnings,
    error = target$metrics$error
  )
}

#' @export
target_skip.tar_stem <- function(
  target,
  pipeline,
  scheduler,
  meta,
  active
) {
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
  tar_assert_correct_fields(
    target,
    stem_new,
    optional = c("junction", "metrics", "subpipeline", "value")
  )
  NextMethod()
  command_validate(target$command)
  tar_assert_dbl(target$seed)
  tar_assert_scalar(target$seed)
  tar_assert_none_na(target$seed)
  tar_assert_chr(target$deps)
  store_validate(target$store)
  file_validate(target$file)
  if (!is.null(target$junction)) {
    junction_validate(target$junction)
  }
}

#' @export
target_bootstrap.tar_stem <- function(
  target,
  pipeline,
  meta,
  branched_over
) {
  NextMethod()
  if (branched_over) {
    stem_restore_junction(target, pipeline, meta)
    stem_insert_buds(target, pipeline)
  }
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

stem_produce_bud <- function(target, name, index) {
  junction <- .subset2(target, "junction")
  bud_new(name = name, settings = .subset2(target, "settings"), index = index)
}

stem_insert_buds <- function(target, pipeline) {
  pipeline_initialize_references_children(
    pipeline = pipeline,
    name_parent = target_get_name(target),
    names_children = junction_splits(target$junction),
    type = "bud"
  )
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
target_produce_child.tar_stem <- function(target, name, index) {
  stem_produce_bud(target, name, index)
}

#' @export
print.tar_stem <- function(x, ...) {
  cat(
    "<tar_stem>",
    "\n  name:",
    target_get_name(x),
    "\n  description:",
    x$settings$description,
    "\n  command:\n   ",
    produce_lines(string_sub_expression(x$command$string)),
    "\n  format:",
    x$settings$format,
    "\n  repository:",
    x$settings$repository,
    "\n  iteration method:",
    x$settings$iteration,
    "\n  error mode:",
    x$settings$error,
    "\n  memory mode:",
    x$settings$memory,
    "\n  storage mode:",
    x$settings$storage,
    "\n  retrieval mode:",
    x$settings$retrieval,
    "\n  deployment mode:",
    x$settings$deployment,
    "\n  priority:",
    x$settings$priority,
    "\n  resources:\n   ",
    produce_lines(paste_list(x$settings$resources)),
    "\n  cue:\n   ",
    produce_lines(paste_list(as.list(x$cue))),
    "\n  packages:\n   ",
    produce_lines(x$command$packages),
    "\n  library:\n   ",
    produce_lines(x$command$library)
  )
}
