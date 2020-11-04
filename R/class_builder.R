builder_new <- function(
  command = NULL,
  settings = NULL,
  cue = NULL,
  cache = NULL,
  value = NULL,
  metrics = NULL,
  store = NULL,
  subpipeline = NULL
) {
  force(command)
  force(settings)
  force(cue)
  force(cache)
  force(value)
  force(metrics)
  force(store)
  force(subpipeline)
  enclass(environment(), c("tar_builder", "tar_target"))
}

#' @export
target_update_depend.tar_builder <- function(target, meta) {
  depends <- meta$depends
  memory_set_object(
    depends,
    target_get_name(target),
    meta$produce_depend(target)
  )
}

#' @export
target_read_value.tar_builder <- function(target, pipeline = NULL) {
  object <- store_read_object(target$store)
  iteration <- target$settings$iteration
  value_init(object, iteration)
}

#' @export
target_prepare.tar_builder <- function(target, pipeline, scheduler) {
  scheduler$progress$register_running(target_get_name(target))
  scheduler$reporter$report_running(target, scheduler$progress)
  builder_ensure_deps(target, pipeline, "main")
  builder_ensure_subpipeline(target, pipeline)
  builder_serialize_subpipeline(target)
}

#' @export
target_should_run.tar_builder <- function(target, meta) {
  cue <- target$cue
  if (cue_record(cue, target, meta)) return(TRUE)
  if (cue_always(cue, target, meta)) return(TRUE)
  if (cue_never(cue, target, meta)) return(FALSE)
  if (cue_command(cue, target, meta)) return(TRUE)
  if (cue_depend(cue, target, meta)) return(TRUE)
  if (cue_format(cue, target, meta)) return(TRUE)
  if (cue_iteration(cue, target, meta)) return(TRUE)
  if (cue_file(cue, target, meta)) return(TRUE)
  FALSE
}

#' @export
target_should_run_worker.tar_builder <- function(target) {
  identical(target$settings$deployment, "worker")
}

#' @export
target_run.tar_builder <- function(target) {
  on.exit(builder_unset_tar_envir_run())
  builder_set_tar_envir_run(target)
  builder_unserialize_subpipeline(target)
  builder_ensure_deps(target, target$subpipeline, "worker")
  target_cache_deps(target, target$subpipeline)
  builder_update_build(target)
  cache_clear_objects(target$cache)
  target$subpipeline <- NULL
  builder_update_paths(target)
  builder_ensure_object(target, "worker")
  target
}

#' @export
target_run_worker.tar_builder <- function(target) {
  target_gc(target)
  target_run(target)
  builder_serialize_value(target)
  target
}

#' @export
target_skip.tar_builder <- function(target, pipeline, scheduler, meta) {
  target_update_queue(target, scheduler)
  path <- meta$get_record(target_get_name(target))$path
  file <- target$store$file
  file$path <- path
  scheduler$progress$assign_skipped(target_get_name(target))
  scheduler$reporter$report_skipped(target, scheduler$progress)
}

#' @export
target_conclude.tar_builder <- function(target, pipeline, scheduler, meta) {
  target_update_queue(target, scheduler)
  builder_handle_warnings(target, scheduler)
  switch(
    metrics_outcome(target$metrics),
    cancel = builder_cancel(target, pipeline, scheduler, meta),
    error = builder_error(target, pipeline, scheduler, meta),
    built = builder_conclude(target, pipeline, scheduler, meta)
  )
  NextMethod()
}

builder_conclude <- function(target, pipeline, scheduler, meta) {
  builder_ensure_object(target, "main")
  builder_wait_correct_hash(target)
  target_ensure_buds(target, pipeline, scheduler)
  meta$insert_record(target_produce_record(target, meta))
  target_patternview_meta(target, pipeline, meta)
  pipeline_register_loaded(pipeline, target_get_name(target))
  scheduler$progress$register_built(target_get_name(target))
}

builder_error <- function(target, pipeline, scheduler, meta) {
  target_restore_buds(target, pipeline, scheduler, meta)
  builder_record_error_meta(target, meta)
  target_patternview_meta(target, pipeline, meta)
  builder_handle_error(target, pipeline, scheduler, meta)
}

builder_cancel <- function(target, pipeline, scheduler, meta) {
  target_restore_buds(target, pipeline, scheduler, meta)
  scheduler$progress$register_cancelled(target_get_name(target))
  scheduler$reporter$report_cancelled(target, scheduler$progress)
  target_patternview_cancelled(target, pipeline, scheduler)
}

#' @export
target_debug.tar_builder <- function(target) {
  debug <- tar_option_get("debug")
  if (length(debug) && target_get_name(target) %in% debug) {
    # Covered in tests/interactive/test-debug.R
    # nocov start
    target$command$expr <- c(expression(browser()), target$command$expr)
    target$cue$mode <- "always"
    target$settings$deployment <- "main"
    # nocov end
  }
}

#' @export
target_sync_file_meta.tar_builder <- function(target, meta) {
  store_sync_file_meta(target$store, target, meta)
}

#' @export
target_validate.tar_builder <- function(target) {
  NextMethod()
  if (!is.null(target$store)) {
    store_validate(target$store)
  }
  if (!is.null(target$metrics)) {
    metrics_validate(target$metrics)
  }
}

builder_ensure_deps <- function(target, pipeline, retrieval) {
  if (identical(target$settings$retrieval, retrieval)) {
    target_ensure_deps(target, pipeline)
  }
}

builder_update_subpipeline <- function(target, pipeline) {
  target$subpipeline <- pipeline_produce_subpipeline(
    pipeline,
    target_get_name(target)
  )
}

builder_serialize_subpipeline <- function(target) {
  subpipeline <- target$subpipeline
  retrieval <- target$settings$retrieval
  if (!is.null(subpipeline) && identical(retrieval, "main")) {
    pipeline_serialize_values(subpipeline)
  }
}

builder_unserialize_subpipeline <- function(target) {
  subpipeline <- target$subpipeline
  retrieval <- target$settings$retrieval
  if (!is.null(subpipeline) && identical(retrieval, "main")) {
    pipeline_unserialize_values(target$subpipeline)
  }
}

builder_ensure_subpipeline <- function(target, pipeline) {
  if (identical(target$settings$deployment, "worker")) {
    builder_update_subpipeline(target, pipeline)
  }
}

builder_handle_warnings <- function(target, scheduler) {
  if (metrics_has_warnings(target$metrics)) {
    scheduler$progress$assign_warned(target_get_name(target))
  }
}

builder_handle_error <- function(target, pipeline, scheduler, meta) {
  scheduler$progress$register_errored(target_get_name(target))
  scheduler$reporter$report_errored(target, scheduler$progress)
  target_patternview_errored(target, pipeline, scheduler)
  if (identical(target$settings$error, "save")) {
    builder_save_workspace(target, pipeline, scheduler)
  }
  if (!identical(target$settings$error, "continue")) {
    throw_run(target$metrics$error)
  }
}

builder_save_workspace <- function(target, pipeline, scheduler) {
  scheduler$reporter$report_workspace(target)
  out <- as.list(target$cache$imports$envir)
  target_ensure_deps(target, pipeline)
  target_cache_deps(target, pipeline)
  for (name in target$cache$targets$names) {
    out[[name]] <- memory_get_object(target$cache$targets, name)
  }
  out$.targets <- list(
    packages = target$command$packages,
    library = target$command$library,
    seed = target$command$seed,
    traceback = target$metrics$traceback
  )
  dir_create(path_workspaces_dir())
  path <- path_workspaces(target$settings$name)
  qs::qsave(x = out, file = path, preset = "high")
}

builder_record_error_meta <- function(target, meta) {
  record <- target_produce_record(target, meta)
  meta$handle_error(record)
  meta$insert_record(record)
}

builder_update_build <- function(target) {
  command_load_packages(target$command)
  envir <- cache_get_envir(target$cache)
  build <- command_produce_build(target$command, envir)
  cache_clear_objects(target$cache)
  object <- store_coerce_object(target$store, build$object)
  target$value <- value_init(object, target$settings$iteration)
  target$metrics <- build$metrics
  invisible()
}

builder_update_paths <- function(target) {
  if (metrics_terminated_early(target$metrics)) {
    return()
  }
  name <- target_get_name(target)
  store_update_path(target$store, name, target$value$object)
  store_update_stage(target$store, name, target$value$object)
  store_early_hash(target$store)
}

builder_unload_value <- function(target) {
  settings <- target$settings
  clear <- identical(settings$deployment, "worker") &&
    identical(settings$storage, "worker")
  if (clear) {
    target$value <- NULL
  }
}

builder_update_object <- function(target) {
  if (metrics_terminated_early(target$metrics)) {
    return()
  }
  file_validate_path(target$store$file$path)
  store_write_object(target$store, target$value$object)
  builder_unload_value(target)
  store_late_hash(target$store)
  store_upload_object(target$store)
}

builder_ensure_object <- function(target, storage) {
  if (identical(target$settings$storage, storage)) {
    builder_update_object(target)
  }
}

builder_wait_correct_hash <- function(target) {
  storage <- target$settings$storage
  deployment <- target$settings$deployment
  store_ensure_correct_hash(target$store, storage, deployment)
}

builder_set_tar_envir_run <- function(target) {
  assign(x = "name", value = target_get_name(target), envir = tar_envir_run)
  assign(x = "seed", value = target$command$seed, envir = tar_envir_run)
}

builder_unset_tar_envir_run <- function() {
  names <- c("name", "seed")
  remove(list = names, envir = tar_envir_run, inherits = FALSE)
}

builder_serialize_value <- function(target) {
  if (identical(target$settings$storage, "main")) {
    target_serialize_value(target)
  }
}

builder_unserialize_value <- function(target) {
  if (identical(target$settings$storage, "main")) {
    target_unserialize_value(target)
  }
}

builder_sitrep <- function(target, meta) {
  cue <- target$cue
  record <- cue_record(cue, target, meta)
  list(
    name = target_get_name(target),
    record = cue_record(cue, target, meta),
    always = cue_always(cue, target, meta),
    never = cue_never(cue, target, meta),
    command = trn(record, NA, cue_command(cue, target, meta)),
    depend = trn(record, NA, cue_depend(cue, target, meta)),
    format = trn(record, NA, cue_format(cue, target, meta)),
    iteration = trn(record, NA, cue_iteration(cue, target, meta)),
    file = trn(record, NA, cue_file(cue, target, meta))
  )
}

tar_envir_run <- new.env(parent = emptyenv())
