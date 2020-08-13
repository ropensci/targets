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
target_record_meta.tar_builder <- function(target, meta) {
  record <- target_produce_record(target, meta)
  meta$handle_error(record)
  meta$insert_record(record)
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
  builder_ensure_deps(target, pipeline, "local")
  builder_ensure_subpipeline(target, pipeline)
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
target_should_run_remote.tar_builder <- function(target) {
  target$settings$deployment == "remote"
}

#' @export
target_run.tar_builder <- function(target) {
  on.exit(builder_unset_envir_run())
  builder_set_envir_run(target)
  builder_ensure_deps(target, target$subpipeline, "remote")
  builder_update_build(target)
  target$subpipeline <- NULL
  builder_update_path(target)
  builder_ensure_object(target, "remote")
  target
}

#' @export
target_run_remote.tar_builder <- function(target, garbage_collection) {
  run_gc(garbage_collection)
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
  builder_handle_warnings(target, scheduler)
  if (metrics_has_cancel(target$metrics)) {
    return(builder_cancel(target, pipeline, scheduler))
  }
  builder_ensure_object(target, "local")
  target_record_meta(target, meta)
  builder_patternview_meta(target, pipeline, meta)
  builder_handle_error(target, pipeline, scheduler, meta)
  builder_ensure_restored(target, pipeline)
  pipeline_register_loaded(pipeline, target_get_name(target))
  if (!metrics_terminated_early(target$metrics)) {
    scheduler$progress$register_built(target_get_name(target))
  }
  builder_wait_correct_hash(target)
  NextMethod()
}

#' @export
target_debug.tar_builder <- function(target) {
  debug <- tar_option_get("debug")
  if (length(debug) && target_get_name(target) %in% debug) {
    # Covered in tests/interactive/test-debug.R
    # nocov start
    target$command$expr <- c(expression(browser()), target$command$expr)
    target$cue$mode <- "always"
    target$settings$deployment <- "local"
    # nocov end
  }
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

builder_patternview_meta <- function(target, pipeline, meta) {
  UseMethod("builder_patternview_meta")
}

#' @export
builder_patternview_meta.default <- function(target, pipeline, meta) {
}

builder_patternview_cancelled <- function(target, pipeline, scheduler) {
  UseMethod("builder_patternview_cancelled")
}

#' @export
builder_patternview_cancelled.default <- function(target, pipeline, scheduler) {
}

builder_patternview_errored <- function(target, pipeline, scheduler) {
  UseMethod("builder_patternview_errored")
}

#' @export
builder_patternview_errored.default <- function(target, pipeline, scheduler) {
}

builder_ensure_deps <- function(target, pipeline, retrieval) {
  if (target$settings$retrieval == retrieval) {
    target_load_deps(target, pipeline)
  }
}

builder_update_subpipeline <- function(target, pipeline) {
  subpipeline <- pipeline_produce_subpipeline(
    pipeline,
    target_get_name(target)
  )
  pipeline_stash_targets(pipeline, subpipeline)
  target$subpipeline <- subpipeline
}

builder_ensure_subpipeline <- function(target, pipeline) {
  if (target$settings$retrieval == "remote") {
    builder_update_subpipeline(target, pipeline)
  }
}

builder_handle_warnings <- function(target, scheduler) {
  if (metrics_has_warnings(target$metrics)) {
    scheduler$progress$assign_warned(target_get_name(target))
  }
}

builder_handle_error <- function(target, pipeline, scheduler, meta) {
  if (!metrics_has_error(target$metrics)) {
    return()
  }
  scheduler$progress$register_errored(target_get_name(target))
  scheduler$reporter$report_errored(target, scheduler$progress)
  builder_patternview_errored(target, pipeline, scheduler)
  assign("traceback", target$metrics$traceback, envir = envir_run)
  if (target$settings$error != "continue") {
    throw_run(target$metrics$error)
  }
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

builder_update_path <- function(target) {
  if (metrics_terminated_early(target$metrics)) {
    return()
  }
  settings <- target$settings
  name <- settings$name
  object <- target$value$object
  store_update_path(target$store, name, object)
  store_early_hash(target$store)
}

builder_update_object <- function(target) {
  if (metrics_terminated_early(target$metrics)) {
    return()
  }
  file_validate_path(target$store$file$path)
  store_write_object(target$store, target$value$object)
  warn_output(target_get_name(target), target$store$file$path)
  store_late_hash(target$store)
}

builder_ensure_object <- function(target, storage) {
  if (target$settings$storage == storage) {
    builder_update_object(target)
  }
}

builder_restore_targets <- function(target, pipeline) {
  pipeline_restore_targets(pipeline)
  target$subpipeline <- NULL
}

builder_ensure_restored <- function(target, pipeline) {
  if (target$settings$retrieval == "remote") {
    builder_restore_targets(target, pipeline)
  }
}

builder_wait_correct_hash <- function(target) {
  remote <- target$settings$storage == "remote"
  store_wait_correct_hash(target$store, remote)
}

builder_set_envir_run <- function(target) {
  assign(x = "name", value = target_get_name(target), envir = envir_run)
}

builder_unset_envir_run <- function() {
  remove(list = "name", envir = envir_run, inherits = FALSE)
}

builder_cancel <- function(target, pipeline, scheduler) {
  scheduler$progress$register_cancelled(target_get_name(target))
  scheduler$reporter$report_cancelled(target, scheduler$progress)
  builder_patternview_cancelled(target, pipeline, scheduler)
}

builder_serialize_value <- function(target) {
  if (target$settings$storage == "local") {
    store_serialize_value(target$store, target$value)
  }
}

builder_unserialize_value <- function(target) {
  if (target$settings$storage == "local") {
    store_unserialize_value(target$store, target$value)
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
