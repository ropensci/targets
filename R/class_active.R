active_new <- function(
  pipeline = NULL,
  meta = NULL,
  names = NULL,
  queue = NULL,
  reporter = NULL,
  envir = NULL
) {
  active_class$new(
    pipeline = pipeline,
    meta = meta,
    names = names,
    queue = queue,
    reporter = reporter,
    envir = envir
  )
}

active_class <- R6::R6Class(
  classname = "tar_active",
  inherit = algorithm_class,
  portable = FALSE,
  cloneable = FALSE,
  public = list(
    envir = NULL,
    process = NULL,
    initialize = function(
      pipeline = NULL,
      meta = NULL,
      names = NULL,
      queue = NULL,
      reporter = NULL,
      envir = NULL
    ) {
      super$initialize(
        pipeline = pipeline,
        meta = meta,
        names = names,
        queue = queue,
        reporter = reporter
      )
      self$envir <- envir
    },
    ensure_meta = function() {
      self$meta$validate()
      self$meta$database$preprocess(write = TRUE)
      self$write_gitignore()
      self$meta$record_imports(self$pipeline$imports, self$pipeline)
      self$meta$restrict_records(self$pipeline)
    },
    write_gitignore = function() {
      writeLines(
        c("*", "!.gitignore", "!meta"),
        path_gitignore(self$meta$get_path_store())
      )
    },
    ensure_process = function() {
      self$process <- process_init(path_store = self$meta$get_path_store())
      self$process$record_process()
    },
    produce_exports = function(envir, is_globalenv = NULL) {
      map(names(envir), ~force(envir[[.x]])) # try to nix high-mem promises
      if (is_globalenv %|||% identical(envir, globalenv())) {
        out <- as.list(envir, all.names = TRUE)
        out <- out[fltr(names(out), ~!is_internal_name(.x, envir))]
        out[[".tar_envir_5048826d"]] <- "globalenv"
      } else {
        discard <- fltr(names(envir), ~is_internal_name(.x, envir))
        remove(list = discard, envir = envir)
        out <- list(.tar_envir_5048826d = envir)
      }
      out[[".tar_options_5048826d"]] <- tar_options$export()
      out[[".tar_config_5048826d"]] <- tar_config$export()
      out
    },
    unload_transient = function() {
      pipeline_unload_transient(self$pipeline)
    },
    unserialize_target = function(target) {
      builder_unserialize_value(target)
    },
    skip_target = function(target) {
      target_skip(
        target,
        self$pipeline,
        self$scheduler,
        self$meta
      )
      target_sync_file_meta(target, self$meta)
    },
    process_target = function(name) {
      self$scheduler$backoff$reset()
      target <- pipeline_get_target(self$pipeline, name)
      target_debug(target)
      target_update_depend(target, self$pipeline, self$meta)
      if_any(
        target_should_run(target, self$meta),
        self$run_target(name),
        self$skip_target(target)
      )
      builder_ensure_workspace(target, self$pipeline, self$scheduler)
    },
    start = function() {
      pipeline_prune_names(self$pipeline, self$names)
      self$update_scheduler()
      self$ensure_meta()
      self$ensure_process()
      self$scheduler$progress$database$reset_storage()
      self$scheduler$reporter$report_start()
    },
    end = function() {
      pipeline_unload_loaded(self$pipeline)
      scheduler <- self$scheduler
      scheduler$reporter$report_end(scheduler$progress)
      path_scratch_del(path_store = self$meta$get_path_store())
      self$meta$database$deduplicate_storage()
    },
    validate = function() {
      super$validate()
      if (!is.null(self$process)) {
        self$process$validate()
      }
    }
  )
)
