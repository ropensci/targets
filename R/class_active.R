active_new <- function(
  pipeline = NULL,
  meta = NULL,
  names = NULL,
  shortcut = NULL,
  queue = NULL,
  reporter = NULL,
  envir = NULL
) {
  active_class$new(
    pipeline = pipeline,
    meta = meta,
    names = names,
    shortcut = shortcut,
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
    seconds_start = NULL,
    initialize = function(
      pipeline = NULL,
      meta = NULL,
      names = NULL,
      shortcut = NULL,
      queue = NULL,
      reporter = NULL,
      envir = NULL
    ) {
      super$initialize(
        pipeline = pipeline,
        meta = meta,
        names = names,
        shortcut = shortcut,
        queue = queue,
        reporter = reporter
      )
      self$envir <- envir
    },
    ensure_meta = function() {
      new_store <- !file.exists(self$meta$get_path_store())
      self$meta$migrate_database()
      self$meta$validate()
      self$meta$database$preprocess(write = TRUE)
      if (new_store) {
        self$write_gitignore()
        self$write_user()
      }
      self$meta$record_imports(self$pipeline$imports, self$pipeline)
      self$meta$restrict_records(self$pipeline)
    },
    write_gitignore = function() {
      writeLines(
        c("*", "!.gitignore", "!meta", "meta/*", "!meta/meta"),
        path_gitignore(self$meta$get_path_store())
      )
    },
    write_user = function() {
      dir_create(path_user_dir(self$meta$get_path_store()))
    },
    ensure_process = function() {
      self$process <- process_init(path_store = self$meta$get_path_store())
      self$process$record_process()
    },
    produce_exports = function(envir, path_store, is_globalenv = NULL) {
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
      out[[".tar_path_store_5048826d"]] <- path_store
      out[[".tar_fun_5048826d"]] <- tar_runtime$get_fun()
      out[[".tar_options_5048826d"]] <- tar_options$export()
      out[[".tar_envvars_5048826d"]] <- tar_envvars()
      out
    },
    unload_transient = function() {
      pipeline_unload_transient(self$pipeline)
    },
    unmarshal_target = function(target) {
      builder_unmarshal_value(target)
    },
    skip_target = function(target) {
      target_skip(
        target = target,
        pipeline = self$pipeline,
        scheduler = self$scheduler,
        meta = self$meta,
        active = TRUE
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
    },
    start = function() {
      self$seconds_start <- unname(proc.time()["elapsed"])
      pipeline_prune_names(self$pipeline, self$names)
      self$ensure_meta()
      self$update_scheduler()
      self$bootstrap_shortcut_deps()
      self$ensure_process()
      self$scheduler$progress$database$reset_storage()
      self$scheduler$reporter$report_start()
    },
    end = function() {
      scheduler <- self$scheduler
      scheduler$progress$database$dequeue_rows()
      self$meta$database$dequeue_rows()
      pipeline_unload_loaded(self$pipeline)
      seconds_elapsed <- unname(proc.time()["elapsed"]) - self$seconds_start
      scheduler$reporter$report_end(scheduler$progress, seconds_elapsed)
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
