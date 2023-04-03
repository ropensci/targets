crew_init <- function(
  pipeline = NULL,
  meta = meta_init(),
  names = NULL,
  shortcut = FALSE,
  queue = "parallel",
  reporter = "verbose",
  envir = tar_option_get("envir"),
  controller = NULL
) {
  crew_new(
    pipeline = pipeline,
    meta = meta,
    names = names,
    shortcut = shortcut,
    queue = queue,
    reporter = reporter,
    envir = envir,
    controller = controller
  )
}

crew_new <- function(
  pipeline = NULL,
  meta = NULL,
  names = NULL,
  shortcut = NULL,
  queue = NULL,
  reporter = NULL,
  envir = NULL,
  controller = NULL
) {
  crew_class$new(
    pipeline = pipeline,
    meta = meta,
    names = names,
    shortcut = shortcut,
    queue = queue,
    reporter = reporter,
    envir = envir,
    controller = controller
  )
}

crew_class <- R6::R6Class(
  classname = "tar_crew",
  inherit = active_class,
  portable = FALSE,
  cloneable = FALSE,
  public = list(
    controller = NULL,
    initialize = function(
    pipeline = NULL,
    meta = NULL,
    names = NULL,
    shortcut = NULL,
    queue = NULL,
    reporter = NULL,
    envir = NULL,
    exports = NULL,
    controller = NULL
    ) {
      super$initialize(
        pipeline = pipeline,
        meta = meta,
        names = names,
        shortcut = shortcut,
        queue = queue,
        reporter = reporter,
        envir = envir
      )
      self$controller = controller
    },
    update_exports = function() {
      self$exports <- self$produce_exports(
        envir = self$envir,
        path_store = self$meta$get_path_store()
      )
    },
    ensure_exports = function() {
      if (is.null(self$exports)) {
        self$update_exports()
      }
    },
    run_worker = function(target) {
      self$ensure_exports()
      expr <- quote(
        target_run_worker(
          target = target,
          envir = .tar_envir_5048826d,
          path_store = .tar_path_store_5048826d,
          fun = .tar_fun_5048826d,
          options = .tar_options_5048826d,
          envvars = .tar_envvars_5048826d
        )
      )
      
      
      browser()
      
      self$controller$push("...")
      
    },
    run_main = function(target) {
      target_run(
        target = target,
        envir = self$envir,
        path_store = self$meta$get_path_store()
      )
      target_conclude(
        target,
        self$pipeline,
        self$scheduler,
        self$meta
      )
    },
    run_target = function(name) {
      target <- pipeline_get_target(self$pipeline, name)
      target_gc(target)
      target_prepare(target, self$pipeline, self$scheduler)
      if_any(
        target_should_run_worker(target),
        self$run_worker(target),
        self$run_main(target)
      )
      self$unload_transient()
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
    next_target = function() {
      queue <- self$scheduler$queue
      if_any(
        queue$should_dequeue(),
        self$process_target(queue$dequeue()),
        self$backoff()
      )
    },
    conclude_worker_target = function(target) {
      if (is.null(target)) {
        return()
      }
      pipeline_set_target(self$pipeline, target)
      self$unmarshal_target(target)
      target_conclude(
        target,
        self$pipeline,
        self$scheduler,
        self$meta
      )
      self$scheduler$backoff$reset()
    },
    produce_prelocal = function() {
      prelocal_new(
        pipeline = self$pipeline,
        meta = self$meta,
        names = self$names,
        queue = self$queue,
        reporter = self$reporter,
        envir = self$envir,
        scheduler = self$scheduler
      )
    },
    run_crew = function() {
      suppressWarnings(crew::crew_session_start())
      self$controller$start()
      on.exit({
        suppressWarnings(crew::crew_session_terminate())
        self$controller$terminate()
      })
      while (self$scheduler$progress$any_remaining()) {
        self$next_target()
      }
    },
    run = function() {
      self$start()
      on.exit(self$end())
      tryCatch(
        self$produce_prelocal()$run(),
        tar_condition_prelocal = function(e) NULL
      )
      if (self$scheduler$queue$is_nonempty()) {
        self$run_crew()
      }
    },
    validate = function() {
      super$validate()
      tar_assert_inherits(
        x = self$controller,
        class = "crew_class_controller",
        msg = paste(
          "controller for tar_make_crew() must be a valid",
          "object of class \"crew_class_controller\" from the",
          "{crew} R package."
        )
      )
      self$controller$validate()
    }
  )
)
