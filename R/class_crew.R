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
    exports = NULL,
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
      self$controller <- controller
    },
    produce_exports = function(envir, path_store, is_globalenv = NULL) {
      map(names(envir), ~force(envir[[.x]])) # try to nix high-mem promises
      common <- list()
      globals <- list()
      # Avoid the global environment in autometed tests.
      # Covered in semi-automated tests.
      # nocov start
      if (is_globalenv %|||% identical(envir, globalenv())) {
        globals <- as.list(envir, all.names = TRUE)
        which_globals <- fltr(names(globals), ~!is_internal_name(.x, envir))
        globals <- globals[which_globals]
        common$envir <- "globalenv"
      } else {
      # nocov end
        discard <- fltr(names(envir), ~is_internal_name(.x, envir))
        remove(list = discard, envir = envir)
        common$envir <- envir
      }
      common$path_store <- path_store
      common$fun <- tar_runtime$get_fun()
      common$options <- tar_options$export()
      common$envvars <- tar_envvars()
      list(common = common, globals = globals)
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
      command <- quote(
        targets::target_run_worker(
          target = target,
          envir = envir,
          path_store = path_store,
          fun = fun,
          options = options,
          envvars = envvars
        )
      )
      data <- self$exports$common
      data$target <- target
      globals <- self$exports$globals
      self$controller$push(
        command = command,
        data = data,
        globals = globals,
        substitute = FALSE,
        name = target_get_name(target)
      )
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
    iterate = function() {
      queue <- self$scheduler$queue
      should_dequeue <- queue$should_dequeue()
      if (should_dequeue) {
        self$process_target(queue$dequeue())
      }
      result <- self$controller$pop()
      self$conclude_worker_task(result)
      if (should_dequeue || (!is.null(result))) {
        self$backoff()
      }
    },
    conclude_worker_task = function(result) {
      if (is.null(result)) {
        return()
      }
      tar_assert_all_na(
        result$error,
        msg = paste("target", result$name, "error:", result$error)
      )
      target <- result$result[[1]]
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
    nonempty = function() {
      self$scheduler$progress$any_remaining() ||
        (!self$controller$empty())
    },
    run_crew = function() {
      self$controller$start()
      on.exit(self$controller$terminate())
      while (self$nonempty()) {
        self$iterate()
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
      validate_crew_controller(self$controller)
    }
  )
)

validate_crew_controller <- function(controller) {
  tar_assert_inherits(
    x = controller,
    class = "crew_class_controller",
    msg = paste(
      "controller for tar_make() must be a valid",
      "object of class \"crew_class_controller\" from the",
      "{crew} R package."
    )
  )
  controller$validate()
}
