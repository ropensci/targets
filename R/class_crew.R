crew_init <- function(
  pipeline = NULL,
  meta = meta_init(),
  names = NULL,
  shortcut = FALSE,
  queue = "parallel",
  reporter = "verbose",
  seconds_meta_append = 0,
  seconds_meta_upload = 15,
  seconds_reporter = 0,
  garbage_collection = FALSE,
  envir = tar_option_get("envir"),
  controller = NULL,
  terminate_controller = TRUE
) {
  crew_new(
    pipeline = pipeline,
    meta = meta,
    names = names,
    shortcut = shortcut,
    queue = queue,
    reporter = reporter,
    seconds_meta_append = seconds_meta_append,
    seconds_meta_upload = seconds_meta_upload,
    seconds_reporter = seconds_reporter,
    garbage_collection = garbage_collection,
    envir = envir,
    controller = controller,
    terminate_controller = terminate_controller
  )
}

crew_new <- function(
  pipeline = NULL,
  meta = NULL,
  names = NULL,
  shortcut = NULL,
  queue = NULL,
  reporter = NULL,
  seconds_meta_append = NULL,
  seconds_meta_upload = NULL,
  seconds_reporter = NULL,
  garbage_collection = NULL,
  envir = NULL,
  controller = NULL,
  terminate_controller = NULL
) {
  crew_class$new(
    pipeline = pipeline,
    meta = meta,
    names = names,
    shortcut = shortcut,
    queue = queue,
    reporter = reporter,
    seconds_meta_append = seconds_meta_append,
    seconds_meta_upload = seconds_meta_upload,
    seconds_reporter = seconds_reporter,
    garbage_collection = garbage_collection,
    envir = envir,
    controller = controller,
    terminate_controller = terminate_controller
  )
}

crew_class <- R6::R6Class(
  classname = "tar_crew",
  inherit = active_class,
  portable = FALSE,
  cloneable = FALSE,
  public = list(
    controller = NULL,
    terminate_controller = NULL,
    initialize = function(
      pipeline = NULL,
      meta = NULL,
      names = NULL,
      shortcut = NULL,
      queue = NULL,
      reporter = NULL,
      seconds_meta_append = NULL,
      seconds_meta_upload = NULL,
      seconds_reporter = NULL,
      garbage_collection = NULL,
      envir = NULL,
      controller = NULL,
      terminate_controller = NULL
    ) {
      super$initialize(
        pipeline = pipeline,
        meta = meta,
        names = names,
        shortcut = shortcut,
        queue = queue,
        reporter = reporter,
        seconds_meta_append = seconds_meta_append,
        seconds_meta_upload = seconds_meta_upload,
        seconds_reporter = seconds_reporter,
        garbage_collection = garbage_collection,
        envir = envir
      )
      self$controller <- controller
      self$terminate_controller <- terminate_controller
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
      common$fun <- tar_runtime$fun
      common$options <- tar_options$export()
      common$envvars <- tar_envvars()
      list(common = common, globals = globals)
    },
    run_worker = function(target) {
      name <- target_get_name(target)
      resources <- target$settings$resources$crew
      name_controller <- resources$controller
      # Covered in tests/hpc/test-crew_local.R
      # nocov start
      if (self$controller$saturated(controller = name_controller)) {
        self$controller$push_backlog(name = name, controller = name_controller)
        return()
      }
      # nocov end
      if (self$garbage_collection) {
        gc()
      }
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
      target_prepare(
        target = target,
        pipeline = self$pipeline,
        scheduler = self$scheduler,
        meta = self$meta,
        pending = FALSE
      )
      self$sync_meta_time()
      self$controller$push(
        command = command,
        data = data,
        globals = globals,
        substitute = FALSE,
        name = name,
        controller = name_controller,
        scale = TRUE,
        throttle = TRUE,
        seconds_timeout = resources$seconds_timeout
      )
    },
    run_main = function(target) {
      target_prepare(target, self$pipeline, self$scheduler, self$meta)
      self$sync_meta_time()
      target_run(
        target = target,
        envir = self$envir,
        path_store = self$meta$store
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
      self$sync_meta_time()
      queue <- self$scheduler$queue
      # Covered in tests/hpc/test-crew_local.R
      # nocov start
      if (queue$should_dequeue()) {
        self$process_target(queue$dequeue())
        self$conclude_worker_task()
      } else if (length(backlog <- self$controller$pop_backlog())) {
        map(
          x = backlog,
          f = ~{
            self$process_target(.x)
            self$conclude_worker_task()
          }
        )
      } else {
        self$controller$wait(
          mode = "one",
          seconds_interval = 0.5,
          seconds_timeout = 0.5,
          scale = TRUE,
          throttle = TRUE
        )
        self$conclude_worker_task()
      }
      # nocov end
    },
    conclude_worker_task = function() {
      result <- self$controller$pop(scale = TRUE, throttle = TRUE)
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
    },
    produce_prelocal = function() {
      prelocal_new(
        pipeline = self$pipeline,
        meta = self$meta,
        names = self$names,
        queue = self$queue,
        reporter = self$reporter,
        garbage_collection = self$garbage_collection,
        seconds_meta_append = self$seconds_meta_append,
        seconds_meta_upload = self$seconds_meta_upload,
        seconds_reporter = self$seconds_reporter,
        envir = self$envir,
        scheduler = self$scheduler
      )
    },
    nonempty = function() {
      self$scheduler$progress$any_remaining() ||
        (!self$controller$empty())
    },
    record_controller_summary = function(summary) {
      database <- database_crew(self$meta$store)
      database$overwrite_storage(summary)
      database$upload(verbose = FALSE)
    },
    finalize_crew = function() {
      summary <- crew_summary(self$controller)
      if (!is.null(summary)) {
        self$record_controller_summary(summary)
      }
      if (self$terminate_controller) {
        self$controller$terminate()
      }
    },
    run_crew = function() {
      self$controller$start()
      on.exit(self$finalize_crew())
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
      tar_assert_lgl(self$terminate_controller)
      tar_assert_scalar(self$terminate_controller)
      tar_assert_none_na(self$terminate_controller)
    }
  )
)

crew_summary <- function(controller) {
  summary <- controller$summary()
  data_frame(
    controller = summary$controller,
    worker = summary$worker,
    seconds = summary$seconds,
    targets = summary$tasks
  )
}

database_crew <- function(path_store) {
  database_init(
    path = file.path(path_meta_dir(path_store), "crew"),
    subkey = file.path(basename(path_meta("")), "crew"),
    header = c("controller", "worker", "seconds", "targets")
  )
}

validate_crew_controller <- function(controller) {
  tar_assert_envir(controller, msg = "invalid crew controller")
  tar_assert_function(controller$validate, msg = "invalid crew controller")
  controller$validate()
}
