future_init <- function(
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
  workers = 1L
) {
  future_new(
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
    workers = as.integer(workers)
  )
}

future_new <- function(
  pipeline = NULL,
  meta = NULL,
  names = NULL,
  shortcut = NULL,
  queue = NULL,
  reporter = NULL,
  envir = NULL,
  seconds_meta_append = NULL,
  seconds_meta_upload = NULL,
  seconds_reporter = NULL,
  garbage_collection = NULL,
  workers = NULL
) {
  future_class$new(
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
    workers = workers
  )
}

future_class <- R6::R6Class(
  classname = "tar_future",
  inherit = active_class,
  portable = FALSE,
  cloneable = FALSE,
  public = list(
    workers = NULL,
    worker_list = NULL,
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
      workers = NULL
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
      self$workers <- workers
      self$worker_list <- memory_init()
    },
    run_worker = function(target) {
      builder_marshal_subpipeline(target)
      if (self$garbage_collection) {
        gc()
      }
      self$ensure_exports()
      globals <- self$exports
      globals$.tar_target_5048826d <- target
      plan_new <- target$settings$resources$future$plan %|||%
        target$settings$resources$plan
      if (!is.null(plan_new)) {
        # Temporary solution to allow heterogeneous workers
        # from different plans. Uses .cleanup = FALSE # nolint
        # to avoid destroying psock clusters.
        # .cleanup may not be supported long-term # nolint
        # so we only attempt to change plans if the user
        # sets the plan in the resources argument/option.
        plan_old <- future::plan()
        on.exit(future::plan(plan_old, .cleanup = FALSE))
        future::plan(plan_new, .cleanup = FALSE)
      }
      # TODO: default to resources from the future plan
      # after unstructured resources are totally defunct.
      resources <- target$settings$resources$future$resources %|||%
        target$settings$resources # compat: deprecated unstructured resources
      args <- list(
        expr = quote(
          targets::target_run_worker(
            target = .tar_target_5048826d,
            envir = .tar_envir_5048826d,
            path_store = .tar_path_store_5048826d,
            fun = .tar_fun_5048826d,
            options = .tar_options_5048826d,
            envvars = .tar_envvars_5048826d
          )
        ),
        substitute = TRUE,
        packages = "targets",
        globals = globals,
        label = target_get_name(target),
        resources = resources,
        lazy = FALSE,
        seed = FALSE
      )
      future <- do.call(what = future::future, args = args)
      memory_set_object(
        self$worker_list,
        name = target_get_name(target),
        object = future
      )
    },
    run_main = function(target) {
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
      target_prepare(target, self$pipeline, self$scheduler, self$meta)
      self$sync_meta_time()
      if_any(
        target_should_run_worker(target),
        self$run_worker(target),
        self$run_main(target)
      )
      self$unload_transient()
    },
    next_target = function() {
      queue <- self$scheduler$queue
      if (queue$should_dequeue()) {
        self$process_target(queue$dequeue())
      }
    },
    conclude_worker_target = function(value, name) {
      target <- future_value_target(value, name, self$pipeline)
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
    can_submit = function() {
      self$worker_list$count < self$workers &&
        self$scheduler$queue$is_nonempty()
    },
    try_submit = function(wait) {
      if (self$can_submit()) {
        self$next_target()
      } else if (wait) {
        self$backoff()
      }
    },
    future_value = function(worker) {
      tryCatch(future::value(worker, signal = FALSE), error = identity)
    },
    process_worker = function(name) {
      worker <- memory_get_object(self$worker_list, name)
      if (future::resolved(worker)) {
        value <- self$future_value(worker)
        self$conclude_worker_target(value, name)
        memory_del_objects(self$worker_list, name)
      }
      self$try_submit(wait = FALSE)
    },
    process_workers = function() {
      names <- self$worker_list$names
      if (!length(names)) {
        return()
      }
      targets <- map(names, ~pipeline_get_target(self$pipeline, .x))
      priorities <- map_dbl(targets, ~.x$settings$priority)
      names(priorities) <- names
      names <- names(sort(priorities, decreasing = TRUE))
      lapply(names, self$process_worker)
    },
    iterate = function() {
      self$sync_meta_time()
      self$process_workers()
      self$try_submit(wait = TRUE)
    },
    run = function() {
      self$start()
      on.exit(self$end())
      while (self$scheduler$progress$any_remaining()) {
        self$iterate()
      }
    },
    end = function() {
      # Cleans up psock clusters.
      # Implicitly assumes .cleanup = TRUE is the default. # nolint
      # Does not set .cleanup directly # nolint
      # because this argument may not be supported long-term.
      future::plan(future::sequential)
      super$end()
    },
    validate = function() {
      super$validate()
      tar_assert_int(self$workers)
    }
  )
)

future_value_target <- function(value, name, pipeline) {
  UseMethod("future_value_target")
}

#' @export
future_value_target.tar_target <- function(value, name, pipeline) {
  value
}

#' @export
future_value_target.condition <- function(value, name, pipeline) {
  target <- pipeline_get_target(pipeline, name)
  builder_error_internal(target, value, "_future_")
}
