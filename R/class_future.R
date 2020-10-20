future_init <- function(
  pipeline = NULL,
  meta = meta_init(),
  names = NULL,
  queue = "parallel",
  reporter = "verbose",
  workers = 1L
) {
  future_new(
    pipeline = pipeline,
    meta = meta,
    names = names,
    queue = queue,
    reporter = reporter,
    workers = as.integer(workers)
  )
}

future_new <- function(
  pipeline = NULL,
  meta = NULL,
  names = NULL,
  queue = NULL,
  reporter = NULL,
  workers = NULL
) {
  future_class$new(
    pipeline = pipeline,
    meta = meta,
    names = names,
    queue = queue,
    reporter = reporter,
    workers = workers
  )
}

future_class <- R6::R6Class(
  classname = "tar_future",
  inherit = active_class,
  class = FALSE,
  portable = FALSE,
  cloneable = FALSE,
  public = list(
    workers = NULL,
    crew = NULL,
    globals = NULL,
    initialize = function(
      pipeline = NULL,
      meta = NULL,
      names = NULL,
      queue = NULL,
      reporter = NULL,
      workers = NULL
    ) {
      super$initialize(
        pipeline = pipeline,
        meta = meta,
        names = names,
        queue = queue,
        reporter = reporter
      )
      self$workers <- workers
      self$crew <- memory_init()
    },
    update_globals = function() {
      self$globals <- self$produce_exports(self$pipeline$envir)
    },
    ensure_globals = function() {
      if (is.null(self$globals)) {
        self$update_globals()
      }
    },
    run_worker = function(target) {
      self$ensure_globals()
      globals <- self$globals
      globals$.targets_target_5048826d <- target
      plan_new <- target$settings$resources$plan
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
      future <- future::future(
        expr = target_run_worker(.targets_target_5048826d),
        packages = "targets",
        globals = globals,
        label = target_get_name(target),
        resources = target$settings$resources,
        lazy = FALSE,
        seed = 0L
      )
      memory_set_object(
        self$crew,
        name = target_get_name(target),
        object = future
      )
    },
    run_main = function(target) {
      target_run(target)
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
      trn(
        target_should_run_worker(target),
        self$run_worker(target),
        self$run_main(target)
      )
      self$unload_transient()
    },
    wait = function() {
      Sys.sleep(0.001)
    },
    next_target = function() {
      queue <- self$scheduler$queue
      if (queue$should_dequeue()) {
        self$process_target(queue$dequeue())
      }
    },
    conclude_worker_target = function(target) {
      pipeline_set_target(self$pipeline, target)
      self$unserialize_target(target)
      target_conclude(
        target,
        self$pipeline,
        self$scheduler,
        self$meta
      )
    },
    scan_worker = function(name) {
      worker <- memory_get_object(self$crew, name)
      if (future::resolved(worker)) {
        self$conclude_worker_target(future::value(worker))
        memory_del_objects(self$crew, name)
      }
    },
    iterate = function() {
      lapply(self$crew$names, self$scan_worker)
      should_submit <- self$crew$count < self$workers &&
        self$scheduler$queue$is_nonempty()
      trn(
        should_submit,
        self$next_target(),
        self$wait()
      )
    },
    run = function() {
      self$start()
      while (self$scheduler$progress$any_remaining()) {
        self$iterate()
      }
      self$end()
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
      assert_int(self$workers)
    }
  )
)
