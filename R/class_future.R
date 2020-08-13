future_init <- function(
  pipeline = NULL,
  names = NULL,
  queue = "parallel",
  meta = meta_init(),
  reporter = "verbose",
  garbage_collection = FALSE,
  workers = 1L
) {
  pipeline_prune_names(pipeline, names)
  scheduler <- pipeline_produce_scheduler(pipeline, queue, reporter)
  future_new(
    pipeline,
    scheduler,
    meta,
    garbage_collection,
    workers = as.integer(workers),
    crew = memory_init()
  )
}

future_new <- function(
  pipeline = NULL,
  scheduler = NULL,
  meta = NULL,
  garbage_collection = NULL,
  workers = NULL,
  crew = NULL,
  globals = NULL
) {
  future_class$new(
    pipeline = pipeline,
    scheduler = scheduler,
    meta = meta,
    garbage_collection = garbage_collection,
    workers = workers,
    crew = crew,
    globals = globals
  )
}

future_class <- R6::R6Class(
  classname = "tar_future",
  inherit = algorithm_class,
  class = FALSE,
  portable = FALSE,
  cloneable = FALSE,
  public = list(
    garbage_collection = NULL,
    workers = NULL,
    crew = NULL,
    globals = NULL,
    initialize = function(
      pipeline = NULL,
      scheduler = NULL,
      meta = NULL,
      garbage_collection = NULL,
      workers = NULL,
      crew = NULL,
      globals = NULL
    ) {
      super$initialize(
        pipeline = pipeline,
        scheduler = scheduler,
        meta = meta
      )
      self$garbage_collection <- garbage_collection
      self$workers <- workers
      self$crew <- crew
      self$globals <- globals
    },
    update_globals = function() {
      globals <- as.list(
        pipeline_get_envir(self$pipeline),
        all.names = FALSE
      )
      globals$.targets_gc_5048826d <- self$garbage_collection
      self$globals <- globals
    },
    ensure_globals = function() {
      if (is.null(self$globals)) {
        self$update_globals()
      }
    },
    run_remote = function(target) {
      self$ensure_globals()
      globals <- self$globals
      globals$.targets_target_5048826d <- target
      future <- future::future(
        expr = target_run_remote(
          .targets_target_5048826d,
          .targets_gc_5048826d
        ),
        packages = "targets",
        globals = globals,
        label = target_get_name(target),
        resources = target$settings$resources
      )
      memory_set_object(
        self$crew,
        name = target_get_name(target),
        object = future
      )
    },
    run_local = function(target) {
      target_run(target)
      target_conclude(
        target,
        self$pipeline,
        self$scheduler,
        self$meta
      )
    },
    run_target = function(name) {
      run_gc(self$garbage_collection)
      target <- pipeline_get_target(self$pipeline, name)
      target_prepare(target, self$pipeline, self$scheduler)
      trn(
        target_should_run_remote(target),
        self$run_remote(target),
        self$run_local(target)
      )
      pipeline_unload_transient(self$pipeline)
    },
    skip_target = function(target) {
      target_skip(
        target,
        self$pipeline,
        self$scheduler,
        self$meta
      )
    },
    process_target = function(name) {
      target <- pipeline_get_target(self$pipeline, name)
      target_debug(target)
      target_update_depend(target, meta)
      trn(
        target_should_run(target, self$meta),
        self$run_target(name),
        self$skip_target(target)
      )
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
    conclude_remote_target = function(target) {
      pipeline_set_target(self$pipeline, target)
      builder_unserialize_value(target)
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
        self$conclude_remote_target(future::value(worker))
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
    start = function() {
      assert_package("future")
      super$start()
    },
    end = function() {
      super$end()
      run_gc(self$garbage_collection)
    },
    run = function() {
      self$start()
      while (self$scheduler$progress$any_remaining()) {
        self$iterate()
      }
      self$end()
    },
    validate = function() {
      super$validate()
      assert_int(self$workers)
    }
  )
)
