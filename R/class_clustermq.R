clustermq_init <- function(
  pipeline = NULL,
  names = NULL,
  queue = "parallel",
  meta = meta_init(),
  reporter = "verbose",
  garbage_collection = FALSE,
  workers = 1L,
  template = list(),
  log_worker = FALSE
) {
  pipeline_prune_names(pipeline, names)
  scheduler <- pipeline_produce_scheduler(pipeline, queue, reporter)
  clustermq_new(
    pipeline,
    scheduler,
    meta,
    garbage_collection,
    workers = as.integer(workers),
    template = as.list(template),
    log_worker = log_worker
  )
}

clustermq_new <- function(
  pipeline = NULL,
  scheduler = NULL,
  meta = NULL,
  garbage_collection = NULL,
  workers = NULL,
  crew = NULL,
  template = NULL,
  log_worker = NULL
) {
  clustermq_class$new(
    pipeline = pipeline,
    scheduler = scheduler,
    meta = meta,
    garbage_collection = garbage_collection,
    workers = workers,
    crew = crew,
    template = template,
    log_worker = log_worker
  )
}

clustermq_class <- R6::R6Class(
  classname = "tar_clustermq",
  inherit = algorithm_class,
  class = FALSE,
  portable = FALSE,
  cloneable = FALSE,
  public = list(
    garbage_collection = NULL,
    workers = NULL,
    crew = NULL,
    template = NULL,
    log_worker = NULL,
    initialize = function(
      pipeline = NULL,
      scheduler = NULL,
      meta = NULL,
      garbage_collection = NULL,
      workers = NULL,
      crew = NULL,
      template = NULL,
      log_worker = NULL
    ) {
      super$initialize(
        pipeline = pipeline,
        scheduler = scheduler,
        meta = meta
      )
      self$garbage_collection <- garbage_collection
      self$workers <- workers
      self$crew <- crew
      self$template <- template
      self$log_worker <- log_worker
    },
    set_common_data = function(envir) {
      self$crew$set_common_data(
        export = as.list(envir, all.names = FALSE),
        fun = identity,
        const = list(),
        rettype = list(),
        pkgs = "targets",
        common_seed = 0L,
        token = "set_common_data_token"
      )
    },
    create_crew = function() {
      crew <- clustermq::workers(
        n_jobs = self$workers,
        template = self$template,
        log_worker = self$log_worker
      )
      self$crew <- crew
    },
    start_crew = function(envir) {
      self$create_crew()
      self$set_common_data(envir)
    },
    run_remote = function(target) {
      self$crew$send_call(
        expr = target_run_remote(target, garbage_collection),
        env = list(
          target = target,
          garbage_collection = self$garbage_collection
        )
      )
    },
    run_local = function(target) {
      self$crew$send_wait()
      target_run(target)
      target_conclude(
        target,
        self$pipeline,
        self$scheduler,
        self$meta
      )
    },
    run_target = function(name) {
      if (self$garbage_collection) {
        gc()
      }
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
      self$crew$send_wait()
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
      self$crew$send_wait()
      Sys.sleep(0.001)
    },
    next_target = function() {
      queue <- self$scheduler$queue
      trn(
        queue$should_dequeue(),
        self$process_target(queue$dequeue()),
        self$wait()
      )
    },
    conclude_remote_target = function(target) {
      if (is.null(target)) {
        return()
      }
      pipeline_set_target(self$pipeline, target)
      builder_unserialize_value(target)
      target_conclude(
        target,
        self$pipeline,
        self$scheduler,
        self$meta
      )
    },
    iterate = function() {
      message <- self$crew$receive_data()
      self$conclude_remote_target(message$result)
      if (!identical(message$token, "set_common_data_token")) {
        self$crew$send_common_data()
      } else if (self$scheduler$queue$is_nonempty()) {
        self$next_target()
      } else {
        self$crew$send_shutdown_worker() # nocov
      }
    },
    produce_prelocal = function() {
      prelocal_new(
        pipeline = self$pipeline,
        scheduler = self$scheduler,
        meta = self$meta,
        garbage_collection = self$garbage_collection
      )
    },
    start = function() {
      assert_package("clustermq")
      self$start_algorithm()
    },
    end = function() {
      self$end_algorithm()
      run_gc(self$garbage_collection)
    },
    run_clustermq = function() {
      on.exit(self$crew$finalize())
      self$start_crew(pipeline_get_envir(self$pipeline))
      while (self$scheduler$progress$any_remaining()) {
        self$iterate()
      }
      if (self$crew$cleanup()) {
        on.exit()
      }
    },
    run = function() {
      self$start()
      tryCatch(
        self$produce_prelocal()$run(),
        condition_prelocal = function(e) NULL
      )
      if (self$scheduler$queue$is_nonempty()) {
        self$run_clustermq()
      }
      self$end()
    },
    validate = function() {
      super$validate()
      assert_lgl(self$garbage_collection)
      assert_int(self$workers)
      assert_list(self$template)
    }
  )
)
