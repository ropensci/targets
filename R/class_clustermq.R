clustermq_init <- function(
  pipeline = NULL,
  meta = meta_init(),
  names = NULL,
  queue = "parallel",
  reporter = "verbose",
  workers = 1L,
  log_worker = FALSE
) {
  clustermq_new(
    pipeline = pipeline,
    meta = meta,
    names = names,
    queue = queue,
    reporter = reporter,
    workers = as.integer(workers),
    log_worker = log_worker
  )
}

clustermq_new <- function(
  pipeline = NULL,
  meta = NULL,
  names = NULL,
  queue = NULL,
  reporter = NULL,
  workers = NULL,
  crew = NULL,
  log_worker = NULL
) {
  clustermq_class$new(
    pipeline = pipeline,
    meta = meta,
    names = names,
    queue = queue,
    reporter = reporter,
    workers = workers,
    crew = crew,
    log_worker = log_worker
  )
}

clustermq_class <- R6::R6Class(
  classname = "tar_clustermq",
  inherit = active_class,
  portable = FALSE,
  cloneable = FALSE,
  public = list(
    workers = NULL,
    crew = NULL,
    log_worker = NULL,
    initialize = function(
      pipeline = NULL,
      meta = NULL,
      names = NULL,
      queue = NULL,
      reporter = NULL,
      workers = NULL,
      crew = NULL,
      log_worker = NULL
    ) {
      super$initialize(
        pipeline = pipeline,
        meta = meta,
        names = names,
        queue = queue,
        reporter = reporter
      )
      self$workers <- workers
      self$crew <- crew
      self$log_worker <- log_worker
    },
    set_common_data = function(envir) {
      self$crew$set_common_data(
        fun = identity,
        const = list(),
        export = self$produce_exports(envir),
        rettype = list(),
        pkgs = "targets",
        common_seed = 0L,
        token = "set_common_data_token"
      )
    },
    create_crew = function() {
      crew <- clustermq::workers(
        n_jobs = self$workers,
        template = tar_option_get("resources"),
        log_worker = self$log_worker
      )
      self$crew <- crew
    },
    start_crew = function(envir) {
      self$create_crew()
      self$set_common_data(envir)
    },
    run_worker = function(target) {
      self$crew$send_call(
        expr = target_run_worker(target, .tar_envir_5048826d),
        env = list(target = target)
      )
    },
    run_main = function(target) {
      self$crew$send_wait()
      target_run(target, tar_option_get("envir"))
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
    skip_target = function(target) {
      self$crew$send_wait()
      target_skip(
        target,
        self$pipeline,
        self$scheduler,
        self$meta
      )
      target_sync_file_meta(target, self$meta)
    },
    # Requires a longer target to guarantee test coverage.
    # Tested in tests/hpc/test-clustermq.R.
    # nocov start
    wait = function() {
      self$crew$send_wait()
      self$scheduler$backoff$wait()
    },
    # nocov end
    next_target = function() {
      queue <- self$scheduler$queue
      trn(
        queue$should_dequeue(),
        self$process_target(queue$dequeue()),
        self$wait()
      )
    },
    conclude_worker_target = function(target) {
      if (is.null(target)) {
        return()
      }
      pipeline_set_target(self$pipeline, target)
      self$unserialize_target(target)
      target_conclude(
        target,
        self$pipeline,
        self$scheduler,
        self$meta
      )
    },
    iterate = function() {
      message <- self$crew$receive_data()
      self$conclude_worker_target(message$result)
      if (!identical(message$token, "set_common_data_token")) {
        self$crew$send_common_data()
      } else if (self$scheduler$queue$is_nonempty()) {
        self$next_target()
      } else {
        # To cover this line, we need a longer and more complicated pipeline
        # than would be appropriate for fully automated testing.
        # tests/hpc/test-clustermq.R has a couple longer tests.
        self$crew$send_shutdown_worker() # nocov
      }
    },
    produce_prelocal = function() {
      prelocal_new(
        pipeline = self$pipeline,
        meta = self$meta,
        names = self$names,
        queue = self$queue,
        reporter = self$reporter,
        scheduler = self$scheduler
      )
    },
    run_clustermq = function() {
      on.exit(try(self$crew$finalize()))
      self$start_crew(tar_option_get("envir"))
      while (self$scheduler$progress$any_remaining()) {
        self$iterate()
      }
      if (identical(try(self$crew$cleanup()), TRUE)) {
        on.exit()
      }
    },
    run = function() {
      self$start()
      on.exit(self$end())
      tryCatch(
        self$produce_prelocal()$run(),
        condition_prelocal = function(e) NULL
      )
      if (self$scheduler$queue$is_nonempty()) {
        self$run_clustermq()
      }
    },
    validate = function() {
      super$validate()
      assert_int(self$workers)
    }
  )
)
