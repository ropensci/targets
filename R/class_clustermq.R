clustermq_init <- function(
  pipeline = NULL,
  meta = meta_init(),
  names = NULL,
  shortcut = FALSE,
  queue = "parallel",
  reporter = "verbose",
  seconds_meta_append = 0,
  seconds_meta_upload = 15,
  envir = tar_option_get("envir"),
  workers = 1L,
  log_worker = FALSE
) {
  clustermq_new(
    pipeline = pipeline,
    meta = meta,
    names = names,
    shortcut = shortcut,
    queue = queue,
    reporter = reporter,
    seconds_meta_append = seconds_meta_append,
    seconds_meta_upload = seconds_meta_upload,
    envir = envir,
    workers = as.integer(workers),
    log_worker = log_worker
  )
}

clustermq_new <- function(
  pipeline = NULL,
  meta = NULL,
  names = NULL,
  shortcut = NULL,
  queue = NULL,
  reporter = NULL,
  seconds_meta_append = NULL,
  seconds_meta_upload = NULL,
  envir = NULL,
  workers = NULL,
  log_worker = NULL
) {
  clustermq_class$new(
    pipeline = pipeline,
    meta = meta,
    names = names,
    shortcut = shortcut,
    queue = queue,
    reporter = reporter,
    seconds_meta_append = seconds_meta_append,
    seconds_meta_upload = seconds_meta_upload,
    envir = envir,
    workers = workers,
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
    log_worker = NULL,
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
      envir = NULL,
      workers = NULL,
      log_worker = NULL
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
        envir = envir
      )
      self$workers <- as.integer(workers)
      self$log_worker <- log_worker
    },
    # Need to suppress tests on covr only, due to
    # https://github.com/r-lib/covr/issues/315.
    # Cannot use multicore clustermq backend
    # due to https://github.com/ropensci/targets/discussions/780
    # nocov start
    start_workers = function() {
      self$worker_list <- clustermq::workers(
        n_jobs = self$workers,
        template = tar_option_get("resources")$clustermq$template %|||%
          tar_option_get("resources") %|||%
          list(),
        log_worker = self$log_worker
      )
      exports <- self$produce_exports(
        envir = self$envir,
        path_store = self$meta$store
      )
      do.call(what = self$worker_list$env, args = exports)
    },
    any_upcoming_jobs = function() {
      need_workers <- fltr(
        counter_get_names(self$scheduler$progress$queued),
        ~ target_needs_worker(pipeline_get_target(self$pipeline, .x))
      )
      length(need_workers) > 0L
    },
    run_worker = function(target) {
      builder_marshal_subpipeline(target)
      self$worker_list$send(
        cmd = targets::target_run_worker(
          target = target,
          envir = .tar_envir_5048826d,
          path_store = .tar_path_store_5048826d,
          fun = .tar_fun_5048826d,
          options = .tar_options_5048826d,
          envvars = .tar_envvars_5048826d
        ),
        target = target
      )
    },
    run_main = function(target) {
      self$wait_or_shutdown()
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
    run_target = function(target) {
      target_prepare(target, self$pipeline, self$scheduler, self$meta)
      self$sync_meta_time()
      if_any(
        target_should_run_worker(target),
        self$run_worker(target),
        self$run_main(target)
      )
      self$unload_transient()
    },
    skip_target = function(target) {
      self$wait_or_shutdown()
      target_skip(
        target = target,
        pipeline = self$pipeline,
        scheduler = self$scheduler,
        meta = self$meta,
        active = TRUE
      )
    },
    shut_down_worker = function() {
      if (self$workers > 0L) {
        self$worker_list$send_shutdown()
        self$workers <- self$workers - 1L
        self$scheduler$backoff$reset()
      }
    },
    # Requires a long-running pipeline to guarantee test coverage,
    # which is not appropriate for fully automated unit tests.
    # Covered in tests/interactive/test-parallel.R
    # and tests/hpc/test-clustermq.R.
    # nocov start
    wait_or_shutdown = function() {
      try(
        if_any(
          self$any_upcoming_jobs(),
          self$worker_list$send_wait(),
          self$shut_down_worker()
        )
      )
    },
    backoff = function() {
      self$wait_or_shutdown()
      self$scheduler$backoff$wait()
    },
    # nocov end
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
      self$unmarshal_target(target)
      target_conclude(
        target,
        self$pipeline,
        self$scheduler,
        self$meta
      )
      self$scheduler$backoff$reset()
    },
    iterate = function() {
      self$sync_meta_time()
      if (self$workers > 0L) {
        self$conclude_worker_target(self$worker_list$recv())
      }
      if (self$scheduler$queue$is_nonempty()) {
        self$next_target()
      } else {
        self$shut_down_worker()
      }
    },
    produce_prelocal = function() {
      prelocal_new(
        pipeline = self$pipeline,
        meta = self$meta,
        names = self$names,
        queue = self$queue,
        reporter = self$reporter,
        seconds_meta_append = self$seconds_meta_append,
        seconds_meta_upload = self$seconds_meta_upload,
        envir = self$envir,
        scheduler = self$scheduler
      )
    },
    run_clustermq = function() {
      on.exit({
        if (!is.null(self$worker_list)) {
          try(self$worker_list$cleanup())
        }
      })
      self$start_workers()
      while (self$scheduler$progress$any_remaining()) {
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
        self$run_clustermq()
      }
    },
    # nocov end
    validate = function() {
      super$validate()
      tar_assert_int(self$workers)
    }
  )
)
