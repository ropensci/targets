clustermq_init <- function(
  pipeline = NULL,
  meta = meta_init(),
  names = NULL,
  shortcut = FALSE,
  queue = "parallel",
  reporter = "verbose",
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
  envir = NULL,
  workers = NULL,
  crew = NULL,
  log_worker = NULL
) {
  clustermq_class$new(
    pipeline = pipeline,
    meta = meta,
    names = names,
    shortcut = shortcut,
    queue = queue,
    reporter = reporter,
    envir = envir,
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
      shortcut = NULL,
      queue = NULL,
      reporter = NULL,
      envir = NULL,
      workers = NULL,
      crew = NULL,
      log_worker = NULL
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
      self$workers <- as.integer(workers)
      self$crew <- crew
      self$log_worker <- log_worker
    },
    set_common_data = function() {
      self$crew$set_common_data(
        fun = identity,
        const = list(),
        export = self$produce_exports(
          envir = self$envir,
          path_store = self$meta$get_path_store()
        ),
        rettype = list(),
        pkgs = "targets",
        common_seed = 0L,
        token = "set_common_data_token"
      )
    },
    create_crew = function() {
      crew <- clustermq::workers(
        n_jobs = self$workers,
        template = tar_option_get("resources")$clustermq$template %|||%
          tar_option_get("resources") %|||%
          list(),
        log_worker = self$log_worker
      )
      self$crew <- crew
    },
    start_crew = function() {
      self$create_crew()
      self$set_common_data()
    },
    any_upcoming_jobs = function() {
      need_workers <- fltr(
        counter_get_names(self$scheduler$progress$queued),
        ~ target_needs_worker(pipeline_get_target(self$pipeline, .x))
      )
      length(need_workers) > 0L
    },
    run_worker = function(target) {
      args <- list(
        expr = quote(
          target_run_worker(
            target = target,
            envir = .tar_envir_5048826d,
            path_store = .tar_path_store_5048826d,
            fun = .tar_fun_5048826d,
            options = .tar_options_5048826d,
            envvars = .tar_envvars_5048826d
          )
        ),
        env = list(target = target)
      )
      do.call(what = self$crew$send_call, args = args)
    },
    run_main = function(target) {
      self$wait_or_shutdown()
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
      self$wait_or_shutdown()
      target_skip(
        target = target,
        pipeline = self$pipeline,
        scheduler = self$scheduler,
        meta = self$meta,
        active = TRUE
      )
      target_sync_file_meta(target, self$meta)
    },
    shut_down_worker = function() {
      if (self$workers > 0L) {
        self$crew$send_shutdown_worker()
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
          self$crew$send_wait(),
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
    iterate = function() {
      message <- if_any(self$workers > 0L, self$crew$receive_data(), list())
      self$conclude_worker_target(message$result)
      token <- message$token
      if (self$workers > 0L && !identical(token, "set_common_data_token")) {
        self$crew$send_common_data()
      } else if (self$scheduler$queue$is_nonempty()) {
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
        envir = self$envir,
        scheduler = self$scheduler
      )
    },
    run_clustermq = function() {
      on.exit(try(self$crew$finalize()))
      self$start_crew()
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
        tar_condition_prelocal = function(e) NULL
      )
      if (self$scheduler$queue$is_nonempty()) {
        self$run_clustermq()
      }
    },
    validate = function() {
      super$validate()
      tar_assert_int(self$workers)
    }
  )
)
