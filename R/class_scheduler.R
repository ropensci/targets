scheduler_init <- function(pipeline = pipeline_init(), reporter = "verbose") {
  names <- pipeline_get_names(pipeline)
  graph <- graph_init(pipeline)
  queue <- queue_init(names, graph$produce_degrees(names, "upstream"))
  queued <- counter_init(names)
  progress <- progress_init(queued = queued)
  reporter <- reporter_init(reporter)
  scheduler_new(graph, queue, progress, reporter)
}

scheduler_new <- function(
  graph = NULL,
  queue = NULL,
  progress = NULL,
  reporter = NULL
) {
  scheduler_class$new(graph, queue, progress, reporter)
}

scheduler_class <- R6::R6Class(
  classname = "tar_scheduler",
  class = FALSE,
  portable = FALSE,
  cloneable = FALSE,
  public = list(
    graph = NULL,
    queue = NULL,
    progress = NULL,
    reporter = NULL,
    initialize = function(
      graph = NULL,
      queue = NULL,
      progress = NULL,
      reporter = NULL
    ) {
      self$graph <- graph
      self$queue <- queue
      self$progress <- progress
      self$reporter <- reporter
    },
    count_unfinished_deps = function(name) {
      deps <- self$graph$produce_upstream(name)
      deps_queued <- counter_filter_exists(
        self$progress$queued,
        deps
      )
      deps_running <- counter_filter_exists(
        self$progress$running,
        deps
      )
      length(deps_queued) + length(deps_running)
    },
    validate = function() {
      self$graph$validate()
      self$queue$validate()
      self$progress$validate()
      self$reporter$validate()
    }
  )
)
