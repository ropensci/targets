scheduler_init <- function(
  pipeline = pipeline_init(),
  meta = meta_init(),
  queue = "parallel",
  reporter = "verbose",
  seconds_reporter = 0.5,
  names = NULL,
  shortcut = FALSE
) {
  pipeline <- pipeline_prune_shortcut(pipeline, names, shortcut)
  edges <- pipeline_upstream_edges(pipeline, targets_only = TRUE)
  igraph <- igraph::simplify(igraph::graph_from_data_frame(edges))
  tar_assert_target_dag(igraph)
  graph <- graph_init(remove_loops(edges))
  priorities <- pipeline_get_priorities(pipeline)
  names <- scheduler_topo_sort(igraph, priorities, queue)
  queue <- queue_init(queue, names, initial_ranks(names, graph, priorities))
  queued <- counter_init(names)
  progress <- progress_init(
    path_store = meta$store,
    queued = queued
  )
  reporter <- reporter_init(reporter, seconds_interval = seconds_reporter)
  backoff <- tar_options$get_backoff()
  trimmed <- counter_init()
  scheduler_new(
    graph = graph,
    queue = queue,
    progress = progress,
    reporter = reporter,
    backoff = backoff,
    trimmed <- trimmed
  )
}

scheduler_topo_sort <- function(igraph, priorities, queue) {
  if_any(
    identical(queue, "parallel"),
    igraph::V(igraph)$name,
    topo_sort_custom(igraph, priorities)
  )
}

initial_ranks <- function(names, graph, priorities) {
  graph$produce_degrees(names, "upstream") + rank_offset(priorities[names])
}

rank_offset <- function(priorities) {
  - priorities / 2
}

scheduler_new <- function(
  graph = NULL,
  queue = NULL,
  progress = NULL,
  reporter = NULL,
  backoff = NULL,
  trimmed = NULL
) {
  scheduler_class$new(graph, queue, progress, reporter, backoff, trimmed)
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
    backoff = NULL,
    trimmed = NULL,
    initialize = function(
      graph = NULL,
      queue = NULL,
      progress = NULL,
      reporter = NULL,
      backoff = NULL,
      trimmed = NULL
    ) {
      self$graph <- graph
      self$queue <- queue
      self$progress <- progress
      self$reporter <- reporter
      self$backoff <- backoff
      self$trimmed <- trimmed
    },
    count_unfinished_deps = function(name) {
      deps <- self$graph$produce_upstream(name)
      deps_queued <- counter_filter_exists(
        self$progress$queued,
        deps
      )
      deps_dispatched <- counter_filter_exists(
        self$progress$dispatched,
        deps
      )
      length(deps_queued) + length(deps_dispatched)
    },
    abridge = function(target) {
      self$reporter$report_error(target$metrics$error)
      self$progress$abridge()
      self$queue$abridge()
    },
    trim = function(target, pipeline) {
      parent_name <- target_get_parent(target)
      parent_target <- pipeline_get_target(pipeline, parent_name)
      downstream <- self$graph$produce_downstream(parent_name)
      siblings <- target_get_children(parent_target)
      counter_set_names(self$trimmed, c(downstream, siblings))
    },
    validate = function() {
      self$graph$validate()
      self$queue$validate()
      self$progress$validate()
      self$reporter$validate()
      self$backoff$validate()
      counter_validate(self$trimmed)
    }
  )
)
