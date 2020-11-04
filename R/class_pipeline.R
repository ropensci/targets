pipeline_init <- function(targets = list()) {
  targets <- pipeline_targets_init(targets)
  pipeline_new(
    targets = targets,
    envir = pipeline_envir(targets),
    loaded = counter_init(),
    transient = counter_init()
  )
}

pipeline_new <- function(
  targets = NULL,
  envir = NULL,
  loaded = NULL,
  transient = NULL
) {
  force(targets)
  force(envir)
  force(loaded)
  force(transient)
  enclass(environment(), "tar_pipeline")
}

pipeline_targets_init <- function(targets) {
  targets <- targets %||% list()
  names <- map_chr(targets, ~.x$settings$name)
  assert_unique_targets(names)
  names(targets) <- names
  list2env(targets, parent = emptyenv(), hash = TRUE)
}

pipeline_envir <- function(targets) {
  for (name in names(targets)) {
    target <- targets[[name]]
    if (inherits(target, "tar_stem")) {
      return(target$cache$imports$envir)
    }
  }
  target_empty_envir
}

pipeline_get_target <- function(pipeline, name) {
  pipeline$targets[[name]]
}

pipeline_get_names <- function(pipeline) {
  names(pipeline$targets)
}

pipeline_get_priorities <- function(pipeline) {
  map_dbl(
    pipeline_get_names(pipeline),
    ~pipeline_get_target(pipeline, .x)$settings$priority,
    USE.NAMES = TRUE
  )
}

pipeline_reset_priorities <- function(pipeline) {
  map(pipeline_get_names(pipeline), ~pipeline_reset_priority(pipeline, .x))
}

pipeline_reset_priority <- function(pipeline, name) {
  target <- pipeline_get_target(pipeline, name)
  target$settings$priority <- 0
}

pipeline_set_target <- function(pipeline, target) {
  assign(
    x = target$settings$name,
    value = target,
    envir = pipeline$targets,
    inherits = FALSE,
    immediate = TRUE
  )
  invisible()
}

pipeline_exists_target <- function(pipeline, name) {
  exists(x = name, envir = pipeline$targets, inherits = FALSE)
}

pipeline_targets_only_edges <- function(edges) {
  edges[edges$from %in% edges$to,, drop = FALSE] # nolint
}

pipeline_upstream_edges <- function(pipeline, targets_only = TRUE) {
  edge_list <- map(pipeline$targets, ~target_upstream_edges(.x))
  edges <- do.call(rbind, edge_list)
  edges <- trn(targets_only, pipeline_targets_only_edges(edges), edges)
  edges <- edges %||% data_frame(from = character(0), to = character(0))
  rownames(edges) <- NULL
  edges
}

pipeline_produce_igraph <- function(pipeline, targets_only = TRUE) {
  edges <- pipeline_upstream_edges(pipeline, targets_only = targets_only)
  igraph::simplify(igraph::graph_from_data_frame(edges))
}

pipeline_produce_scheduler <- function(
  pipeline,
  queue = "parallel",
  reporter = "verbose"
) {
  scheduler_init(pipeline = pipeline, queue = queue, reporter = reporter)
}

pipeline_register_loaded_target <- function(pipeline, name) { # nolint
  counter_set_name(pipeline$loaded, name)
  target <- pipeline_get_target(pipeline, name)
  if (identical(target$settings$memory, "transient")) {
    counter_set_name(pipeline$transient, name)
  }
}

pipeline_register_loaded <- function(pipeline, names) {
  lapply(names, pipeline_register_loaded_target, pipeline = pipeline)
}

pipeline_unload_target <- function(pipeline, name) {
  target <- pipeline_get_target(pipeline, name)
  store_unload(target$store, target)
  target$value <- NULL
  counter_del_name(pipeline$loaded, name)
  counter_del_name(pipeline$transient, name)
}

pipeline_unload_loaded <- function(pipeline) {
  for (name in counter_get_names(pipeline$loaded)) {
    pipeline_unload_target(pipeline, name)
    counter_del_name(pipeline$loaded, name)
    counter_del_name(pipeline$transient, name)
  }
}

pipeline_unload_transient <- function(pipeline) {
  for (name in counter_get_names(pipeline$transient)) {
    pipeline_unload_target(pipeline, name)
    counter_del_name(pipeline$loaded, name)
    counter_del_name(pipeline$transient, name)
  }
}

pipeline_produce_subpipeline <- function(pipeline, name) {
  target <- pipeline_get_target(pipeline, name)
  deps <- target_deps_deep(target, pipeline)
  targets <- new.env(parent = emptyenv())
  lapply(
    deps,
    pipeline_assign_target_copy,
    pipeline = pipeline,
    envir = targets
  )
  pipeline_new(
    targets = targets,
    loaded = counter_init(),
    transient = counter_init()
  )
}

pipeline_assign_target_copy <- function(pipeline, name, envir) {
  target <- pipeline_get_target(pipeline, name)
  copy <- target_subpipeline_copy(target)
  assign(name, copy, envir = envir)
}

pipeline_prune_names <- function(pipeline, names) {
  if (!is.null(names)) {
    pipeline_prune_targets(pipeline, names)
  }
}

pipeline_prune_targets <- function(pipeline, names) {
  graph <- pipeline_produce_igraph(pipeline, targets_only = TRUE)
  keep <- upstream_vertices(graph = graph, from = names)
  discard <- setdiff(pipeline_get_names(pipeline), keep)
  remove(list = discard, envir = pipeline$targets, inherits = FALSE)
}

pipeline_validate_targets <- function(targets) {
  eapply(targets, function(target) target_validate(target))
}

pipeline_validate_dag <- function(igraph) {
  if (!igraph::is_dag(igraph)) {
    throw_validate("graph contains a cycle.")
  }
}

pipeline_validate_envirs <- function(pipeline) {
  targets <- as.list(pipeline$targets)
  if (!length(targets)) {
    return()
  }
  envir <- targets[[1]]$cache$imports$envir
  assert_envir(envir)
  assert_identical(
    envir,
    pipeline$envir,
    "pipeline and target environments must agree."
  )
  lapply(targets, pipeline_validate_envir, envir = envir)
}

pipeline_validate_envir <- function(target, envir) {
  if (!identical(target$cache$imports$envir, envir)) {
    throw_validate("all targets must share the same environment")
  }
}

pipeline_validate <- function(pipeline) {
  UseMethod("pipeline_validate")
}

#' @export
#' @keywords internal
pipeline_validate.tar_pipeline <- function(pipeline) {
  assert_correct_fields(pipeline, pipeline_new)
  pipeline_validate_targets(pipeline$targets)
  pipeline_validate_dag(pipeline_produce_igraph(pipeline))
  pipeline_validate_envirs(pipeline)
  counter_validate(pipeline$loaded)
  counter_validate(pipeline$transient)
}

#' @export
#' @keywords internal
pipeline_validate.default <- function(pipeline) {
  throw_validate("not a tar_pipeline() object. _targets.R must end with one.")
}

pipeline_validate_lite <- function(pipeline) {
  UseMethod("pipeline_validate_lite")
}

#' @export
#' @keywords internal
pipeline_validate_lite.tar_pipeline <- function(pipeline) {
  assert_correct_fields(pipeline, pipeline_new)
  pipeline_validate_envirs(pipeline)
}

#' @export
#' @keywords internal
pipeline_validate_lite.default <- function(pipeline) {
  throw_validate("not a tar_pipeline() object. _targets.R must end with one.")
}

#' @export
#' @keywords internal
print.tar_pipeline <- function(x, ...) {
  count <- length(pipeline_get_names(x))
  msg <- paste0(
    "<pipeline with ",
    count,
    " target",
    trn(identical(count, 1L), "", "s"),
    ">"
  )
  cat(msg)
}
