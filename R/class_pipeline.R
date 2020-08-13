pipeline_init <- function(targets = list()) {
  targets <- pipeline_targets_init(targets)
  pipeline_new(
    targets = targets,
    loaded = counter_init(),
    transient = counter_init(),
    stash = stash_init()
  )
}

pipeline_new <- function(
  targets = NULL,
  loaded = NULL,
  transient = NULL,
  stash = NULL
) {
  force(targets)
  force(loaded)
  force(transient)
  force(stash)
  enclass(environment(), "tar_pipeline")
}

pipeline_targets_init <- function(targets) {
  targets <- targets %||% list()
  names <- map_chr(targets, ~.x$settings$name)
  assert_unique_targets(names)
  names(targets) <- names
  list2env(targets, parent = emptyenv(), hash = TRUE)
}

pipeline_get_target <- function(pipeline, name) {
  pipeline$targets[[name]]
}

pipeline_get_names <- function(pipeline) {
  names(pipeline$targets)
}

pipeline_get_envir <- function(pipeline) {
  for (name in pipeline_get_names(pipeline)) {
    target <- pipeline_get_target(pipeline, name)
    if (inherits(target, "tar_stem")) {
      return(target$cache$imports$envir)
    }
  }
  target_empty_envir
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
  if (target$settings$memory == "transient") {
    counter_set_name(pipeline$transient, name)
  }
}

pipeline_register_loaded <- function(pipeline, names) {
  lapply(names, pipeline_register_loaded_target, pipeline = pipeline)
}

pipeline_unload_target <- function(pipeline, name) {
  target <- pipeline_get_target(pipeline, name)
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
  map(deps, ~assign(.x, pipeline_get_target(pipeline, .x), envir = targets))
  pipeline_new(
    targets = targets,
    loaded = counter_init(),
    transient = counter_init()
  )
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

pipeline_stash_targets <- function(pipeline, subpipeline) {
  stash_targets(pipeline$stash, subpipeline)
}

pipeline_restore_targets <- function(pipeline) {
  stash_restore_targets(pipeline$stash, pipeline)
}

pipeline_validate_targets <- function(targets) {
  eapply(targets, function(target) target_validate(target))
}

pipeline_validate_dag <- function(igraph) {
  if (!igraph::is_dag(igraph)) {
    throw_validate("graph contains a cycle.")
  }
}

pipeline_validate_envirs <- function(targets) {
  targets <- as.list(targets)
  if (!length(targets)) {
    return()
  }
  envir <- targets[[1]]$cache$imports$envir
  assert_envir(envir)
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
  pipeline_validate_envirs(pipeline$targets)
  counter_validate(pipeline$loaded)
  counter_validate(pipeline$transient)
  stash_validate(pipeline$stash)
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
  pipeline_validate_envirs(pipeline$targets)
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
  msg <- paste("<pipeline with", count, "targets>")
  cat(msg)
}
