pipeline_init <- function(targets = list(), clone_targets = TRUE) {
  targets <- pipeline_targets_init(targets, clone_targets)
  imports <- imports_init(tar_option_get("envir"))
  pipeline_new(
    targets = targets,
    imports = imports,
    loaded = counter_init(),
    transient = counter_init()
  )
}

pipeline_new <- function(
  targets = NULL,
  imports = NULL,
  loaded = NULL,
  transient = NULL
) {
  force(targets)
  force(imports)
  force(loaded)
  force(transient)
  enclass(environment(), "tar_pipeline")
}

pipeline_targets_init <- function(targets, clone_targets) {
  targets <- targets %|||% list()
  tar_assert_target_list(targets)
  names <- map_chr(targets, ~.x$settings$name)
  tar_assert_unique_targets(names)
  if (clone_targets) {
    # If the user has target objects in the global environment,
    # loading data into them may cause huge data transfers to workers.
    # Best to not modify the user's copies of target objects.
    targets <- map(targets, ~target_subpipeline_copy(.x, keep_value = FALSE))
  }
  names(targets) <- names
  list2env(targets, parent = emptyenv(), hash = TRUE)
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

pipeline_uses_priorities <- function(pipeline) {
  any(length(unique(pipeline_get_priorities(pipeline))) > 1L)
}

pipeline_reset_priorities <- function(pipeline) {
  map(pipeline_get_names(pipeline), ~pipeline_reset_priority(pipeline, .x))
}

pipeline_reset_priority <- function(pipeline, name) {
  target <- pipeline_get_target(pipeline, name)
  target$settings$priority <- 0
}

pipeline_reset_deployments <- function(pipeline) {
  map(pipeline_get_names(pipeline), ~pipeline_reset_deployment(pipeline, .x))
}

pipeline_reset_deployment <- function(pipeline, name) {
  target <- pipeline_get_target(pipeline, name)
  target$settings$deployment <- "main"
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
  envir <- pipeline$targets %|||% tar_empty_envir
  exists(x = name, envir = envir, inherits = FALSE)
}

pipeline_exists_import <- function(pipeline, name) {
  exists(x = name, envir = pipeline$imports, inherits = FALSE)
}

pipeline_exists_object <- function(pipeline, name) {
  pipeline_exists_target(pipeline, name) ||
    pipeline_exists_import(pipeline, name)
}

pipeline_targets_only_edges <- function(edges) {
  edges[edges$from %in% edges$to,, drop = FALSE] # nolint
}

pipeline_upstream_edges <- function(pipeline, targets_only = TRUE) {
  edge_list <- map(pipeline$targets, ~target_upstream_edges(.x))
  edges <- do.call(rbind, edge_list)
  edges <- if_any(targets_only, pipeline_targets_only_edges(edges), edges)
  edges <- edges %|||% data_frame(from = character(0), to = character(0))
  rownames(edges) <- NULL
  edges
}

pipeline_produce_igraph <- function(pipeline, targets_only = TRUE) {
  edges <- pipeline_upstream_edges(pipeline, targets_only = targets_only)
  igraph::simplify(igraph::graph_from_data_frame(edges))
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

pipeline_produce_subpipeline <- function(pipeline, name, keep_value = NULL) {
  target <- pipeline_get_target(pipeline, name)
  deps <- target_deps_deep(target, pipeline)
  targets <- new.env(parent = emptyenv())
  keep_value <- keep_value %|||% identical(target$settings$retrieval, "main")
  lapply(
    deps,
    pipeline_assign_target_copy,
    pipeline = pipeline,
    envir = targets,
    keep_value = keep_value
  )
  pipeline_new(
    targets = targets,
    loaded = counter_init(),
    transient = counter_init()
  )
}

pipeline_assign_target_copy <- function(pipeline, name, envir, keep_value) {
  target <- pipeline_get_target(pipeline, name)
  copy <- target_subpipeline_copy(target, keep_value)
  assign(name, copy, envir = envir)
}

pipeline_serialize_values <- function(pipeline) {
  map(
    pipeline_get_names(pipeline),
    ~target_serialize_value(pipeline_get_target(pipeline, .x))
  )
}

pipeline_unserialize_values <- function(pipeline) {
  map(
    pipeline_get_names(pipeline),
    ~target_unserialize_value(pipeline_get_target(pipeline, .x))
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

pipeline_prune_shortcut <- function(pipeline, names, shortcut) {
  if (is.null(names) || !shortcut) {
    return(pipeline)
  }
  available <- intersect(names, pipeline_get_names(pipeline))
  targets <- map(available, ~pipeline_get_target(pipeline, .x))
  pipeline_init(targets = targets, clone_targets = FALSE)
}

pipeline_get_packages <- function(pipeline) {
  out <- map(
    pipeline_get_names(pipeline),
    ~target_get_packages(pipeline_get_target(pipeline, .x))
  )
  sort(unique(unlist(out)))
}

pipeline_bootstrap_deps <- function(pipeline, meta, names) {
  deps <- map(names, ~pipeline_get_target(pipeline, .x)$command$deps)
  deps <- intersect(unique(unlist(deps)), pipeline_get_names(pipeline))
  deps <- setdiff(x = deps, y = names)
  map(
    deps,
    ~target_bootstrap(pipeline_get_target(pipeline, .x), pipeline, meta)
  )
}

pipeline_validate_targets <- function(targets) {
  eapply(targets, function(target) target_validate(target))
}

pipeline_validate_dag <- function(igraph) {
  if (!igraph::is_dag(igraph)) {
    tar_throw_validate("graph contains a cycle.")
  }
}

pipeline_validate_conflicts <- function(pipeline) {
  conflicts <- intersect(names(pipeline$imports), names(pipeline$targets))
  msg <- paste0(
    "Targets and globals must have unique names. ",
    "Ignoring global objects that conflict with target names: ",
    paste(conflicts, collapse = ", "),
    ". Suppress this warning with Sys.setenv(TAR_WARN = \"false\") ",
    "in _targets.R."
  )
  if (length(conflicts) && !identical(Sys.getenv("TAR_WARN"), "false")) {
    tar_warn_validate(msg)
  }
}

pipeline_validate <- function(pipeline) {
  pipeline_validate_lite(pipeline)
  pipeline_validate_targets(pipeline$targets)
  pipeline_validate_dag(pipeline_produce_igraph(pipeline))
  counter_validate(pipeline$loaded)
  counter_validate(pipeline$transient)
}

#' @title Abridged pipeline validation function.
#' @export
#' @keywords internal
#' @description Internal function. Do not invoke directly.
#' @param pipeline A pipeline object.
pipeline_validate_lite <- function(pipeline) {
  tar_assert_inherits(pipeline, "tar_pipeline", msg = "invalid pipeline.")
  tar_assert_correct_fields(pipeline, pipeline_new)
  pipeline_validate_conflicts(pipeline)
}

#' @title Convert to a pipeline object.
#' @export
#' @keywords internal
#' @description Not a user-side function. Do not invoke directly.
#' @return An object of class `"tar_pipeline"`.
#' @param x A list of target objects or a pipeline object.
as_pipeline <- function(x) {
  UseMethod("as_pipeline")
}

#' @export
#' @keywords internal
as_pipeline.tar_pipeline <- function(x) {
  x
}

#' @export
#' @keywords internal
as_pipeline.default <- function(x) {
  pipeline_init(unlist(list(x), recursive = TRUE))
}

#' @export
#' @keywords internal
print.tar_pipeline <- function(x, ...) {
  count <- length(pipeline_get_names(x))
  msg <- paste("<tar_pipeline>\n  targets:", count)
  cat(msg)
}
