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
  out <- new.env(parent = emptyenv(), hash = FALSE)
  out$targets <- targets
  out$imports <- imports
  out$loaded <- loaded
  out$transient <- transient
  enclass(out, pipeline_s3_class)
}

pipeline_s3_class <- "tar_pipeline"

pipeline_targets_init <- function(targets, clone_targets) {
  targets <- targets %|||% list()
  tar_assert_target_list(targets)
  names <- map_chr(targets, ~ target_get_name(.x))
  tar_assert_unique_targets(names)
  if (clone_targets) {
    # If the user has target objects in the global environment,
    # loading data into them may cause huge data transfers to workers.
    # Best to not modify the user's copies of target objects.
    targets <- map(targets, ~ target_subpipeline_copy(.x, keep_value = FALSE))
  }
  names(targets) <- names
  list2env(targets, parent = emptyenv(), hash = TRUE)
}

pipeline_resolve_auto <- function(pipeline) {
  # eapply() is fine here because pipeline_resolve_auto()
  # is called before any dynamic branching occurs.
  targets <- .subset2(pipeline, "targets")
  eapply(
    targets,
    function(target) {
      branching_over <- .subset2(.subset2(target, "settings"), "dimensions")
      for (name_upstream in branching_over) {
        upstream <- .subset2(targets, name_upstream)
        if (inherits(upstream, "tar_stem")) {
          target_resolve_auto(upstream, "memory", value = "persistent")
          target_resolve_auto(target, "retrieval", value = "main")
        }
      }
    }
  )
  eapply(
    targets,
    function(target) {
      target_resolve_auto(target, "memory", value = "transient")
      target_resolve_auto(target, "retrieval", value = "worker")
    }
  )
}

pipeline_get_target <- function(pipeline, name) {
  out <- .subset2(.subset2(pipeline, "targets"), name)
  if (is_reference_not_target(out)) {
    out <- reference_produce_target(out, pipeline, name)
  }
  out
}

pipeline_set_target <- function(pipeline, target) {
  envir <- .subset2(pipeline, "targets")
  name <- target_get_name(target)
  envir[[name]] <- target
  NULL
}

pipeline_set_reference <- function(pipeline, target) {
  envir <- .subset2(pipeline, "targets")
  name <- target_get_name(target)
  envir[[name]] <- target_produce_reference(target)
  NULL
}

pipeline_initialize_references_children <- function(
  pipeline,
  name_parent,
  names_children,
  type
) {
  envir <- .subset2(pipeline, "targets")
  index <- 1L
  n <- length(names_children)
  bar <- cli_local_progress_bar_init(
    label = sprintf("creating %s references", type),
    total = n
  )
  on.exit(cli_local_progress_bar_destroy(bar = bar))
  while (index <= n) {
    name <- .subset(names_children, index)
    envir[[name]] <- reference_new(parent = name_parent, index = index)
    index <- index + 1L
    cli_local_progress_bar_update(bar = bar, index = index)
  }
  NULL
}

pipeline_get_names <- function(pipeline) {
  names(.subset2(pipeline, "targets"))
}

pipeline_get_priorities <- function(pipeline) {
  map_dbl(
    pipeline_get_names(pipeline),
    ~ pipeline_get_target(pipeline, .x)$settings$priority,
    USE.NAMES = TRUE
  )
}

pipeline_uses_priorities <- function(pipeline) {
  any(length(unique(pipeline_get_priorities(pipeline))) > 1L)
}

pipeline_reset_priorities <- function(pipeline) {
  map(pipeline_get_names(pipeline), ~ pipeline_reset_priority(pipeline, .x))
}

pipeline_reset_priority <- function(pipeline, name) {
  target <- pipeline_get_target(pipeline, name)
  target$settings$priority <- 0
}

pipeline_reset_deployments <- function(pipeline) {
  map(pipeline_get_names(pipeline), ~ pipeline_reset_deployment(pipeline, .x))
}

pipeline_reset_deployment <- function(pipeline, name) {
  target <- pipeline_get_target(pipeline, name)
  target$settings$deployment <- "main"
}

pipeline_exists_target <- function(pipeline, name) {
  envir <- .subset2(pipeline, "targets")
  if (is.null(envir)) {
    return(FALSE)
  }
  !is.null(.subset2(envir, name))
}

pipeline_targets_only_edges <- function(edges) {
  edges[edges$from %in% edges$to, , drop = FALSE] # nolint
}

pipeline_upstream_edges <- function(pipeline, targets_only = TRUE) {
  edge_list <- map(
    pipeline_get_names(pipeline),
    ~ target_upstream_edges(pipeline_get_target(pipeline, .x))
  )
  from <- map(edge_list, ~ .x$from)
  to <- map(edge_list, ~ .x$to)
  from <- unlist(from, recursive = FALSE, use.names = FALSE)
  to <- unlist(to, recursive = FALSE, use.names = FALSE)
  edges <- data_frame(from = from, to = to)
  edges <- if_any(targets_only, pipeline_targets_only_edges(edges), edges)
  if (ncol(edges) < 2L) {
    edges <- data_frame(from = character(0), to = character(0))
  }
  rownames(edges) <- NULL
  edges
}

pipeline_produce_igraph <- function(pipeline, targets_only = TRUE) {
  edges <- pipeline_upstream_edges(pipeline, targets_only = targets_only)
  igraph::simplify(igraph::graph_from_data_frame(edges))
}

pipeline_register_loaded <- function(pipeline, name) {
  # nolint
  counter_set_name(pipeline$loaded, name)
  target <- pipeline_get_target(pipeline, name)
  if (identical(target$settings$memory, "transient")) {
    counter_set_name(pipeline$transient, name)
  }
}

pipeline_unload_target <- function(pipeline, name) {
  target <- .subset2(.subset2(pipeline, "targets"), name)
  if (!is_reference_not_target(target)) {
    store_unload(target$store, target)
    pipeline_set_reference(pipeline, target)
  }
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

pipeline_produce_subpipeline <- function(
  pipeline,
  target,
  keep_value = NULL
) {
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
  subpipeline <- pipeline_new(
    targets = targets,
    loaded = counter_init(),
    transient = counter_init()
  )
  pipeline_compress_subpipeline(subpipeline)
  subpipeline
}

pipeline_assign_target_copy <- function(pipeline, name, envir, keep_value) {
  target <- pipeline_get_target(pipeline, name)
  copy <- target_subpipeline_copy(target, keep_value)
  envir[[name]] <- copy
}

pipeline_compress_subpipeline <- function(pipeline) {
  for (name in pipeline_get_names(pipeline)) {
    target <- pipeline_get_target(pipeline, name)
    null_value <- is.null(.subset2(target, "value"))
    has_parent <- pipeline_exists_target(pipeline, target_get_parent(target))
    if (null_value && has_parent) {
      pipeline_set_reference(pipeline, target)
    }
  }
}

pipeline_marshal_values <- function(pipeline) {
  map(
    pipeline_get_names(pipeline),
    ~ target_marshal_value(pipeline_get_target(pipeline, .x))
  )
}

pipeline_unmarshal_values <- function(pipeline) {
  names <- pipeline_get_names(pipeline)
  map(
    names,
    ~ target_unmarshal_value(pipeline_get_target(pipeline, .x))
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
  remove(
    list = discard,
    envir = .subset2(pipeline, "targets"),
    inherits = FALSE
  )
}

pipeline_prune_shortcut <- function(pipeline, names, shortcut) {
  if (is.null(names) || !shortcut) {
    return(pipeline)
  }
  available <- intersect(names, pipeline_get_names(pipeline))
  targets <- map(available, ~ pipeline_get_target(pipeline, .x))
  pipeline_init(targets = targets, clone_targets = FALSE)
}

pipeline_get_packages <- function(pipeline) {
  out <- map(
    pipeline_get_names(pipeline),
    ~ target_get_packages(pipeline_get_target(pipeline, .x))
  )
  sort_chr(unique(unlist(out)))
}

pipeline_bootstrap_deps <- function(pipeline, meta, names) {
  deps <- map(names, ~ pipeline_get_target(pipeline, .x)$deps)
  deps <- intersect(unique(unlist(deps)), pipeline_get_names(pipeline))
  deps <- setdiff(x = deps, y = names)
  branched_over <- map(
    names,
    ~ pipeline_get_target(pipeline, .x)$settings$dimensions
  )
  branched_over <- intersect(
    unique(unlist(branched_over)),
    pipeline_get_names(pipeline)
  )
  branched_over <- setdiff(x = branched_over, y = names)
  not_branched_over <- setdiff(deps, branched_over)
  map(
    not_branched_over,
    ~ target_bootstrap(
      pipeline_get_target(pipeline, .x),
      pipeline,
      meta,
      branched_over = FALSE
    )
  )
  map(
    branched_over,
    ~ target_bootstrap(
      pipeline_get_target(pipeline, .x),
      pipeline,
      meta,
      branched_over = TRUE
    )
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
  conflicts <- intersect(
    names(.subset2(pipeline, "imports")),
    pipeline_get_names(pipeline)
  )
  msg <- paste0(
    "Targets and globals must have unique names. ",
    "Ignoring global objects that conflict with target names: ",
    paste(conflicts, collapse = ", "),
    ". Warnings like this one are important, but if you must suppress them, ",
    "you can do so with Sys.setenv(TAR_WARN = \"false\")."
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

pipeline_validate_lite <- function(pipeline) {
  tar_assert_inherits(pipeline, "tar_pipeline", msg = "invalid pipeline.")
  tar_assert_correct_fields(pipeline, pipeline_new)
  tar_assert_target_name_case(pipeline_get_names(pipeline))
  pipeline_validate_conflicts(pipeline)
}

pipeline_from_list <- function(x) {
  UseMethod("pipeline_from_list")
}

#' @export
#' @keywords internal
pipeline_from_list.tar_pipeline <- function(x) {
  x
}

#' @export
#' @keywords internal
pipeline_from_list.default <- function(x) {
  out <- unlist(list(x), recursive = TRUE)
  out <- fltr(out, ~ inherits(x = .x, what = "tar_target"))
  pipeline_init(out)
}

#' @export
#' @keywords internal
print.tar_pipeline <- function(x, ...) {
  count <- length(pipeline_get_names(x))
  msg <- paste("<tar_pipeline>\n  targets:", count)
  cat(msg)
}
