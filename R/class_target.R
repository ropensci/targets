target_init <- function(
  name = character(0),
  expr = NULL,
  packages = (.packages()),
  library = NULL,
  deps = NULL,
  string = NULL,
  format = "rds",
  pattern = NULL,
  iteration = "vector",
  error = "stop",
  memory = "persistent",
  garbage_collection = FALSE,
  deployment = "worker",
  priority = 0,
  resources = list(),
  storage = "main",
  retrieval = "main",
  cue = NULL
) {
  seed <- produce_seed(name)
  command <- command_init(expr, packages, library, seed, deps, string)
  cue <- cue %|||% cue_init()
  settings <- settings_init(
    name = name,
    format = format,
    pattern = pattern,
    iteration = iteration,
    error = error,
    memory = memory,
    garbage_collection = garbage_collection,
    deployment = deployment,
    priority = priority,
    resources = resources,
    storage = storage,
    retrieval = retrieval
  )
  command$deps <- unique(c(command$deps, settings$dimensions))
  command$deps <- setdiff(command$deps, name)
  if_any(
    is.null(settings$pattern),
    stem_new(
      command,
      settings,
      cue,
      store = settings_produce_store(settings)
    ),
    pattern_new(
      command,
      settings,
      cue,
      patternview = patternview_init()
    )
  )
}

target_new <- function(
  command = NULL,
  settings = NULL,
  cue = NULL,
  value = NULL
) {
  force(command)
  force(settings)
  force(cue)
  force(value)
  enclass(environment(), "tar_target")
}

target_get_name <- function(target) {
  target$settings$name
}

target_ensure_dep <- function(target, dep, pipeline) {
  target_ensure_value(dep, pipeline)
}

target_ensure_deps <- function(target, pipeline) {
  map(
    target_deps_shallow(target, pipeline),
    ~target_ensure_dep(target, pipeline_get_target(pipeline, .x), pipeline)
  )
}

target_load_value <- function(target, pipeline) {
  target$value <- target_read_value(target, pipeline)
  pipeline_register_loaded(pipeline, target_get_name(target))
}

target_ensure_value <- function(target, pipeline) {
  if (is.null(target$value)) {
    target_load_value(target, pipeline)
  }
}

target_deps_shallow <- function(target, pipeline) {
  fltr(target$command$deps, ~pipeline_exists_target(pipeline, .x))
}

target_deps_deep <- function(target, pipeline) {
  deps <- target_deps_shallow(target, pipeline)
  children <- unlist(
    lapply(deps, function(dep) {
      target_get_children(pipeline_get_target(pipeline, dep))
    })
  )
  parents <- map_chr(deps, function(dep) {
    target_get_parent(pipeline_get_target(pipeline, dep))
  })
  unique(c(deps, children, parents))
}

target_downstream_branching <- function(target, pipeline, scheduler) {
  name <- target_get_name(target)
  names <- scheduler$graph$produce_downstream(name)
  fltr(names, ~target_branches_over(pipeline_get_target(pipeline, .x), name))
}

target_downstream_nonbranching <- function(target, pipeline, scheduler) {
  name <- target_get_name(target)
  names <- scheduler$graph$produce_downstream(name)
  fltr(names, ~!target_branches_over(pipeline_get_target(pipeline, .x), name))
}

target_upstream_edges <- function(target) {
  name <- target_get_name(target)
  from <- c(name, target$command$deps)
  to <- rep(name, length(from))
  data_frame(from = from, to = to)
}

target_update_queue <- function(target, scheduler) {
  scheduler$queue$update_ranks(target, scheduler)
}

target_downstream_names <- function(target, scheduler) {
  scheduler$graph$produce_downstream(target_get_name(target))
}

target_decrement_ranks <- function(names, scheduler) {
  scheduler$queue$increment_ranks(names, -1L)
}

target_get_parent <- function(target) {
  UseMethod("target_get_parent")
}

#' @export
target_get_parent.default <- function(target) {
  target_get_name(target)
}

target_get_children <- function(target) {
  UseMethod("target_get_children")
}

#' @export
target_get_children.default <- function(target) {
  character(0)
}

target_get_type <- function(target) {
  UseMethod("target_get_type")
}

target_get_type_cli <- function(target) {
  UseMethod("target_get_type_cli")
}

#' @export
target_get_type_cli.default <- function(target) {
  target_get_type(target)
}

target_branches_over <- function(target, name) {
  UseMethod("target_branches_over")
}

#' @export
target_branches_over.default <- function(target, name) {
  FALSE
}

target_produce_junction <- function(target, pipeline) {
  UseMethod("target_produce_junction")
}

target_read_value <- function(target, pipeline) {
  UseMethod("target_read_value")
}

target_produce_record <- function(target, pipeline, meta) {
  UseMethod("target_produce_record")
}

target_skip <- function(target, pipeline, scheduler, meta, active) {
  UseMethod("target_skip")
}

target_prepare <- function(target, pipeline, scheduler) {
  UseMethod("target_prepare")
}

#' @export
target_prepare.default <- function(target, pipeline, scheduler) {
}

target_should_run <- function(target, meta) {
  UseMethod("target_should_run")
}

target_should_run_worker <- function(target) {
  UseMethod("target_should_run_worker")
}

#' @export
target_should_run_worker.default <- function(target) {
  FALSE
}

target_needs_worker <- function(target) {
  UseMethod("target_needs_worker")
}

#' @export
target_needs_worker.default <- function(target) {
  FALSE
}

target_run <- function(target, envir, path_store) {
  UseMethod("target_run")
}

#' @export
target_run.default <- function(target, envir, path_store) {
}

#' @title Internal function to run a target on a worker.
#' @export
#' @keywords internal
#' @description For internal purposes only. Not a user-side function.
#' @param target A target object.
#' @param envir An environment or the string `"globalenv"`.
#' @param path_store Character of length 1, path to the data store.
#' @param fun Character of length 1, name of the user-side function called
#'   to run the pipeline.
#' @param options List, exported from an object of class `"tar_options"`.
#' @param envvars Data frame of `targets`-specific environment variables
#'   from [tar_envvars()].
target_run_worker <- function(
  target,
  envir,
  path_store,
  fun,
  options,
  envvars
) {
  UseMethod("target_run_worker")
}

target_gc <- function(target) {
  if (target$settings$garbage_collection) {
    gc()
  }
}

target_conclude <- function(target, pipeline, scheduler, meta) {
  UseMethod("target_conclude")
}

#' @export
target_conclude.tar_target <- function(target, pipeline, scheduler, meta) {
}

target_ensure_buds <- function(target, pipeline, scheduler) {
  UseMethod("target_ensure_buds")
}

target_restore_buds <- function(target, pipeline, scheduler, meta) {
  UseMethod("target_restore_buds")
}

target_update_depend <- function(target, pipeline, meta) {
  UseMethod("target_update_depend")
}

target_is_branchable <- function(target) {
  UseMethod("target_is_branchable")
}

#' @export
target_is_branchable.default <- function(target) {
  FALSE
}

target_bootstrap <- function(target, pipeline, meta) {
  UseMethod("target_bootstrap")
}

target_bootstrap_record <- function(target, meta) {
  name <- target$settings$name
  if (!meta$exists_record(name)) {
    tar_throw_validate(
      "cannot bootstrap target ",
      name,
      " because there is no record of ",
      name,
      " the metadata. Run the pipeline with shortcut = FALSE to create it."
    )
  }
  meta$get_record(name)
}

target_subpipeline_copy <- function(target, keep_value) {
  class <- class(target)
  out <- list2env(as.list(target), parent = emptyenv(), hash = FALSE)
  class(out) <- class
  if (!is.null(out$metrics)) {
    out$metrics <- metrics_new()
  }
  if (!keep_value) {
    out$value <- NULL
  }
  # Eliminate high-memory promise objects:
  force(out$value$object)
  out
}

target_workspace_copy <- function(target) {
  class <- class(target)
  out <- list2env(as.list(target), parent = emptyenv(), hash = FALSE)
  class(out) <- class
  out$value <- NULL
  out
}

target_patternview_meta <- function(target, pipeline, meta) {
  UseMethod("target_patternview_meta")
}

#' @export
target_patternview_meta.default <- function(target, pipeline, meta) {
}

target_patternview_started <- function(target, pipeline, scheduler) {
  UseMethod("target_patternview_started")
}

#' @export
target_patternview_started.default <- function(target, pipeline, scheduler) {
}

target_patternview_canceled <- function(target, pipeline, scheduler) {
  UseMethod("target_patternview_canceled")
}

#' @export
target_patternview_canceled.default <- function(target, pipeline, scheduler) {
}

target_patternview_errored <- function(target, pipeline, scheduler) {
  UseMethod("target_patternview_errored")
}

#' @export
target_patternview_errored.default <- function(target, pipeline, scheduler) {
}

target_debug <- function(target) {
  UseMethod("target_debug")
}

#' @export
target_debug.default <- function(target) {
}

target_sync_file_meta <- function(target, meta) {
  UseMethod("target_sync_file_meta")
}

#' @export
target_sync_file_meta.default <- function(target, meta) {
}

target_marshal_value <- function(target) {
  if (!is.null(target$value)) {
    store_marshal_value(target$store, target)
  }
}

target_unmarshal_value <- function(target) {
  if (!is.null(target$value)) {
    store_unmarshal_value(target$store, target)
  }
}

target_get_packages <- function(target) {
  UseMethod("target_get_packages")
}

#' @export
target_get_packages.default <- function(target) {
  packages_command <- target$command$packages
  store <- settings_produce_store(target$settings)
  packages_store <- store_get_packages(store)
  sort(unique(c(packages_command, packages_store)))
}

target_validate <- function(target) {
  UseMethod("target_validate")
}

#' @export
target_validate.tar_target <- function(target) {
  command_validate(target$command)
  settings_validate(target$settings)
  if (!is.null(target$cue)) {
    cue_validate(target$cue)
  }
  if (!is.null(target$value)) {
    value_validate(target$value)
  }
}
