target_init <- function(
  name = character(0),
  expr = NULL,
  packages = (.packages()),
  library = NULL,
  deps = NULL,
  string = NULL,
  envir = NULL,
  format = "rds",
  pattern = NULL,
  iteration = "vector",
  error = "stop",
  memory = "persistent",
  deployment = "remote",
  priority = 0,
  template = NULL,
  resources = list(),
  storage = "local",
  retrieval = storage,
  cue = NULL
) {
  force(envir)
  envir <- envir %||% target_empty_envir
  seed <- produce_seed(name)
  command <- command_init(expr, packages, library, seed, deps, string)
  settings <- settings_init(
    name = name,
    format = format,
    pattern = pattern,
    iteration = iteration,
    error = error,
    memory = memory,
    deployment = deployment,
    priority = priority,
    template = template,
    resources = resources,
    storage = storage,
    retrieval = retrieval
  )
  cue <- cue %||% cue_default
  cache <- cache_init(memory_init(envir))
  command$deps <- unique(c(command$deps, settings$dimensions))
  switch(
    settings$growth,
    none = stem_new(
      command,
      settings,
      cue,
      cache,
      store = settings_produce_store(settings)
    ),
    map = map_new(
      command,
      settings,
      cue,
      cache,
      patternview = patternview_init()
    ),
    cross = cross_new(
      command,
      settings,
      cue,
      cache,
      patternview = patternview_init()
    ),
    throw_validate("unsupported pattern")
  )
}

target_new <- function(
  command = NULL,
  settings = NULL,
  cue = NULL,
  cache = NULL,
  value = NULL
) {
  force(command)
  force(settings)
  force(cue)
  force(cache)
  force(value)
  enclass(environment(), "tar_target")
}

#' @title Internal infrastructure function.
#' @export
#' @keywords internal
#' @description Not a user-side function.
#'   Only use for developing external HPC backend packages.
#' @param target Target object.
target_get_name <- function(target) {
  target$settings$name
}

target_load_dep <- function(target, dep, pipeline) {
  target_ensure_value(dep, pipeline)
  object <- dep$value$object
  cache_set_object(target$cache, target_get_parent(dep), object)
}

target_load_deps <- function(target, pipeline) {
  map(
    target_deps_shallow(target, pipeline),
    ~target_load_dep(target, pipeline_get_target(pipeline, .x), pipeline)
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

#' @title Internal infrastructure function.
#' @export
#' @keywords internal
#' @description Not a user-side function.
#'   Only use for developing external HPC backend packages.
#' @param target Target object.
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
  type <- target_get_type(target)
  ifelse(type == "stem", "target", type)
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

#' @title Internal infrastructure function.
#' @export
#' @keywords internal
#' @description Not a user-side function.
#'   Only use for developing external HPC backend packages.
#' @param target Target object.
#' @param pipeline Pipeline object.
target_read_value <- function(target, pipeline) {
  UseMethod("target_read_value")
}

target_produce_record <- function(target, meta) {
  UseMethod("target_produce_record")
}

#' @title Internal infrastructure function.
#' @export
#' @keywords internal
#' @description Not a user-side function.
#'   Only use for developing external HPC backend packages.
#' @param target Target object.
#' @param pipeline Pipeline object.
#' @param scheduler Scheduler object.
#' @param meta Meta object.
target_skip <- function(target, pipeline, scheduler, meta) {
  UseMethod("target_skip")
}

#' @title Internal infrastructure function.
#' @export
#' @keywords internal
#' @description Not a user-side function.
#'   Only use for developing external HPC backend packages.
#' @param target Target object.
#' @param pipeline Pipeline object.
#' @param scheduler Scheduler object.
target_prepare <- function(target, pipeline, scheduler) {
  UseMethod("target_prepare")
}

#' @export
target_prepare.default <- function(target, pipeline, scheduler) {
}

#' @title Internal infrastructure function.
#' @export
#' @keywords internal
#' @description Not a user-side function.
#'   Only use for developing external HPC backend packages.
#' @param target Target object.
#' @param meta Meta object.
target_should_run <- function(target, meta) {
  UseMethod("target_should_run")
}

#' @export
target_should_run.default <- function(target, meta) {
  TRUE
}

#' @title Internal infrastructure function.
#' @export
#' @keywords internal
#' @description Not a user-side function.
#'   Only use for developing external HPC backend packages.
target_should_run_remote <- function(target) {
  UseMethod("target_should_run_remote")
}

#' @export
target_should_run_remote.default <- function(target) {
  FALSE
}

#' @title Internal infrastructure function.
#' @export
#' @keywords internal
#' @description Not a user-side function.
#'   Only use for developing external HPC backend packages.
#' @param target Target object.
target_run <- function(target) {
  UseMethod("target_run")
}

#' @export
target_run.default <- function(target) {
}

#' @title Internal function to run a target on a remote worker.
#' @export
#' @keywords internal
#' @description For internal purposes only. Not a user-side function.
#' @param target A target object.
#' @param garbage_collection Logical, whether to run garbage collection.
target_run_remote <- function(target, garbage_collection) {
  UseMethod("target_run_remote")
}

#' @export
target_run_remote.default <- function(target, garbage_collection) {
}

#' @title Internal infrastructure function.
#' @export
#' @keywords internal
#' @description Not a user-side function.
#'   Only use for developing external HPC backend packages.
#' @param target Target object.
#' @param pipeline Pipeline object.
#' @param scheduler Scheduler object.
#' @param meta Meta object.
target_conclude <- function(target, pipeline, scheduler, meta) {
  UseMethod("target_conclude")
}

#' @export
target_conclude.tar_target <- function(target, pipeline, scheduler, meta) {
  cache_clear_objects(target$cache)
}

target_ensure_buds <- function(target, pipeline, scheduler) {
  UseMethod("target_ensure_buds")
}

target_restore_buds <- function(target, pipeline, scheduler, meta) {
  UseMethod("target_restore_buds")
}

#' @title Internal infrastructure function.
#' @export
#' @keywords internal
#' @description Not a user-side function.
#'   Only use for developing external HPC backend packages.
#' @param target Target object.
#' @param meta Meta object.
target_update_depend <- function(target, meta) {
  UseMethod("target_update_depend")
}

target_is_branchable <- function(target) {
  UseMethod("target_is_branchable")
}

target_is_branchable.default <- function(target) {
  FALSE
}

target_patternview_meta <- function(target, pipeline, meta) {
  UseMethod("target_patternview_meta")
}

#' @export
target_patternview_meta.default <- function(target, pipeline, meta) {
}

target_patternview_cancelled <- function(target, pipeline, scheduler) {
  UseMethod("target_patternview_cancelled")
}

#' @export
target_patternview_cancelled.default <- function(target, pipeline, scheduler) {
}

target_patternview_errored <- function(target, pipeline, scheduler) {
  UseMethod("target_patternview_errored")
}

#' @export
target_patternview_errored.default <- function(target, pipeline, scheduler) {
}

#' @title Internal infrastructure function.
#' @export
#' @keywords internal
#' @description Not a user-side function.
#'   Only use for developing external HPC backend packages.
#' @param target Target object.
target_debug <- function(target) {
  UseMethod("target_debug")
}

#' @export
target_debug.default <- function(target) {
}

target_validate <- function(target) {
  UseMethod("target_validate")
}

#' @export
target_validate.tar_target <- function(target) {
  command_validate(target$command)
  settings_validate(target$settings)
  target_validate_deps(target)
  if (!is.null(target$cue)) {
    cue_validate(target$cue)
  }
  if (!is.null(target$cache)) {
    cache_validate(target$cache)
  }
  if (!is.null(target$value)) {
    value_validate(target$value)
  }
}

target_validate_deps <- function(target) {
  if (target_get_name(target) %in% target$command$deps) {
    throw_validate("target ", target_get_name(target), " depends on itself.")
  }
}

target_empty_envir <- new.env(parent = baseenv())
