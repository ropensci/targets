target_init <- function(
  name = character(0),
  expr = NULL,
  packages = (.packages()),
  library = NULL,
  deps = NULL,
  string = NULL,
  format = "rds",
  repository = "local",
  pattern = NULL,
  iteration = "vector",
  error = "stop",
  memory = "auto",
  garbage_collection = FALSE,
  deployment = "worker",
  priority = 0,
  resources = list(),
  storage = "main",
  retrieval = "main",
  cue = NULL,
  description = character(0L)
) {
  seed <- tar_seed_create(name)
  deps <- deps %|||% deps_function(embody_expr(expr))
  command <- command_init(expr, packages, library, string)
  cue <- cue %|||% cue_init()
  if (any(grepl("^aws_", format))) {
    format <- gsub("^aws_", "", format)
    repository <- "aws"
  }
  if (identical(format, "url")) {
    repository <- "local"
  }
  if (store_format_custom_old_repository(format) == "aws") {
    repository <- "aws"
  }
  settings <- settings_init(
    name = name,
    description = description,
    format = format,
    repository = repository,
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
  deps <- unique(c(deps, settings$dimensions))
  deps <- setdiff(deps, name)
  if_any(
    is.null(settings$pattern),
    stem_init(
      name = name,
      command = command,
      seed = seed,
      deps = deps,
      settings = settings,
      cue = cue
    ),
    pattern_init(
      name = name,
      command = command,
      seed = seed,
      deps = deps,
      settings = settings,
      cue = cue
    )
  )
}

target_new <- function(
  command = NULL,
  settings = NULL,
  cue = NULL
) {
  out <- new.env(parent = emptyenv(), hash = FALSE)
  out$command <- command
  out$settings <- settings
  out$cue <- cue
  enclass(out, target_s3_class)
}

target_s3_class <- "tar_target"

target_get_name <- function(target) {
  .subset2(target, "name")
}

target_ensure_dep <- function(target, dep, pipeline) {
  tryCatch(
    target_ensure_value(dep, pipeline),
    error = function(error) {
      message <- paste0(
        "could not load dependency ",
        target_get_name(dep),
        " of target ",
        target_get_name(target),
        ". ",
        conditionMessage(error)
      )
      expr <- as.expression(as.call(list(quote(stop), message)))
      target$command$expr <- expr
      target$settings$deployment <- "main"
    }
  )
}

target_ensure_deps_worker <- function(target, pipeline) {
  map(
    target_deps_shallow(target, pipeline),
    ~target_ensure_dep(target, pipeline_get_target(pipeline, .x), pipeline)
  )
}

target_ensure_deps_main <- function(target, pipeline) {
  for (name in target_deps_shallow(target, pipeline)) {
    dep <- pipeline_get_target(pipeline, name)
    if (inherits(dep, "tar_pattern")) {
      map(
        target_get_children(dep),
        ~target_ensure_dep(target, pipeline_get_target(pipeline, .x), pipeline)
      )
    } else {
      target_ensure_dep(target, dep, pipeline)
    }
  }
}

target_value_null <- function(target) {
  value_init(
    object = NULL,
    iteration = target$settings$iteration
  )
}

target_load_value <- function(target, pipeline) {
  target$value <- tryCatch(
    target_read_value(target, pipeline),
    error = function(condition) {
      if_any(
        identical(target$settings$error, "null"),
        target_value_null(target),
        tar_throw_run(conditionMessage(condition))
      )
    }
  )
  pipeline_set_target(pipeline, target)
  pipeline_register_loaded(pipeline, target_get_name(target))
}

target_ensure_value <- function(target, pipeline) {
  if (is.null(target$value)) {
    target_load_value(target, pipeline)
  }
}

target_deps_shallow <- function(target, pipeline) {
  fltr(target$deps, ~pipeline_exists_target(pipeline, .x))
}

target_deps_deep <- function(target, pipeline) {
  deps <- target_deps_shallow(target, pipeline)
  retrieval_worker <- identical(target$settings$retrieval, "worker")
  extras <- map(
    deps,
    ~target_worker_extras(
      target = pipeline_get_target(pipeline, .x),
      pipeline = pipeline,
      retrieval_worker = retrieval_worker
    )
  )
  unique(as.character(c(deps, unlist(extras))))
}

target_worker_extras <- function(target, pipeline, retrieval_worker) {
  UseMethod("target_worker_extras")
}

#' @export
target_worker_extras.tar_target <- function(
  target,
  pipeline,
  retrieval_worker
) {
  character(0L)
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
  from <- c(name, target$deps)
  to <- rep(name, length(from))
  list(from = from, to = to)
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
  .subset2(.subset2(target, "settings"), "name")
}

target_get_children <- function(target) {
  UseMethod("target_get_children")
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

target_skip <- function(
  target,
  pipeline,
  scheduler,
  meta,
  active
) {
  UseMethod("target_skip")
}

target_prepare <- function(
  target,
  pipeline,
  scheduler,
  meta,
  pending = FALSE
) {
  UseMethod("target_prepare")
}

#' @export
target_prepare.default <- function(
  target,
  pipeline,
  scheduler,
  meta,
  pending = FALSE
) {
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

target_run <- function(target, envir, path_store, on_worker = FALSE) {
  UseMethod("target_run")
}

#' @export
target_run.default <- function(target, envir, path_store, on_worker = FALSE) {
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
  } else {
    count <- .subset2(tar_runtime, "number_targets_run") %|||% 0L
    interval <- .subset2(tar_options, "get_garbage_collection")()
    if (interval > 0L && count > 0L && (count %% interval) == 0L) {
      gc()
    }
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

target_bootstrap <- function(target, pipeline, meta, branched_over) {
  UseMethod("target_bootstrap")
}

target_bootstrap_record <- function(target, meta) {
  name <- target_get_name(target)
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

target_resolve_auto <- function(target, setting, value) {
  settings <- .subset2(target, "settings")
  if (.subset2(settings, setting) == "auto") {
    settings[[setting]] <- value
  }
}

target_patternview_meta <- function(target, pipeline, meta) {
  UseMethod("target_patternview_meta")
}

#' @export
target_patternview_meta.default <- function(target, pipeline, meta) {
}

target_patternview_dispatched <- function(target, pipeline, scheduler) {
  UseMethod("target_patternview_dispatched")
}

#' @export
target_patternview_dispatched.default <- function(
  target,
  pipeline,
  scheduler
) {
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

target_marshal_value <- function(target) {
  UseMethod("target_marshal_value")
}

#' @export
target_marshal_value.default <- function(target) {
  if (!is.null(target$value)) {
    store_marshal_value(target$store, target)
  }
}

target_unmarshal_value <- function(target) {
  UseMethod("target_unmarshal_value")
}

#' @export
target_unmarshal_value.default <- function(target) {
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
  sort_chr(unique(c(packages_command, packages_store)))
}

target_allow_meta <- function(target) {
  settings <- .subset2(target, "settings")
  format <- .subset2(settings, "format")
  repository <- .subset2(settings, "repository")
  (format == "file" || format == "file_fast") && (repository == "local")
}

target_reformat <- function(target, format) {
  target$settings <- settings_clone(.subset2(target, "settings"))
  target$settings$format <- format
  target$store <- settings_produce_store(.subset2(target, "settings"))
}

target_validate <- function(target) {
  UseMethod("target_validate")
}

target_produce_child <- function(target, name, index) {
  UseMethod("target_produce_child")
}

target_produce_reference <- function(target) {
  UseMethod("target_produce_reference")
}

#' @export
target_produce_reference.default <- function(target) {
  target
}

#' @export
target_validate.tar_target <- function(target) {
  tar_assert_chr(target$name)
  tar_assert_scalar(target$name)
  tar_assert_nzchar(target$name)
  settings_validate(target$settings)
  if (!is.null(target$cue)) {
    cue_validate(target$cue)
  }
  if (!is.null(target$value)) {
    value_validate(target$value)
  }
}
