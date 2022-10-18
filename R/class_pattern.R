pattern_new <- function(
  command = NULL,
  settings = NULL,
  cue = NULL,
  value = NULL,
  junction = NULL,
  patternview = NULL
) {
  force(command)
  force(settings)
  force(cue)
  force(value)
  force(junction)
  force(patternview)
  enclass(environment(), c("tar_pattern", "tar_target"))
}

#' @export
target_get_children.tar_pattern <- function(target) {
  target$junction$splits
}

#' @export
target_produce_record.tar_pattern <- function(target, pipeline, meta) {
  record_init(
    name = target_get_name(target),
    type = target_get_type(target),
    data = pattern_produce_data_hash(target, pipeline, meta),
    command = target$command$hash,
    seed = target$command$seed,
    bytes = target$patternview$bytes,
    format = target$settings$format,
    repository = target$settings$repository,
    iteration = target$settings$iteration,
    children = target_get_children(target),
    seconds = target$patternview$seconds
  )
}

#' @export
target_should_run.tar_pattern <- function(target, meta) {
  TRUE
}

#' @export
target_skip.tar_pattern <- function(
  target,
  pipeline,
  scheduler,
  meta,
  active
) {
  if_any(
    is.null(target$junction),
    pattern_begin_initial(target, pipeline, scheduler, meta),
    pattern_begin_final(target, pipeline, scheduler, meta)
  )
}

#' @export
target_conclude.tar_pattern <- function(target, pipeline, scheduler, meta) {
  if_any(
    is.null(target$junction),
    pattern_conclude_initial(target, pipeline, scheduler, meta),
    pattern_conclude_final(target, pipeline, scheduler, meta)
  )
}

#' @export
target_read_value.tar_pattern <- function(target, pipeline) {
  branches <- target_get_children(target)
  map(
    branches,
    ~target_ensure_value(pipeline_get_target(pipeline, .x), pipeline)
  )
  objects <- map(
    branches,
    ~pipeline_get_target(pipeline, .x)$value$object
  )
  names(objects) <- branches
  value <- value_init(iteration = target$settings$iteration)
  value$object <- value_produce_aggregate(value, objects)
  value
}

#' @export
target_branches_over.tar_pattern <- function(target, name) {
  name %in% target$settings$dimensions
}

#' @export
target_update_depend.tar_pattern <- function(target, pipeline, meta) {
  depends <- meta$depends
  memory_set_object(depends, target_get_name(target), null64)
}

#' @export
target_is_branchable.tar_pattern <- function(target) {
  TRUE
}

#' @export
target_produce_junction.tar_pattern <- function(target, pipeline) {
  dimensions <- target$settings$dimensions
  pattern_tar_assert_dimensions(target, dimensions, pipeline)
  siblings <- setdiff(target_deps_shallow(target, pipeline), dimensions)
  niblings <- pattern_children_columns(dimensions, pipeline)
  pattern <- target$settings$pattern
  niblings <- pattern_produce_grid(pattern, niblings, target$command$seed)
  all_deps <- pattern_combine_niblings_siblings(niblings, siblings)
  nibling_deps <- all_deps[, dimensions, drop = FALSE]
  names <- pattern_name_branches(target_get_parent(target), nibling_deps)
  junction_init(target_get_parent(target), names, all_deps)
}

#' @export
target_get_type.tar_pattern <- function(target) {
  "pattern"
}

#' @export
target_needs_worker.tar_pattern <- function(target) {
  identical(target$settings$deployment, "worker") &&
    is.null(target$junction)
}

#' @export
target_validate.tar_pattern <- function(target) {
  tar_assert_correct_fields(target, pattern_new)
  if (!is.null(target$junction)) {
    junction_validate(target$junction)
  }
  NextMethod()
}

#' @export
target_bootstrap.tar_pattern <- function(
  target,
  pipeline,
  meta,
  branched_over = FALSE
) {
  record <- target_bootstrap_record(target, meta)
  name <- target$settings$name
  children <- record$children
  target$junction <- junction_init(nexus = name, splits = children)
  branches <- pattern_produce_branches(target, pipeline)
  lapply(branches, pipeline_set_target, pipeline = pipeline)
  map(
    children,
    ~target_bootstrap(
      pipeline_get_target(pipeline, .x),
      pipeline,
      meta,
      branched_over = FALSE
    )
  )
  invisible()
}

#' @export
print.tar_pattern <- function(x, ...) {
  cat(
    "<tar_pattern>",
    "\n  name:", target_get_name(x),
    "\n  command:\n   ",
    produce_lines(string_sub_expression(x$command$string)),
    "\n  pattern:\n   ",
    produce_lines(string_sub_expression(tar_deparse_safe(x$settings$pattern))),
    "\n  format:", x$settings$format,
    "\n  repository:", x$settings$repository,
    "\n  iteration method:", x$settings$iteration,
    "\n  error mode:", x$settings$error,
    "\n  memory mode:", x$settings$memory,
    "\n  storage mode:", x$settings$storage,
    "\n  retrieval mode:", x$settings$retrieval,
    "\n  deployment mode:", x$settings$deployment,
    "\n  priority:", x$settings$priority,
    "\n  resources:\n   ",
    produce_lines(paste_list(x$settings$resources)),
    "\n  cue:\n   ",
    produce_lines(paste_list(as.list(x$cue))),
    "\n  packages:\n   ", produce_lines(x$command$packages),
    "\n  library:\n   ", produce_lines(x$command$library)
  )
}

pattern_update_junction <- function(pattern, pipeline) {
  pattern$junction <- target_produce_junction(pattern, pipeline)
}

pattern_engraph_branches <- function(target, pipeline, scheduler) {
  scheduler$queue$engraph_branches(target, pipeline, scheduler)
}

pattern_prepend_branches <- function(target, scheduler) {
  children <- target_get_children(target)
  ranks <- scheduler$queue$branch_ranks(children, scheduler)
  ranks <- ranks + rank_offset(target$settings$priority)
  scheduler$queue$prepend(children, ranks)
}

pattern_produce_branch <- function(spec, command, settings, cue) {
  branch_init(
    command = command,
    settings = settings,
    cue = cue,
    deps = spec$deps,
    child = spec$split,
    index = spec$index
  )
}

pattern_produce_branches <- function(target, pipeline) {
  map(
    junction_transpose(target$junction),
    pattern_produce_branch,
    command = target$command,
    settings = target$settings,
    cue = target$cue
  )
}

pattern_insert_branches <- function(target, pipeline, scheduler) {
  branches <- pattern_produce_branches(target, pipeline)
  lapply(branches, pipeline_set_target, pipeline = pipeline)
  pattern_engraph_branches(target, pipeline, scheduler)
  lapply(branches, scheduler$progress$assign_queued)
  pattern_prepend_branches(target, scheduler)
}

pattern_requeue_downstream_branching <- function(
  target,
  pipeline,
  scheduler
) {
  names <- target_downstream_branching(target, pipeline, scheduler)
  scheduler$queue$increment_ranks(names, by = -1L)
}

pattern_requeue_downstream_nonbranching <- function(
  target,
  pipeline,
  scheduler
) {
  names <- target_downstream_nonbranching(target, pipeline, scheduler)
  scheduler$queue$increment_ranks(names, by = -1L)
}

pattern_requeue_self <- function(target, scheduler) {
  rank <- length(target_get_children(target)) + rank_offset(pattern_priority())
  scheduler$queue$prepend(target_get_name(target), ranks = rank)
}

pattern_priority <- function() {
  1.1
}

pattern_produce_data_hash <- function(target, pipeline, meta) {
  hash_branches <- meta$hash_deps(target_get_children(target), pipeline)
  digest_chr64(paste(target$settings$iteration, hash_branches))
}

pattern_conclude_initial <- function(target, pipeline, scheduler, meta) {
  pattern_begin_initial(target, pipeline, scheduler, meta)
  pattern_debug_branches(target)
}

pattern_conclude_final <- function(target, pipeline, scheduler, meta) {
  pattern_begin_final(target, pipeline, scheduler, meta)
  pattern_record_meta(target, pipeline, meta)
  patternview_register_final(target$patternview, target, scheduler)
  if (identical(target$patternview$progress, "built")) {
    scheduler$reporter$report_built(target, scheduler$progress)
  } else if (identical(target$patternview$progress, "skipped")) {
    scheduler$reporter$report_skipped(target, scheduler$progress)
  }
}

pattern_begin_initial <- function(target, pipeline, scheduler, meta) {
  pattern_update_junction(target, pipeline)
  pattern_requeue_downstream_branching(target, pipeline, scheduler)
  pattern_requeue_self(target, scheduler)
  pattern_insert_branches(target, pipeline, scheduler)
}

pattern_begin_final <- function(target, pipeline, scheduler, meta) {
  scheduler$progress$assign_dequeued(target)
  pattern_requeue_downstream_nonbranching(target, pipeline, scheduler)
}

pattern_tar_assert_dimensions <- function(target, dimensions, pipeline) {
  for (name in dimensions) {
    pipeline_tar_assert_dimension(target, pipeline, name)
  }
}

pipeline_tar_assert_dimension <- function(target, pipeline, name) {
  branchable <- FALSE
  if (pipeline_exists_target(pipeline, name)) {
    dep <- pipeline_get_target(pipeline, name)
    branchable <- target_is_branchable(dep)
  }
  if (!branchable) {
    tar_throw_validate(
      "Target ", target_get_name(target),
      " tried to branch over ", name, ", which is illegal. ",
      "Patterns must only branch over explicitly ",
      "declared targets in the pipeline. ",
      "Stems and patterns are fine, but you cannot branch over branches or ",
      "global objects. Also, if you branch over a target with ",
      "format = \"file\", then that target must also be a pattern."
    )
  }
}

pattern_record_meta <- function(target, pipeline, meta) {
  name <- target_get_name(target)
  old_data <- if_any(
    meta$exists_record(name),
    meta$get_record(name)$data,
    NA_character_
  )
  record <- target_produce_record(target, pipeline, meta)
  if (!identical(record$data, old_data)) {
    meta$insert_record(record)
  }
}

pattern_debug_branches <- function(target) {
  debug <- tar_option_get("debug")
  if (length(debug) && target_get_name(target) %in% debug) {
    # Covered in tests/interactive/test-debug.R
    # nocov start
    tar_option_set(debug = c(debug, target_get_children(target)))
    # nocov end
  }
}

pattern_children_columns <- function(dimensions, pipeline) {
  out <- map(dimensions, ~pattern_children_column(.x, pipeline))
  names(out) <- dimensions
  out
}

pattern_children_column <- function(name, pipeline) {
  niblings <- target_get_children(pipeline_get_target(pipeline, name))
  out <- data_frame(x = niblings)
  names(out) <- name
  out
}

pattern_combine_niblings_siblings <- function(niblings, siblings) {
  out <- as_data_frame(niblings)
  for (name in siblings) {
    out[[name]] <- name
  }
  out
}

pattern_name_branches <- function(parent, niblings) {
  suffixes <- digest_chr32(do.call(paste, niblings))
  paste0(parent, "_", suffixes)
}

pattern_produce_grid <- function(
  pattern,
  niblings,
  seed,
  methods = dynamic_methods
) {
  out <- if_any(
    anyNA(seed),
    eval(pattern, envir = niblings, enclos = dynamic_methods$self),
    withr::with_seed(
      seed,
      eval(pattern, envir = niblings, enclos = dynamic_methods$self)
    )
  )
  rownames(out) <- NULL
  out
}
