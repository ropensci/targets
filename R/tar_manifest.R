#' @title Produce a data frame of information about your targets.
#' @export
#' @family inspect
#' @description Along with [tar_visnetwork()] and [tar_glimpse()],
#'   `tar_manifest()` helps check that you constructed your pipeline correctly.
#' @return A data frame of information about the targets in the pipeline.
#'   Rows appear in topological order (the order they will run
#'   without any influence from parallel computing or priorities).
#' @inheritParams tar_validate
#' @param names Names of the targets to show. Set to `NULL` to
#'   show all the targets (default). Otherwise, you can supply
#'   symbols, a character vector, or `tidyselect` helpers like
#'   [all_of()] and [starts_with()].
#' @param fields Names of the fields, or columns, to show. Set to `NULL` to
#'   show all the fields (default). Otherwise, you can supply
#'   symbols, a character vector, or `tidyselect` helpers like [starts_with()].
#'   Set to `NULL` to print all the fields.
#'   The name of the target is always included as the first column
#'   regardless of the selection.
#'   Possible fields are below. All of them can be set in [tar_target()],
#'   [tar_target_raw()], or [tar_option_set()].
#'   * `name`: Name of the target.
#'   * `command`: the R command that runs when the target builds.
#'   * `pattern`: branching pattern of the target, if applicable.
#'   * `format`: Storage format.
#'   * `repository`: Storage repository.
#'   * `iteration`: Iteration mode for branching.
#'   * `error`: Error mode, what to do when the target fails.
#'   * `memory`: Memory mode, when to keep targets in memory.
#'   * `storage`: Storage mode for high-performance computing scenarios.
#'   * `retrieval`: Retrieval mode for high-performance computing scenarios.
#'   * `deployment`: Where/whether to deploy the target in high-performance
#'     computing scenarios.
#'   * `priority`: Numeric of length 1 between 0 and 1. Controls which
#'     targets get deployed first when multiple competing targets are ready
#'     simultaneously. Targets with priorities closer to 1 get built earlier
#'     (and polled earlier in [tar_make_future()]).
#'   * `resources`: A list of target-specific resource requirements for
#'     [tar_make_future()].
#'   * `cue_mode`: Cue mode from [tar_cue()].
#'   * `cue_depend`: Depend cue from [tar_cue()].
#'   * `cue_expr`: Command cue from [tar_cue()].
#'   * `cue_file`: File cue from [tar_cue()].
#'   * `cue_format`: Format cue from [tar_cue()].
#'   * `cue_repository`: Repository cue from [tar_cue()].
#'   * `cue_iteration`: Iteration cue from [tar_cue()].
#'   * `packages`: List columns of packages loaded before building the target.
#'   * `library`: List column of library paths to load the packages.
#' @examples
#' if (identical(Sys.getenv("TAR_EXAMPLES"), "true")) {
#' tar_dir({ # tar_dir() runs code from a temporary directory.
#' tar_script({
#'   tar_option_set()
#'   list(
#'     tar_target(y1, 1 + 1),
#'     tar_target(y2, 1 + 1),
#'     tar_target(z, y1 + y2),
#'     tar_target(m, z, pattern = map(z)),
#'     tar_target(c, z, pattern = cross(z))
#'   )
#' }, ask = FALSE)
#' tar_manifest()
#' tar_manifest(fields = c("name", "command"))
#' tar_manifest(fields = "command")
#' tar_manifest(fields = starts_with("cue"))
#' })
#' }
tar_manifest <- function(
  names = NULL,
  fields = c("name", "command", "pattern"),
  callr_function = callr::r,
  callr_arguments = targets::callr_args_default(callr_function),
  envir = parent.frame(),
  script = targets::tar_config_get("script")
) {
  force(envir)
  tar_assert_callr_function(callr_function)
  tar_assert_list(callr_arguments, "callr_arguments mut be a list.")
  targets_arguments <- list(
    names_quosure = rlang::enquo(names),
    fields_quosure = rlang::enquo(fields)
  )
  callr_outer(
    targets_function = tar_manifest_inner,
    targets_arguments = targets_arguments,
    callr_function = callr_function,
    callr_arguments = callr_arguments,
    envir = envir,
    script = script,
    store = tar_config_get("store"),
    fun = "tar_manifest"
  )
}

tar_manifest_inner <- function(
  pipeline,
  names_quosure,
  fields_quosure
) {
  igraph <- pipeline_produce_igraph(pipeline, targets_only = TRUE)
  all_names <- topo_sort_igraph(igraph)
  names <- tar_tidyselect_eval(names_quosure, all_names) %|||% all_names
  names <- intersect(all_names, names)
  out <- map(names, ~tar_manifest_target(pipeline_get_target(pipeline, .x)))
  out <- do.call(rbind, out)
  fields <- tar_tidyselect_eval(fields_quosure, colnames(out)) %|||%
    colnames(out)
  out[, base::union("name", fields), drop = FALSE]
}

tar_manifest_target <- function(target) {
  out <- list(
    name = target_get_name(target),
    command = tar_manifest_command(target$command$expr),
    pattern = tar_manifest_pattern(target$settings$pattern),
    format = target$settings$format,
    repository = target$settings$repository,
    iteration = target$settings$iteration,
    error = target$settings$error,
    memory = target$settings$memory,
    storage = target$settings$storage,
    retrieval = target$settings$retrieval,
    deployment = target$settings$deployment,
    priority = target$settings$priority,
    resources = list(target$settings$resources),
    cue_mode = target$cue$mode,
    cue_command = target$cue$command,
    cue_depend = target$cue$depend,
    cue_file = target$cue$file,
    cue_format = target$cue$format,
    cue_repository = target$cue$repository,
    cue_iteration = target$cue$iteration,
    packages = list(target$command$packages),
    library = list(target$command$library)
  )
  tibble::as_tibble(out)
}

tar_manifest_command <- function(expr) {
  out <- tar_deparse_safe(expr, collapse = " \\n ")
  out <- mask_pointers(out)
  string_sub_expression(out)
}

tar_manifest_pattern <- function(pattern) {
  if_any(
    is.null(pattern),
    NA_character_,
    string_sub_expression(tar_deparse_safe(pattern, collapse = " "))
  )
}
