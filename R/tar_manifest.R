#' @title Produce a data frame of information about your targets.
#' @export
#' @family inspect
#' @description Along with [tar_visnetwork()] and [tar_glimpse()],
#'   `tar_manifest()` helps check that you constructed your pipeline correctly.
#' @inheritSection tar_meta Storage access
#' @return A data frame of information about the targets in the pipeline.
#'   Rows appear in topological order (the order they will run
#'   without any influence from parallel computing or priorities).
#' @inheritParams tar_validate
#' @param names Names of the targets to show. Set to `NULL` to
#'   show all the targets (default). Otherwise,
#'   the object supplied to `names` should be a
#'   `tidyselect` expression like [any_of()] or [starts_with()]
#'   from `tidyselect` itself, or [tar_described_as()] to select target names
#'   based on their descriptions.
#' @param fields Names of the fields, or columns, to show. Set to `NULL` to
#'   show all the fields (default). Otherwise, the value of `fields` should be
#'   a `tidyselect` expression like [starts_with()] to select the columns
#'   to show in the output.
#'   Possible fields are below. All of them can be set in [tar_target()],
#'   [tar_target_raw()], or [tar_option_set()].
#'   * `name`: Name of the target.
#'   * `command`: the R command that runs when the target runs.
#'   * `description`: custom free-form text description of the target,
#'     if available.
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
#'     simultaneously. Targets with priorities closer to 1 get dispatched
#'     earlier (and polled earlier in [tar_make_future()]).
#'   * `resources`: A list of target-specific resource requirements for
#'     [tar_make_future()].
#'   * `cue_mode`: Cue mode from [tar_cue()].
#'   * `cue_depend`: Depend cue from [tar_cue()].
#'   * `cue_expr`: Command cue from [tar_cue()].
#'   * `cue_file`: File cue from [tar_cue()].
#'   * `cue_format`: Format cue from [tar_cue()].
#'   * `cue_repository`: Repository cue from [tar_cue()].
#'   * `cue_iteration`: Iteration cue from [tar_cue()].
#'   * `packages`: List columns of packages loaded before running the target.
#'   * `library`: List column of library paths to load the packages.
#' @param drop_missing Logical of length 1, whether to automatically omit
#'   empty columns and columns with all missing values.
#' @examples
#' if (identical(Sys.getenv("TAR_EXAMPLES"), "true")) { # for CRAN
#' tar_dir({ # tar_dir() runs code from a temp dir for CRAN.
#' tar_script({
#'   library(targets)
#'   library(tarchetypes)
#'   tar_option_set()
#'   list(
#'     tar_target(y1, 1 + 1),
#'     tar_target(y2, 1 + 1),
#'     tar_target(z, y1 + y2),
#'     tar_target(m, z, pattern = map(z), description = "branching over z"),
#'     tar_target(c, z, pattern = cross(z))
#'   )
#' }, ask = FALSE)
#' tar_manifest()
#' tar_manifest(fields = any_of(c("name", "command")))
#' tar_manifest(fields = any_of("command"))
#' tar_manifest(fields = starts_with("cue"))
#' })
#' }
tar_manifest <- function(
  names = NULL,
  fields = tidyselect::any_of(c("name", "command", "pattern", "description")),
  drop_missing = TRUE,
  callr_function = callr::r,
  callr_arguments = targets::tar_callr_args_default(callr_function),
  envir = parent.frame(),
  script = targets::tar_config_get("script")
) {
  force(envir)
  tar_assert_lgl(drop_missing)
  tar_assert_scalar(drop_missing)
  tar_assert_none_na(drop_missing)
  tar_assert_callr_function(callr_function)
  tar_assert_list(callr_arguments, "callr_arguments mut be a list.")
  targets_arguments <- list(
    names_quosure = rlang::enquo(names),
    fields_quosure = rlang::enquo(fields),
    drop_missing = drop_missing
  )
  callr_outer(
    targets_function = tar_manifest_inner,
    targets_arguments = targets_arguments,
    callr_function = callr_function,
    callr_arguments = callr_arguments,
    envir = envir,
    script = script,
    store = tempfile(),
    fun = "tar_manifest"
  )
}

tar_manifest_inner <- function(
  pipeline,
  names_quosure,
  fields_quosure,
  drop_missing
) {
  igraph <- pipeline_produce_igraph(pipeline, targets_only = TRUE)
  all_names <- if_any(
    igraph::is_dag(igraph),
    topo_sort_igraph(igraph),
    igraph::V(igraph)$name
  )
  names <- tar_tidyselect_eval(names_quosure, all_names) %|||% all_names
  names <- intersect(all_names, names)
  choices <- names(tar_manifest_fields)
  fields <- tar_tidyselect_eval(fields_quosure, choices) %|||% choices
  fields <- base::union("name", fields)
  targets <- map(names, ~ pipeline_get_target(pipeline, .x))
  out <- tibble::as_tibble(map(tar_manifest_fields[fields], ~ .x(targets)))
  if (drop_missing) {
    for (field in colnames(out)) {
      if (all(is.na(unlist(out[[field]])))) {
        out[[field]] <- NULL
      }
    }
  }
  out
}

tar_manifest_fields <- list(
  name = function(targets) map_chr(targets, target_get_name),
  command = function(targets) {
    map_chr(targets, ~ tar_manifest_command(.x$command$expr))
  },
  description = function(targets) {
    map_chr(targets, ~ .x$settings$description %||% NA_character_)
  },
  pattern = function(targets) {
    map_chr(targets, ~ tar_manifest_pattern(.x$settings$pattern))
  },
  format = function(targets) map_chr(targets, ~ .x$settings$format),
  repository = function(targets) map_chr(targets, ~ .x$settings$repository),
  iteration = function(targets) map_chr(targets, ~ .x$settings$iteration),
  error = function(targets) map_chr(targets, ~ .x$settings$error),
  memory = function(targets) map_chr(targets, ~ .x$settings$memory),
  storage = function(targets) map_chr(targets, ~ .x$settings$storage),
  retrieval = function(targets) map_chr(targets, ~ .x$settings$retrieval),
  deployment = function(targets) map_chr(targets, ~ .x$settings$deployment),
  priority = function(targets) map_dbl(targets, ~ .x$settings$priority),
  resources = function(targets) map(targets, ~ .x$settings$resources),
  cue_mode = function(targets) map_chr(targets, ~ .x$cue$mode),
  cue_command = function(targets) map_lgl(targets, ~ .x$cue$command),
  cue_depend = function(targets) map_lgl(targets, ~ .x$cue$depend),
  cue_file = function(targets) map_lgl(targets, ~ .x$cue$file),
  cue_format = function(targets) map_lgl(targets, ~ .x$cue$format),
  cue_repository = function(targets) map_lgl(targets, ~ .x$cue$repository),
  cue_iteration = function(targets) map_lgl(targets, ~ .x$cue$iteration),
  packages = function(targets) map(targets, ~ .x$command$packages),
  library = function(targets) map(targets, ~ .x$command$library)
)

tar_manifest_command <- function(expr) {
  out <- tar_deparse_safe(expr, collapse = "\n ")
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
