#' @rdname tar_target
#' @export
tar_target_raw <- function(
  name,
  command,
  pattern = NULL,
  packages = targets::tar_option_get("packages"),
  library = targets::tar_option_get("library"),
  deps = NULL,
  string = NULL,
  format = targets::tar_option_get("format"),
  repository = targets::tar_option_get("repository"),
  iteration = targets::tar_option_get("iteration"),
  error = targets::tar_option_get("error"),
  memory = targets::tar_option_get("memory"),
  garbage_collection = isTRUE(targets::tar_option_get("garbage_collection")),
  deployment = targets::tar_option_get("deployment"),
  priority = targets::tar_option_get("priority"),
  resources = targets::tar_option_get("resources"),
  storage = targets::tar_option_get("storage"),
  retrieval = targets::tar_option_get("retrieval"),
  cue = targets::tar_option_get("cue"),
  description = targets::tar_option_get("description")
) {
  tar_assert_nonmissing(name)
  tar_assert_name(name)
  tar_assert_nonmissing(command, paste("target", name, "has no command."))
  if (is.expression(command)) {
    tar_assert_nonmissing(
      command[[1]],
      paste("target", name, "has no command.")
    )
  }
  tar_assert_scalar(
    as.expression(command),
    paste("the command of target", name, "must have length 1.")
  )
  tar_assert_chr(packages)
  tar_assert_chr(
    library %|||% character(0),
    "library in tar_target_raw() must be NULL or character."
  )
  tar_assert_format(format)
  tar_assert_repository(repository)
  if (format == "file_fast") {
    format <- "file"
  }
  tar_assert_flag(iteration, c("vector", "list", "group"))
  tar_assert_flag(
    error,
    c("stop", "continue", "null", "abridge", "trim", "workspace")
  )
  deprecate_error_workspace(error)
  deprecate_priority(priority)
  tar_assert_flag(memory, c("auto", "persistent", "transient"))
  garbage_collection <- isTRUE(garbage_collection)
  tar_assert_lgl(garbage_collection)
  tar_assert_scalar(garbage_collection)
  tar_assert_flag(deployment, c("worker", "main"))
  tar_assert_dbl(priority)
  tar_assert_scalar(priority)
  tar_assert_ge(priority, 0)
  tar_assert_le(priority, 1)
  tar_assert_resources(resources)
  tar_assert_flag(storage, c("main", "worker", "none"))
  tar_assert_flag(retrieval, c("auto", "main", "worker", "none"))
  warn_error_format(error = error, format = format)
  if (!is.null(cue)) {
    cue_validate(cue)
  }
  tar_assert_chr(description)
  tar_assert_scalar(description %||% "x")
  tar_assert_none_na(description)
  tar_assert_true(
    is.null(pattern) || (iteration != "group"),
    msg = sprintf(
      paste(
        "\"group\" iteration is invalid for dynamic targets such as %s.",
        "(e.g. with pattern = %s in tar_target()). You can either",
        "set iteration = \"group\" or set a dynamic branching pattern like",
        "pattern = %s, but please avoid doing both for the same target."
      ),
      name,
      tar_deparse_language(pattern),
      tar_deparse_language(pattern)
    )
  )
  target_init(
    name = name,
    expr = command,
    pattern = pattern,
    packages = packages,
    library = library,
    deps = deps,
    string = string,
    format = format,
    repository = repository,
    iteration = iteration,
    error = error,
    memory = memory,
    garbage_collection = garbage_collection,
    deployment = deployment,
    priority = priority,
    resources = resources,
    storage = storage,
    retrieval = retrieval,
    cue = cue,
    description = description
  )
}

warn_error_format <- function(error, format) {
  if (format %in% c("keras", "torch") && error == "null") {
    message <- paste0(
      "In targets, format = \"",
      format,
      "\" is incompatible with error = \"null\" ",
      "(and superseded by tar_format())."
    )
    tar_warn_deprecate(message)
  }
}
