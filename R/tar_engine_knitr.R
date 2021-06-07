#' @title Target Markdown `knitr` engine
#' @export
#' @family engines
#' @seealso <https://books.ropensci.org/targets/markdown.html>
#' @description `knitr` language engine that runs `{targets}`
#'   code chunks in Target Markdown.
#' @return Character, output generated from `knitr::engine_output()`.
#' @param options A named list of `knitr` chunk options.
#' @section Target Markdown chunk options:
#'   Target Markdown supports the following `knitr` code chunk options:
#'   * `tar_globals`: Logical of length 1,
#'     whether to define globals or targets.
#'     If `TRUE`, the chunk code defines functions, objects, and options
#'     common to all the targets. If `FALSE` or `NULL` (default),
#'     then the chunk returns formal targets for the pipeline.
#'   * `tar_name`: name to use for writing helper script files
#'     (e.g. `_targets_r/targets/target_script.R`)
#'     and specifying target names if the `tar_simple` chunk option
#'     is `TRUE`. All helper scripts and target names must have
#'     unique names, so please do not set this option globally
#'     with `knitr::opts_chunk$set()`.
#'   * `tar_interactive`: Logical of length 1, whether to run in
#'     interactive mode or non-interactive mode.
#'     Defaults to the return value of `interactive()`.
#'   * `tar_script`: Character of length 1, where to write the
#'     target script file in non-interactive mode. Most users can
#'     skip this option and stick with the default `_targets.R` script path.
#'     Helper script files are always written next to the target script in
#'     a folder with an `"_r"` suffix. The `tar_script` path must either be
#'     absolute or be relative to the project root
#'     (where you call `tar_make()` or similar).
#'     If not specified, the target script path defaults to
#'     `tar_config_get("script")` (default: `_targets.R`;
#'     helpers default: `_targets_r/`). When you run `tar_make()` etc.
#'     with a non-default target script, you must select the correct target
#'     script file either with the `script` argument or with
#'     `tar_config_set(script = ...)`. The function will `source()`
#'     the script file from the current working directory
#'     (i.e. with `chdir = FALSE` in `source()`).
#'   * `tar_simple`: Logical of length 1.
#'     Set to `TRUE` to define a single target with a simplified interface.
#'     In code chunks with `tar_simple` equal to `TRUE`, the chunk label
#'     (or the `tar_name` chunk option if you set it)
#'     becomes the name, and the chunk code becomes the command.
#'     In other words, a code chunk with label `targetname` and
#'     command `mycommand()` automatically gets converted to
#'     `tar_target(name = targetname, command = mycommand())`.
#'     All other arguments of `tar_target()` remain at their default
#'     values (configurable with `tar_option_set()` in a
#'     `tar_globals = TRUE` chunk).
#' @examples
#' if (identical(Sys.getenv("TAR_EXAMPLES"), "true")) {
#' # Register the engine.
#' if (requireNamespace("knitr", quietly = TRUE)) {
#'   knitr::knit_engines$set(targets = targets::tar_engine_knitr)
#' }
#' # Then, {targets} code chunks in a knitr report will run
#' # as described at https://books.ropensci.org/targets/markdown.html.
#' }
tar_engine_knitr <- function(options) {
  assert_package("knitr")
  assert_list(options, "knitr chunk options must be a list.")
  options$tar_name <- options$tar_name %|||% options$label
  msg <- paste(
    "{targets} code chunks require a nonempty length-1 character string",
    "for the chunk label or the tar_name chunk option."
  )
  assert_scalar(options$tar_name, msg)
  assert_chr(options$tar_name, msg)
  assert_nzchar(options$tar_name, msg)
  if (!is.null(options$targets)) {
    warn_deprecate(
      "In Target Markdown, the `targets` chunk option is deprecated.",
      "Set the chunk option tar_globals = TRUE to define functions, ",
      "global objects, and settings. To define targets, ",
      "either set tar_globals = FALSE or leave tar_globals unset."
    )
    options$tar_globals <- options$tar_globals %|||% options$targets
  }
  options$tar_script <- options$tar_script %|||% tar_config_get("script")
  knitr_engine_assert_options(options)
  warn_labels_duplicated()
  warn_labels_unnamed(options)
  if_any(
    identical(options$tar_globals, TRUE),
    knitr_engine_globals(options),
    knitr_engine_targets(options)
  )
}

knitr_engine_assert_options <- function(options) {
  choices <- c("tar_globals", "tar_interactive", "tar_script", "tar_simple")
  for (option in choices) {
    assert_scalar(
      options[[option]] %|||% TRUE,
      paste(option, "chunk option must either be NULL or have length 1.")
    )
  }
  for (option in c("tar_globals", "tar_interactive", "tar_simple")) {
    assert_lgl(
      options[[option]] %|||% TRUE,
      paste(option, "chunk option must either be NULL or logical.")
    )
  }
  assert_chr(
    options[["tar_script"]],
    "tar_script chunk option must either be NULL or character."
  )
}

knitr_engine_globals <- function(options) {
  if_any(
    options$tar_interactive %|||% interactive(),
    knitr_engine_globals_prototype(options),
    knitr_engine_globals_construct(options)
  )
}

knitr_engine_targets <- function(options) {
  if (options$tar_simple %|||% FALSE) {
    options$code <- knitr_engine_targets_command(options)
  }
  if_any(
    options$tar_interactive %|||% interactive(),
    knitr_engine_targets_prototype(options),
    knitr_engine_targets_construct(options)
  )
}

knitr_engine_targets_command <- function(options) {
  c(
    paste0("tar_target(", options$tar_name, ", {"),
    paste(" ", options$code),
    "})"
  )
}

knitr_engine_globals_prototype <- function(options) {
  eval(parse(text = options$code), envir = tar_option_get("envir"))
  knitr_engine_output(
    options,
    "Assigned objects to the environment."
  )
}

knitr_engine_globals_construct <- function(options) {
  write_targets_r(options$tar_script)
  write_targets_r_globals(options$code, options$tar_name, options$tar_script)
  out <- paste0(
    "Established ",
    options$tar_script,
    " and ",
    path_script_r_globals(options$tar_script, options$tar_name),
    "."
  )
  knitr_engine_output(options, out)
}

knitr_engine_targets_prototype <- function(options) {
  tar_make_interactive(options$code)
  out <- c(
    knitr_engine_definition_message(options),
    knitr_engine_prototype_message(options)
  )
  knitr_engine_output(options, out)
}

knitr_engine_targets_construct <- function(options) {
  write_targets_r(options$tar_script)
  write_targets_r_targets(options$code, options$tar_name, options$tar_script)
  out <- paste0(
    "Established ",
    options$tar_script,
    " and ",
    path_script_r_targets(options$tar_script, options$tar_name),
    "."
  )
  out <- c(knitr_engine_definition_message(options), out)
  knitr_engine_output(options, out)
}

knitr_engine_output <- function(options, out) {
  code <- paste(options$code, collapse = "\n")
  options$engine <- "r"
  knitr::engine_output(options = options, code = code, out = out)
}

knitr_engine_definition_message <- function(options) {
  if_any(
    options$tar_simple %|||% FALSE,
    paste("Defined target", options$tar_name, "automatically from chunk code."),
    character(0)
  )
}

knitr_engine_prototype_message <- function(options) {
  if_any(
    options$tar_simple %|||% FALSE,
    paste(
      "Ran target",
      options$tar_name,
      "and assigned it to the environment."
    ),
    "Ran targets and assigned them to the environment."
  )
}

write_targets_r <- function(path_script) {
  path <- system.file(
    file.path("pipelines", "_targets_r.R"),
    package = "targets",
    mustWork = TRUE
  )
  lines_new <- gsub(
    pattern = "PATH_SCRIPT_R",
    replacement = path_script_r(path_script),
    x = readLines(path),
    fixed = TRUE
  )
  lines_old <- if_any(
    file.exists(path_script),
    readLines(path_script),
    character(0)
  )
  if (!identical(lines_new, lines_old)) {
    dir_create(dirname(path_script))
    writeLines(lines_new, path_script)
  }
}

write_targets_r_globals <- function(code, name, path_script) {
  dir_create(path_script_r_globals_dir(path_script))
  writeLines(code, path_script_r_globals(path_script, name))
}

write_targets_r_targets <- function(code, name, path_script) {
  dir_create(path_script_r_targets_dir(path_script))
  writeLines(code, path_script_r_targets(path_script, name))
}

warn_labels_duplicated <- function() {
  should_warn <- identical(getOption("knitr.duplicate.label"), "allow") &&
    !identical(Sys.getenv("TAR_WARN"), "false")
  if (should_warn) {
    warn_validate(
      "knitr.duplicate.label is set to \"allow\". Duplicate labels ",
      "interfere with the proper execution of Target Markdown ",
      "unless you set unique values for the tar_name chunk option. ",
      "Please set knitr.duplicate.label to a value other than \"allow\" ",
      "to prohibit duplicate knitr chunk labels. Suppress this warning with ",
      "Sys.setenv(TAR_WARN = \"false\")."
    )
  }
}

warn_labels_unnamed <- function(options) {
  suppressed <- identical(Sys.getenv("TAR_WARN"), "false")
  if (!suppressed && any(grepl("unnamed-chunk-[0-9]*$", options$tar_name))) {
    warn_validate(
      "Please assign explicit labels to {targets} code chunks ",
      "in order to avoid accidental duplicated script files. ",
      "Suppress this warning with Sys.setenv(TAR_WARN = \"false\")."
    )
  }
}

knitr_engine_set <- function() {
  if (requireNamespace("knitr", quietly = TRUE)) {
    knitr::knit_engines$set(targets = function(options) {
      tar_engine_knitr(options)
    })
  }
}
