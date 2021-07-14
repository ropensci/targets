#' @title Target Markdown `knitr` engine
#' @export
#' @family engines
#' @seealso <https://books.ropensci.org/targets/markdown.html>
#' @description `knitr` language engine that runs `{targets}`
#'   code chunks in Target Markdown.
#' @return Character, output generated from `knitr::engine_output()`.
#' @param options A named list of `knitr` chunk options.
#' @section Target Markdown chunk options:
#'   Target Markdown introduces the following `knitr` code chunk options.
#'   Most other standard `knitr` code chunk options should just work.
#'   * `tar_globals`: Logical of length 1,
#'     whether to define globals or targets.
#'     If `TRUE`, the chunk code defines functions, objects, and options
#'     common to all the targets. If `FALSE` or `NULL` (default),
#'     then the chunk returns formal targets for the pipeline.
#'   * `tar_interactive`: Logical of length 1, whether to run in
#'     interactive mode or non-interactive mode.
#'     Defaults to the return value of `interactive()`.
#'   * `tar_name`: name to use for writing helper script files
#'     (e.g. `_targets_r/targets/target_script.R`)
#'     and specifying target names if the `tar_simple` chunk option
#'     is `TRUE`. All helper scripts and target names must have
#'     unique names, so please do not set this option globally
#'     with `knitr::opts_chunk$set()`.
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
  if (identical(as.logical(options$eval), FALSE)) {
    return(engine_knitr_output(options = options, out = character(0)))
  }
  tar_assert_package("knitr")
  tar_assert_list(options, "knitr chunk options must be a list.")
  options$tar_name <- options$tar_name %|||% options$label
  msg <- paste(
    "{targets} code chunks require a nonempty length-1 character string",
    "for the chunk label or the tar_name chunk option."
  )
  tar_assert_scalar(options$tar_name, msg)
  tar_assert_chr(options$tar_name, msg)
  tar_assert_nzchar(options$tar_name, msg)
  if (!is.null(options$targets)) {
    tar_warn_deprecate(
      "In Target Markdown, the `targets` chunk option is deprecated.",
      "Set the chunk option tar_globals = TRUE to define functions, ",
      "global objects, and settings. To define targets, ",
      "either set tar_globals = FALSE or leave tar_globals unset."
    )
    options$tar_globals <- options$tar_globals %|||% options$targets
  }
  options$tar_script <- options$tar_script %|||% tar_config_get("script")
  engine_knitr_tar_assert_options(options)
  warn_labels_duplicated()
  warn_labels_unnamed(options)
  if_any(
    identical(options$tar_globals, TRUE),
    engine_knitr_globals(options),
    engine_knitr_targets(options)
  )
}

engine_knitr_tar_assert_options <- function(options) {
  choices <- c("tar_globals", "tar_interactive", "tar_script", "tar_simple")
  for (option in choices) {
    tar_assert_scalar(
      options[[option]] %|||% TRUE,
      paste(option, "chunk option must either be NULL or have length 1.")
    )
  }
  for (option in c("tar_globals", "tar_interactive", "tar_simple")) {
    tar_assert_lgl(
      options[[option]] %|||% TRUE,
      paste(option, "chunk option must either be NULL or logical.")
    )
  }
  tar_assert_chr(
    options[["tar_script"]],
    "tar_script chunk option must either be NULL or character."
  )
}

engine_knitr_globals <- function(options) {
  if_any(
    options$tar_interactive %|||% interactive(),
    engine_knitr_globals_prototype(options),
    engine_knitr_globals_construct(options)
  )
}

engine_knitr_targets <- function(options) {
  if (options$tar_simple %|||% FALSE) {
    options$code <- engine_knitr_targets_command(options)
  }
  if_any(
    options$tar_interactive %|||% interactive(),
    engine_knitr_targets_prototype(options),
    engine_knitr_targets_construct(options)
  )
}

engine_knitr_targets_command <- function(options) {
  c(
    paste0("tar_target(", options$tar_name, ", {"),
    paste(" ", options$code),
    "})"
  )
}

engine_knitr_globals_prototype <- function(options) {
  eval(parse(text = options$code), envir = tar_option_get("envir"))
  engine_knitr_output(
    options,
    "Ran code and assigned objects to the environment."
  )
}

engine_knitr_globals_construct <- function(options) {
  write_targets_r(options$tar_script)
  write_targets_r_globals(options$code, options$tar_name, options$tar_script)
  out <- paste0(
    "Established ",
    options$tar_script,
    " and ",
    path_script_r_globals(options$tar_script, options$tar_name),
    "."
  )
  engine_knitr_output(options, out)
}

engine_knitr_targets_prototype <- function(options) {
  tar_make_interactive(options$code)
  out <- c(
    engine_knitr_definition_message(options),
    engine_knitr_prototype_message(options)
  )
  engine_knitr_output(options, out)
}

engine_knitr_targets_construct <- function(options) {
  write_targets_r(options$tar_script)
  write_targets_r_targets(options$code, options$tar_name, options$tar_script)
  out <- paste0(
    "Established ",
    options$tar_script,
    " and ",
    path_script_r_targets(options$tar_script, options$tar_name),
    "."
  )
  out <- c(engine_knitr_definition_message(options), out)
  engine_knitr_output(options, out)
}

engine_knitr_output <- function(options, out) {
  code <- paste(options$code, collapse = "\n")
  options$engine <- "r"
  knitr::engine_output(options = options, code = code, out = out)
}

engine_knitr_definition_message <- function(options) {
  if_any(
    options$tar_simple %|||% FALSE,
    paste("Defined target", options$tar_name, "automatically from chunk code."),
    character(0)
  )
}

engine_knitr_prototype_message <- function(options) {
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
  lines_new <- lines_targets_r(path_script)
  if (!file.exists(path_script)) {
    dir_create(dirname(path_script))
    writeLines(lines_new, path_script)
    return()
  }
  lines_old <- readLines(path_script)
  header <- trimws(lines_old[1] %||NA% "")
  comments <- c(
    "# Generated by Target Markdown: do not edit by hand",
    "# Generated by targets: do not edit by hand"
  )
  if (header %in% comments && !identical(lines_old, lines_new)) {
    dir_create(dirname(path_script))
    writeLines(lines_new, path_script)
  } else if (!(header %in% comments)) {
    tar_throw_validate(
      "cannot safely overwrite the target script file ",
      path_script,
      " because the file was written by hand. If you want to use ",
      "Target Markdown or other features that auto-generate target ",
      "script files, please stash or remove ",
      path_script,
      " before trying again."
    )
  }
}

lines_targets_r <- function(path_script) {
  path <- system.file(
    file.path("pipelines", "_targets_r.R"),
    package = "targets",
    mustWork = TRUE
  )
  gsub(
    pattern = "PATH_SCRIPT_R",
    replacement = path_script_r(path_script),
    x = readLines(path),
    fixed = TRUE
  )
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
    tar_warn_validate(
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
    tar_warn_validate(
      "Please assign explicit labels to {targets} code chunks ",
      "in order to avoid accidental duplicated script files. ",
      "Suppress this warning with Sys.setenv(TAR_WARN = \"false\")."
    )
  }
}

# Covered in tests/interactive/test-target_markdown_default.Rmd
# and tests/interactive/test-target_markdown_paths.Rmd.
# nocov start
engine_knitr_set <- function() {
  load_engine <- requireNamespace("knitr", quietly = TRUE) &&
    is.null(knitr::knit_engines$get("targets"))
  if (load_engine) {
    knitr::knit_engines$set(targets = function(options) {
      tar_engine_knitr(options)
    })
  }
}
# nocov end
