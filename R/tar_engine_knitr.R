#' @title Target Markdown `knitr` engine
#' @export
#' @family Target Markdown
#' @seealso <https://books.ropensci.org/targets/markdown.html>
#' @description `knitr` language engine that runs `{targets}`
#'   code chunks in Target Markdown.
#' @return Character, output generated from `knitr::engine_output()`.
#' @param options A named list of `knitr` chunk options.
#' @section Target Markdown interactive mode:
#'   Target Markdown has two modes:
#'   1. Non-interactive mode. This is the default when you
#'     run `knitr::knit()` or `rmarkdown::render()`.
#'     Here, the code in `{targets}` code chunks gets written
#'     to special script files in order to set up a `targets`
#'     pipeline to run later.
#'   2. Interactive mode: here, no scripts are written to set up
#'     a pipeline. Rather, the globals or targets in question
#'     are run in the current environment and the values
#'     are assigned to that environment.
#'
#'   The mode is interactive if `!isTRUE(getOption("knitr.in.progress"))`,
#'   is `TRUE`. The `knitr.in.progress` option is `TRUE`
#'   when you run `knitr::knit()` or `rmarkdown::render()`
#'   and `NULL` if you are running one chunk at a time interactively
#'   in an integrated development environment, e.g. the
#'   notebook interface in RStudio:
#'   <https://bookdown.org/yihui/rmarkdown/notebook.html>.
#'   You can choose the mode with the `tar_interactive`
#'   chunk option.
#'   (In `targets` 0.6.0, `tar_interactive` defaults to `interactive()`
#'   instead of `!isTRUE(getOption("knitr.in.progress"))`.)
#' @section Target Markdown chunk options:
#'   Target Markdown introduces the following `knitr` code chunk options.
#'   Most other standard `knitr` code chunk options should just work
#'   in non-interactive mode. In interactive mode, not all
#'   * `tar_globals`: Logical of length 1,
#'     whether to define globals or targets.
#'     If `TRUE`, the chunk code defines functions, objects, and options
#'     common to all the targets. If `FALSE` or `NULL` (default),
#'     then the chunk returns formal targets for the pipeline.
#'   * `tar_interactive`: Logical of length 1, whether to run in
#'     interactive mode or non-interactive mode.
#'     See the "Target Markdown interactive mode" section of this
#'     help file for details.
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
  options$tar_name <- options$tar_name %|||% options$label
  options$tar_script <- options$tar_script %|||% tar_config_get("script")
  if (!is.null(options$targets)) {
    options$tar_globals <- options$tar_globals %|||% options$targets
    tar_warn_deprecate(
      "In Target Markdown, the `targets` chunk option is deprecated ",
      "(version 0.6.0, 2021-07-21). ",
      "Set the chunk option tar_globals = TRUE to define functions, ",
      "global objects, and settings. To define targets, ",
      "either set tar_globals = FALSE or leave tar_globals unset."
    )
  }
  tar_assert_package("knitr")
  tar_assert_list(options, "knitr chunk options must be a list.")
  warn_labels_duplicated()
  warn_labels_unnamed(options)
  engine_knitr_tar_assert_options(options)
  engine_knitr_set_interactive(options)
  on.exit(engine_knitr_unset_interactive())
  if_any(
    identical(options$tar_globals, TRUE),
    engine_knitr_globals(options),
    engine_knitr_targets(options)
  )
}

engine_knitr_tar_assert_options <- function(options) {
  scalars <- c(
    "tar_globals",
    "tar_interactive",
    "tar_name",
    "tar_script",
    "tar_simple"
  )
  logicals <- c("tar_globals", "tar_interactive", "tar_simple")
  characters <- c("tar_name", "tar_script")
  for (option in scalars) {
    tar_assert_scalar(
      options[[option]] %|||% TRUE,
      paste(option, "chunk option must either be NULL or have length 1.")
    )
  }
  for (option in logicals) {
    tar_assert_lgl(
      options[[option]] %|||% TRUE,
      paste(option, "chunk option must either be NULL or logical.")
    )
  }
  for (option in characters) {
    tar_assert_chr(
      options[[option]],
      paste(option, "chunk option must either be NULL or character.")
    )
    tar_assert_nzchar(
      options[[option]],
      paste(option, "chunk option must not be the empty string.")
    )
  }
}

engine_knitr_set_interactive <- function(options) {
  x <- options$tar_interactive %|||% !isTRUE(getOption("knitr.in.progress"))
  tar_assert_scalar(x)
  tar_assert_lgl(x)
  tar_runtime$set_interactive(x)
}

engine_knitr_unset_interactive <- function() {
  tar_runtime$unset_interactive()
}

engine_knitr_is_interactive <- function() {
  isTRUE(tar_runtime$get_interactive())
}

engine_knitr_globals <- function(options) {
  if_any(
    engine_knitr_is_interactive(),
    engine_knitr_globals_interactive(options),
    engine_knitr_globals_noninteractive(options)
  )
}

engine_knitr_targets <- function(options) {
  if (options$tar_simple %|||% FALSE) {
    options$code <- engine_knitr_targets_command(options)
  }
  if_any(
    engine_knitr_is_interactive(),
    engine_knitr_targets_interactive(options),
    engine_knitr_targets_noninteractive(options)
  )
}

engine_knitr_targets_command <- function(options) {
  c(
    paste0("tar_target(", options$tar_name, ", {"),
    paste(" ", options$code),
    "})"
  )
}

engine_knitr_globals_interactive <- function(options) {
  out_code <- engine_knitr_echo_code(options)
  message <- "Run code and assign objects to the environment."
  out_message <- engine_knitr_run_message(options, message)
  options_globals <- options
  options_globals$echo <- FALSE
  code <- c(
    "evalq({",
    options_globals$code,
    "}, envir = targets::tar_option_get(\"envir\"))"
  )
  options_globals$code <- paste(code, collapse = "\n")
  out_globals <- knitr::knit_engines$get("R")(options = options_globals)
  paste0(out_code, out_message, out_globals)
}

engine_knitr_globals_noninteractive <- function(options) {
  write_targets_r(options$tar_script)
  write_targets_r_globals(options$code, options$tar_name, options$tar_script)
  out <- paste0(
    "Establish ",
    options$tar_script,
    " and ",
    path_script_r_globals(options$tar_script, options$tar_name),
    "."
  )
  engine_knitr_output(options, out)
}

engine_knitr_targets_interactive <- function(options) {
  out_code <- engine_knitr_echo_code(options)
  message <- paste(
    engine_knitr_definition_message(options),
    engine_knitr_interactive_message(options),
    sep = "\n"
  )
  message <- trimws(message)
  out_message <- engine_knitr_run_message(options, message)
  options_make <- options
  code_library <- "library(targets)"
  code_make <- c("targets::tar_make_interactive(", deparse(options$code), ")")
  code_make <- paste(code_make, collapse = "")
  options_make$code <- paste(code_library, code_make, sep = "\n")
  options_make$echo <- FALSE
  out_make <- knitr::knit_engines$get("R")(options = options_make)
  paste0(out_code, out_message, out_make)
}

engine_knitr_targets_noninteractive <- function(options) {
  write_targets_r(options$tar_script)
  write_targets_r_targets(options$code, options$tar_name, options$tar_script)
  out <- paste0(
    "Establish ",
    options$tar_script,
    " and ",
    path_script_r_targets(options$tar_script, options$tar_name),
    "."
  )
  out <- c(engine_knitr_definition_message(options), out)
  engine_knitr_output(options, out)
}

engine_knitr_echo_code <- function(options) {
  options$eval <- FALSE
  options$results <- "hide"
  knitr::knit_engines$get("R")(options)
}

engine_knitr_run_message <- function(options, message) {
  options$code <- sprintf("message(\"%s\")", message)
  options$echo <- FALSE
  knitr::knit_engines$get("R")(options)
}

engine_knitr_output <- function(options, out) {
  code <- paste(options$code, collapse = "\n")
  options$engine <- "r"
  out <- if_any(options$message, out, character(0))
  knitr::engine_output(options = options, code = code, out = out)
}

engine_knitr_definition_message <- function(options) {
  if_any(
    options$tar_simple %|||% FALSE,
    paste("Define target", options$tar_name, "from chunk code."),
    character(0)
  )
}

engine_knitr_interactive_message <- function(options) {
  if_any(
    options$tar_simple %|||% FALSE,
    paste(
      "Run target",
      options$tar_name,
      "and assign it to the environment."
    ),
    "Run targets and assign them to the environment."
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
      "to prohibit duplicate knitr chunk labels. ",
      "Warnings like this one are important, but if you must suppress them, ",
      "you can do so with Sys.setenv(TAR_WARN = \"false\")."
    )
  }
}

warn_labels_unnamed <- function(options) {
  suppressed <- identical(Sys.getenv("TAR_WARN"), "false")
  if (!suppressed && any(grepl("unnamed-chunk-[0-9]*$", options$tar_name))) {
    tar_warn_validate(
      "Please assign explicit labels to {targets} code chunks ",
      "in order to avoid accidental duplicated script files. ",
      "Warnings like this one are important, ",
      "but you can suppress them with Sys.setenv(TAR_WARN = \"false\")."
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
