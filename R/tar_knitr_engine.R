#' @title Target Markdown `knitr` engine
#' @export
#' @family targetopia
#' @seealso <https://books.ropensci.org/targets/markdown.html>
#' @description `knitr` language engine that runs `{targets}`
#'   code chunks in Target Markdown.
#' @return Character, output generated from `knitr::engine_output()`.
#' @param options A named list of `knitr` chunk options.
#' @examples
#' # Register the engine.
#' if (requireNamespace("knitr", quietly = TRUE)) {
#'   knitr::knit_engines$set(targets = tar_knitr_engine)
#' }
#' # Then, {targets} code chunks in a knitr report will run
#' # as described at https://books.ropensci.org/targets/markdown.html.
tar_knitr_engine <- function(options) {
  assert_package("knitr")
  assert_list(options, "knitr chunk options must be a list.")
  assert_chr(options$label, "knitr chunk must have a label")
  assert_nzchar(options$label, "knitr chunk label must not be empty")
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
    paste0("tar_target(", options$label, ", {"),
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
  write_targets_r_globals(options$code, options$label, options$tar_script)
  out <- paste0(
    "Established ",
    options$tar_script,
    " and ",
    path_script_r_globals(options$tar_script, options$label),
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
  write_targets_r_targets(options$code, options$label, options$tar_script)
  out <- paste0(
    "Established ",
    options$tar_script,
    " and ",
    path_script_r_targets(options$tar_script, options$label),
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
    paste("Defined target", options$label, "automatically from chunk code."),
    character(0)
  )
}

knitr_engine_prototype_message <- function(options) {
  if_any(
    options$tar_simple %|||% FALSE,
    paste("Ran target", options$label, "and assigned it to the environment."),
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
  if (identical(getOption("knitr.duplicate.label"), "allow")) {
    warn_validate(
      "knitr.duplicate.label is set to \"allow\". Duplicate labels ",
      "interfere with the proper execution of Target Markdown. ",
      "Please set knitr.duplicate.label to a value other than \"allow\" ",
      "to prohibit duplicate knitr chunk labels."
    )
  }
}

warn_labels_unnamed <- function(options) {
  suppressed <- identical(Sys.getenv("TAR_WARN"), "false")
  if (!suppressed && any(grepl("unnamed-chunk-[0-9]*$", options$label))) {
    warn_validate(
      "Please assign explicit labels to {targets} code chunks ",
      "in order to avoid accidental duplicated script files. ",
      "Suppress this warning with Sys.setenv(TAR_WARN = \"false\")."
    )
  }
}

knitr_engine_set <- function() {
  if (requireNamespace("knitr", quietly = TRUE)) {
    knitr::knit_engines$set(targets = function(options) tar_knitr_engine(options))
  }
}
