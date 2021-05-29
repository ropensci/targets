tar_engine <- function(options) {
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
  for (option in c("tar_globals", "tar_interactive")) {
    assert_scalar(
      options[[option]] %|||% TRUE,
      paste(option, "chunk option must either be NULL or have length 1.")
    )
    assert_scalar(
      options[[option]] %|||% TRUE,
      paste(option, "chunk option must either be NULL or logical.")
    )
  }
  warn_duplicate_labels()
  if_any(
    identical(options$tar_globals, TRUE),
    tar_engine_globals(options),
    tar_engine_targets(options)
  )
}

tar_engine_globals <- function(options) {
  if_any(
    options$tar_interactive %|||% interactive(),
    tar_engine_globals_prototype(options),
    tar_engine_globals_construct(options)
  )
}

tar_engine_targets <- function(options) {
  if_any(
    options$tar_interactive %|||% interactive(),
    tar_engine_targets_prototype(options),
    tar_engine_targets_construct(options)
  )
}

tar_engine_globals_prototype <- function(options) {
  eval(parse(text = options$code), envir = tar_option_get("envir"))
  tar_engine_output(
    options,
    "Assigned objects to the environment."
  )
}

tar_engine_globals_construct <- function(options) {
  write_targets_r()
  write_targets_r_globals(options$label, options$code)
  out <- paste("Wrote _targets.R and", path_script_r_globals(options$label))
  tar_engine_output(options, out)
}

tar_engine_targets_prototype <- function(options) {
  tar_make_interactive(options$code)
  tar_engine_output(
    options,
    "Ran targets and assigned them to the environment."
  )
}

tar_engine_targets_construct <- function(options) {
  write_targets_r()
  write_targets_r_targets(options$label, options$code)
  out <- paste("Wrote _targets.R and", path_script_r_targets(options$label))
  tar_engine_output(options, out)
}

tar_engine_output <- function(options, out) {
  code <- paste(options$code, collapse = "\n")
  options$engine <- "r"
  knitr::engine_output(options = options, code = code, out = out)
}

write_targets_r <- function() {
  path <- system.file(
    file.path("pipelines", "_targets_r.R"),
    package = "targets",
    mustWork = TRUE
  )
  if (!file.exists(path_script()) || !files_identical(path, path_script())) {
    file.copy(path, path_script(), overwrite = TRUE)
  }
}

write_targets_r_globals <- function(name, code) {
  dir_create(path_script_r_globals_dir())
  writeLines(code, path_script_r_globals(name))
}

write_targets_r_targets <- function(name, code) {
  dir_create(path_script_r_targets_dir())
  writeLines(code, path_script_r_targets(name))
}

warn_duplicate_labels <- function() {
  if (identical(getOption("knitr.duplicate.label"), "allow")) {
    warn_validate(
      "knitr.duplicate.label is set to \"allow\". Duplicate labels ",
      "interfere with the proper execution of Target Markdown. ",
      "Please set knitr.duplicate.label to a value other than \"allow\" ",
      "to prohibit duplicate knitr chunk labels."
    )
  }
}

tar_engine_set <- function() {
  if (requireNamespace("knitr", quietly = TRUE)) {
    knitr::knit_engines$set(targets = function(options) tar_engine(options))
  }
}
