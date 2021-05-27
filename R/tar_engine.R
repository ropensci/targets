tar_engine <- function(options, prototype = interactive()) {
  assert_package("knitr")
  assert_list(options, "knitr chunk options must be a list.")
  assert_chr(options$label, "knitr chunk must have a label")
  assert_nzchar(options$label, "knitr chunk label must not be empty")
  warn_duplicate_labels()
  if_any(
    identical(options$targets, FALSE),
    tar_engine_globals(options, prototype),
    tar_engine_targets(options, prototype)
  )
}

tar_engine_globals <- function(options, prototype) {
  if_any(
    prototype,
    tar_engine_globals_prototype(options),
    tar_engine_globals_construct(options)
  )
}

tar_engine_targets <- function(options, prototype) {
  if_any(
    prototype,
    tar_engine_targets_prototype(options),
    tar_engine_targets_construct(options)
  )
}

tar_engine_globals_prototype <- function(options) {
  eval(parse(text = options$code), envir = tar_option_get("envir"))
  tar_engine_output(options, "Created new globals accessible by all targets.")
}

tar_engine_globals_construct <- function(options) {
  write_targets_r()
  write_targets_r_globals(options$label, options$code)
  out <- paste("Wrote _targets.R and", path_script_r_globals(options$label))
  tar_engine_output(options, out)
}

tar_engine_targets_prototype <- function(options) {
  throw_validate("Need tar_make_prototype() for this.")
}

tar_engine_targets_construct <- function(options) {
  write_targets_r()
  write_targets_r_targets(options$label, options$code)
  out <- paste("Wrote _targets.R and", path_script_r_targets(options$label))
  tar_engine_output(options, out)
}

tar_engine_output <- function(options, out) {
  code <- paste(options$code, collapse = "\n")
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
