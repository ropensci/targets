cue_init <- function(
  mode = "thorough",
  command = TRUE,
  depend = TRUE,
  format = TRUE,
  iteration = TRUE,
  file = TRUE
) {
  cue_new(
    mode = mode,
    command = command,
    depend = depend,
    format = format,
    iteration = iteration,
    file = file
  )
}

cue_new <- function(
  mode = NULL,
  command = NULL,
  depend = NULL,
  format = NULL,
  iteration = NULL,
  file = NULL
) {
  force(mode)
  force(command)
  force(depend)
  force(format)
  force(file)
  force(iteration)
  environment()
}

cue_record <- function(cue, target, meta) {
  if (!meta$exists_record(target_get_name(target))) {
    return(TRUE)
  }
  record <- meta$get_record(target_get_name(target))
  if (record_has_error(record)) {
    # Not sure why covr does not catch this.
    # A test in tests/testthat/test-class_builder.R # nolint
    # definitely covers it (errored targets are always outdated).
    return(TRUE) # nocov
  }
  if (!identical(record$type, target_get_type(target))) {
    # Again, not sure why covr does not catch this.
    # A test in tests/testthat/test-class_cue.R # nolint
    # definitely covers it (conflicting import and target).
    return(TRUE) # nocov
  }
  FALSE
}

cue_always <- function(cue, target, meta) {
  identical(cue$mode, "always")
}

cue_never <- function(cue, target, meta) {
  identical(cue$mode, "never")
}

cue_command <- function(cue, target, meta) {
  if (!cue$command) {
    return(FALSE)
  }
  old <- meta$get_record(target_get_name(target))$command
  new <- target$command$hash
  !identical(old, new)
}

cue_depend <- function(cue, target, meta) {
  if (!cue$depend) {
    return(FALSE)
  }
  old <- meta$get_record(target_get_name(target))$depend
  new <- meta$get_depend(target_get_name(target))
  !identical(old, new)
}

cue_format <- function(cue, target, meta) {
  if (!cue$format) {
    return(FALSE)
  }
  old <- meta$get_record(target_get_name(target))$format
  new <- target$settings$format
  !identical(old, new)
}

cue_iteration <- function(cue, target, meta) {
  if (!cue$iteration) {
    return(FALSE)
  }
  old <- meta$get_record(target_get_name(target))$iteration
  new <- target$settings$iteration
  !identical(old, new)
}

cue_file <- function(cue, target, meta) {
  if (!cue$file) {
    return(FALSE)
  }
  record <- meta$get_record(target_get_name(target))
  file_current <- target$store$file
  file_recorded <- file_new(
    path = record$path,
    hash = record$data,
    time = record$time,
    size = record$size,
    bytes = record$bytes
  )
  on.exit(target$store$file <- file_current)
  target$store$file <- file_recorded
  !store_has_correct_hash(target$store)
}

cue_validate <- function(cue) {
  assert_correct_fields(cue, cue_new)
  assert_chr(cue$mode)
  assert_in(cue$mode, c("thorough", "always", "never"))
  assert_lgl(cue$command)
  assert_lgl(cue$depend)
  assert_lgl(cue$format)
  assert_lgl(cue$iteration)
  assert_scalar(cue$mode)
  assert_scalar(cue$command)
  assert_scalar(cue$depend)
  assert_scalar(cue$format)
  assert_scalar(cue$iteration)
}

cue_default <- cue_init()
