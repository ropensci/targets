cue_init <- function(
  mode = "thorough",
  command = TRUE,
  depend = TRUE,
  format = TRUE,
  repository = TRUE,
  iteration = TRUE,
  file = TRUE,
  seed = TRUE
) {
  cue_new(
    mode = mode,
    command = command,
    depend = depend,
    format = format,
    repository = repository,
    iteration = iteration,
    file = file,
    seed = seed
  )
}

cue_new <- function(
  mode = NULL,
  command = NULL,
  depend = NULL,
  format = NULL,
  repository = NULL,
  iteration = NULL,
  file = NULL,
  seed = NULL
) {
  out <- new.env(parent = emptyenv(), hash = FALSE)
  out$mode <- mode
  out$command <- command
  out$depend <- depend
  out$format <- format
  out$repository <- repository
  out$iteration <- iteration
  out$file <- file
  out$seed <- seed
  enclass(out, cue_s3_class)
}

cue_s3_class <- "tar_cue"

cue_meta_exists <- function(cue, target, meta) {
  !.subset2(meta, "exists_record")(target_get_name(target))
}

cue_meta <- function(cue, target, meta, row) {
  if (row_has_error(row)) {
    # Not sure why covr does not catch this.
    # A test in tests/testthat/test-class_builder.R # nolint
    # definitely covers it (errored targets are always outdated).
    return(TRUE) # nocov
  }
  .subset2(row, "type") != target_get_type(target)
}

cue_always <- function(cue, target, meta) {
  .subset2(cue, "mode") == "always"
}

cue_never <- function(cue, target, meta) {
  .subset2(cue, "mode") == "never"
}

cue_command <- function(cue, target, meta, row) {
  if (!.subset2(cue, "command")) {
    return(FALSE)
  }
  .subset2(.subset2(target, "command"), "hash") != .subset2(row, "command")
}

cue_depend <- function(cue, target, meta, row) {
  if (!.subset2(cue, "depend")) {
    return(FALSE)
  }
  name <- target_get_name(target)
  .subset2(meta, "get_depend")(name) != .subset2(row, "depend")
}

cue_format <- function(cue, target, meta, row) {
  if (!.subset2(cue, "format")) {
    return(FALSE)
  }
  new <- .subset2(.subset2(target, "settings"), "format")
  old <- .subset2(row, "format")
  if (new == old) {
    return(FALSE)
  }
  if (new == "auto") {
    return(!(old %in% c("file", "qs")))
  } else {
    return(TRUE)
  }
}

cue_repository <- function(cue, target, meta, row) {
  if (!.subset2(cue, "repository")) {
    return(FALSE)
  }
  .subset2(.subset2(target, "settings"), "repository") !=
    .subset(row, "repository")
}

cue_iteration <- function(cue, target, meta, row) {
  if (!.subset2(cue, "iteration")) {
    return(FALSE)
  }
  .subset2(.subset2(target, "settings"), "iteration") !=
    .subset(row, "iteration")
}

cue_file <- function(cue, target, meta, row) {
  if (!.subset2(cue, "file")) {
    return(FALSE)
  }
  path <- store_path_from_name(
    store = .subset2(target, "store"),
    format = .subset2(row, "format"),
    name = target_get_name(target),
    path = .subset2(row, "path"),
    path_store = .subset2(meta, "store")
  )
  file_current <- .subset2(target, "file")
  file_recorded <- file_new(
    path = path,
    hash = .subset2(row, "data"),
    time = .subset2(row, "time"),
    size = .subset2(row, "size"),
    bytes = .subset2(row, "bytes")
  )
  on.exit(target$file <- file_current)
  target$file <- file_recorded
  !store_has_correct_hash(.subset2(target, "store"), .subset2(target, "file"))
}

cue_seed <- function(cue, target, meta, row) {
  if (!.subset2(cue, "seed")) {
    return(FALSE)
  }
  old <- as.integer(.subset2(row, "seed"))
  new <- as.integer(.subset2(target, "seed"))
  anyNA(new) || (new != old)
}

cue_validate <- function(cue) {
  tar_assert_correct_fields(cue, cue_new)
  tar_assert_chr(cue$mode)
  tar_assert_in(cue$mode, c("thorough", "always", "never"))
  tar_assert_lgl(cue$command)
  tar_assert_lgl(cue$depend)
  tar_assert_lgl(cue$format)
  tar_assert_lgl(cue$repository)
  tar_assert_lgl(cue$iteration)
  tar_assert_lgl(cue$file)
  tar_assert_lgl(cue$seed)
  tar_assert_scalar(cue$mode)
  tar_assert_scalar(cue$command)
  tar_assert_scalar(cue$depend)
  tar_assert_scalar(cue$format)
  tar_assert_scalar(cue$repository)
  tar_assert_scalar(cue$iteration)
  tar_assert_scalar(cue$file)
  tar_assert_scalar(cue$seed)
}

#' @export
print.tar_cue <- function(x, ...) {
  cat("<tar_cue>\n ", paste0(paste_list(as.list(x)), collapse = "\n  "))
}
