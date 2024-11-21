reference_new <- function(
  parent = NULL,
  path = NULL,
  stage = NULL,
  hash = NULL
) {
  out <- list(parent = parent)
  if (!is.null(path)) {
    out$path <- path
  }
  if (!is.null(stage)) {
    out$stage <- stage
  }
  if (!is.null(hash)) {
    out$hash <- hash
  }
  out
}

reference_parent <- function(reference) {
  .subset2(reference, "parent")
}

reference_path <- function(reference) {
  .subset2(reference, "path")
}

reference_stage <- function(reference) {
  .subset2(reference, "stage")
}

reference_hash <- function(reference) {
  .subset2(reference, "hash")
}

reference_produce_target <- function(reference, pipeline, name) {
  parent <- pipeline_get_target(pipeline, reference_parent(reference))
  child <- target_produce_child(parent, name)
  file <- .subset2(child, "file")
  if (!is.null(file)) {
    file$path <- reference_path(reference)
    file$stage <- reference_stage(reference)
    file$hash <- reference_hash(reference)
  }
  child
}

is_reference <- function(reference) {
  is.list(reference) && is.character(reference_parent(reference))
}
