reference_new <- function(parent, path = NULL, stage = NULL) {
  c(parent, path, stage)
}

reference_parent <- function(reference) {
  .subset(reference, 1L)
}

reference_path <- function(reference) {
  .subset(reference, 2L)
}

reference_stage <- function(reference) {
  .subset2(reference, 3L)
}

reference_produce_target <- function(reference, pipeline, name) {
  parent <- pipeline_get_target(pipeline, reference_parent(reference))
  child <- target_produce_child(parent, name)
  file <- .subset2(child, "file")
  if (!is.null(file)) {
    file$path <- reference_path(reference)
    file$stage <- reference_stage(reference)
  }
  child
}

is_reference <- is.character
