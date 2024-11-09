reference_new <- function(parent, path = NULL, stage = NULL) {
  c(parent = parent, path = path, stage = stage)
}

reference_parent <- function(reference) {
  as.character(.subset(reference, "parent"))
}

reference_path <- function(reference) {
  as.character(.subset(reference, "path"))
}

reference_stage <- function(reference) {
  as.character(.subset2(reference, "stage"))
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
