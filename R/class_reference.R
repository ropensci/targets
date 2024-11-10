reference_init <- function(
  parent = NA_character_,
  path = NA_character_,
  stage = NA_character_,
  hash = NA_character_
) {
  reference_new(parent = parent, path = path, stage = stage, hash = hash)
}

reference_new <- function(
  parent = NULL,
  path = NULL,
  stage = NULL,
  hash = NULL
) {
  c(parent = parent, path = path, stage = stage, hash = hash)
}

reference_parent <- function(reference) {
  as.character(.subset(reference, 1L))
}

reference_path <- function(reference) {
  as.character(.subset(reference, 2L))
}

reference_stage <- function(reference) {
  as.character(.subset(reference, 3L))
}

reference_hash <- function(reference) {
  as.character(.subset(reference, 4L))
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

is_reference <- is.character
