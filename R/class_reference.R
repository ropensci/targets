reference_new <- function(parent, path = NULL, stage = NULL) {
  c(parent = parent, path = path, stage = stage)
}

reference_parent <- function(reference) {
  .subset(reference, "parent")
}

reference_path <- function(reference) {
  .subset(reference, "path")
}

reference_stage <- function(reference) {
  .subset2(reference, "stage")
}
