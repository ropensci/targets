path_objects <- function(name) {
  file.path(path_objects_dir(), name)
}

path_objects_dir <- function() {
  file.path("_targets", "objects")
}

path_scratch <- function(pattern = "") {
  tempfile(pattern = pattern, tmpdir = path_scratch_dir())
}

path_scratch_dir <- function() {
  file.path("_targets", "scratch")
}

path_scratch_del <- function() {
  unlink(path_scratch_dir(), recursive = TRUE)
}

path_workspace <- function(name) {
  file.path(path_workspaces_dir(), name)
}

path_workspaces_dir <- function() {
  file.path("_targets", "workspaces")
}
