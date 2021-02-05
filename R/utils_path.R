path_script <- function() {
  path_script_basename()
}

path_script_basename <- function() {
  "_targets.R"
}

path_store <- function() {
  "_targets"
}

path_objects <- function(name) {
  file.path(path_objects_dir(), name)
}

path_objects_dir <- function() {
  file.path(path_store(), "objects")
}

path_meta_dir <- function() {
  file.path(path_store(), "meta")
}

path_meta <- function() {
  file.path(path_meta_dir(), "meta")
}

path_progress <- function() {
  file.path(path_meta_dir(), "progress")
}

path_process <- function() {
  file.path(path_meta_dir(), "process")
}

path_scratch <- function(pattern = "") {
  tempfile(pattern = pattern, tmpdir = path_scratch_dir())
}

path_scratch_dir <- function() {
  file.path(path_store(), "scratch")
}

path_scratch_del <- function() {
  unlink(path_scratch_dir(), recursive = TRUE)
}

path_workspace <- function(name) {
  file.path(path_workspaces_dir(), name)
}

path_workspaces_dir <- function() {
  file.path(path_store(), "workspaces")
}
