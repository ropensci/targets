path_script <- function() {
  tar_config$get_script()
}

path_script_default <- function() {
  "_targets.R"
}

path_script_r <- function() {
  paste0(tools::file_path_sans_ext(path_script()), "_r")
}

path_script_r_globals_dir <- function() {
  file.path(path_script_r(), "globals")
}

path_script_r_globals <- function(name) {
  file.path(path_script_r_globals_dir(), paste0(name, ".R"))
}

path_script_r_targets_dir <- function() {
  file.path(path_script_r(), "targets")
}

path_script_r_targets <- function(name) {
  file.path(path_script_r_targets_dir(), paste0(name, ".R"))
}

path_store <- function() {
  tar_config$get_store()
}

path_store_default <- function() {
  "_targets"
}

path_objects <- function(name) {
  file.path(path_objects_dir(), name)
}

path_objects_dir <- function() {
  file.path(path_store(), "objects")
}

path_objects_dir_cloud <- function() {
  file.path(path_store_default(), "objects", fsep = "/")
}

path_meta_dir <- function() {
  file.path(path_store(), "meta")
}

path_meta <- function() {
  file.path(path_meta_dir(), "meta")
}

path_gitignore <- function() {
  file.path(path_meta_dir(), ".gitignore")
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

path_scratch_fixed <- function(name) {
  file.path(path_scratch_dir(), name)
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
