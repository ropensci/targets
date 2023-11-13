path_script_default <- function() {
  "_targets.R"
}

path_script_r <- function(path_script) {
  paste0(tools::file_path_sans_ext(path_script), "_r")
}

path_script_r_globals_dir <- function(path_script) {
  file.path(path_script_r(path_script), "globals")
}

path_script_r_globals <- function(path_script, name) {
  file.path(path_script_r_globals_dir(path_script), paste0(name, ".R"))
}

path_script_r_targets_dir <- function(path_script) {
  file.path(path_script_r(path_script), "targets")
}

path_script_r_targets <- function(path_script, name) {
  file.path(path_script_r_targets_dir(path_script), paste0(name, ".R"))
}

path_store_default <- function() {
  "_targets"
}

path_gitignore <- function(path_store) {
  file.path(path_store, ".gitignore")
}

path_objects <- function(path_store, name) {
  file.path(path_objects_dir(path_store), name)
}

path_objects_dir <- function(path_store) {
  path_objects_dir(path_store = path_store)
}

path_objects_dir <- function(path_store) {
  file.path(path_store, "objects")
}

path_objects_dir_cloud <- function() {
  file.path(path_store_default(), "objects", fsep = "/")
}

path_meta_dir <- function(path_store) {
  file.path(path_store, "meta")
}

path_meta <- function(path_store) {
  file.path(path_meta_dir(path_store), "meta")
}

path_progress <- function(path_store) {
  file.path(path_meta_dir(path_store), "progress")
}

path_process <- function(path_store) {
  file.path(path_meta_dir(path_store), "process")
}

path_crew <- function(path_store) {
  file.path(path_meta_dir(path_store), "crew")
}

path_scratch <- function(path_store, pattern = "tmp") {
  file.path(path_scratch_dir(path_store), pattern)
}

path_scratch_dir <- function(path_store) {
  file.path(path_store, "scratch")
}

path_scratch_del <- function(path_store) {
  unlink(path_scratch_dir(path_store), recursive = TRUE)
}

path_scratch_dir_network <- function() {
  file.path(tempdir(), path_scratch_dir(path_store_default()))
}

path_scratch_temp_network <- function(pattern = "tmp") {
  tempfile(tmpdir = path_scratch_dir_network(), pattern = pattern)
}

path_user_dir <- function(path_store) {
  file.path(path_store, "user")
}

path_workspace <- function(path_store, name) {
  file.path(path_workspaces_dir(path_store), name)
}

path_workspaces_dir <- function(path_store) {
  file.path(path_store, "workspaces")
}
