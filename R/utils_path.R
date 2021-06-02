path_script_default <- function() {
  "_targets.R"
}

#' @title Target script helper directory path.
#' @export
#' @keywords internal
#' @description Internal function for Target Markdown.
#'   Not a user-side function. Do not invoke directly.
#' @return Character of length 1, target script
#'   helper directory path.
#' @param script Character of length 1, path to the
#'   target script file (default: `_targets.R`).
#' @examples
#' path_script_r()
path_script_r <- function(script) {
  paste0(tools::file_path_sans_ext(script), "_r")
}

path_script_r_globals_dir <- function(script) {
  file.path(path_script_r(script), "globals")
}

path_script_r_globals <- function(script, name) {
  file.path(path_script_r_globals_dir(script), paste0(name, ".R"))
}

path_script_r_targets_dir <- function(script) {
  file.path(path_script_r(script), "targets")
}

path_script_r_targets <- function(script, name) {
  file.path(path_script_r_targets_dir(script), paste0(name, ".R"))
}

path_store_default <- function() {
  "_targets"
}

path_objects <- function(store, name) {
  file.path(path_objects_dir(store), name)
}

path_objects_dir <- function(store) {
  file.path(store, "objects")
}

path_objects_dir_cloud <- function() {
  file.path(path_store_default(), "objects", fsep = "/")
}

path_meta_dir <- function(store) {
  file.path(store, "meta")
}

path_meta <- function(store) {
  file.path(path_meta_dir(store), "meta")
}

path_gitignore <- function(store) {
  file.path(path_meta_dir(store), ".gitignore")
}

path_progress <- function(store) {
  file.path(path_meta_dir(store), "progress")
}

path_process <- function(store) {
  file.path(path_meta_dir(store), "process")
}

path_scratch <- function(store, pattern = "") {
  tempfile(pattern = pattern, tmpdir = path_scratch_dir(store))
}

path_scratch_fixed <- function(store, name) {
  file.path(path_scratch_dir(store), name)
}

path_scratch_dir <- function(store) {
  file.path(store, "scratch")
}

path_scratch_del <- function(store) {
  unlink(path_scratch_dir(store), recursive = TRUE)
}

path_workspace <- function(store, name) {
  file.path(path_workspaces_dir(store), name)
}

path_workspaces_dir <- function(store) {
  file.path(store, "workspaces")
}
