workspace_init <- function(target, pipeline) {
  target <- target_workspace_copy(target)
  subpipeline <- pipeline_produce_subpipeline(
    pipeline,
    target_get_name(target),
    keep_value = FALSE
  )
  workspace_new(target = target, subpipeline = subpipeline)
}

workspace_new <- function(target = NULL, subpipeline = NULL) {
  force(target)
  force(subpipeline)
  environment()
}

workspace_save <- function(workspace) {
  path <- path_workspace(target_get_name(workspace$target))
  dir_create(path_workspaces_dir())
  saveRDS(object = workspace, file = path)
}

workspace_read <- function(name) {
  path <- path_workspace(name)
  assert_path(path, paste0("no workspace found for target ", name, "."))
  readRDS(path)
}

workspace_populate <- function(workspace) {
  target_ensure_deps(workspace$target, workspace$subpipeline)
  target_cache_deps(workspace$target, workspace$subpipeline)
}

workspace_assign <- function(workspace, envir) {
  from <- cache_get_envir(workspace$target$cache)
  map(names(from), ~assign(x = .x, value = from[[.x]], envir = envir))
}

workspace_validate <- function(workspace) {
  target_validate(workspace$target)
  pipeline_validate(workspace$subpipeline)
}
