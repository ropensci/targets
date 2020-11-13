workspace_init <- function(target, pipeline) {
  target <- target_workspace_copy(target)
  subpipeline <- pipeline_produce_subpipeline(
    pipeline,
    target_get_name(target)
  )
  workspace_new(target = target, subpipeline = subpipeline)
}

workspace_new <- function(target = NULL, subpipeline = NULL) {
  force(target)
  force(subpipeline)
  environment()
}

workspace_save <- function(workspace) {
  assert_package("qs", "saving workspaces requires the qs package.")
  workspace_serialize(workspace)
  path <- path_workspace(target_get_name(workspace$target))
  dir_create(path_workspaces_dir())
  qs::qsave(x = out, file = path, preset = "high")
}

workspace_read <- function(name) {
  assert_package("qs", "reading workspaces requires the qs package.")
  workspace <- qs::qread(path_workspace(name))
  workspace_unserialize(workspace)
}

workspace_serialize <- function(workspace) {
  target_ensure_deps(workspace$target, workspace$subpipeline)
  pipeline_serialize_values(workspace$subpipeline)
}

workspace_unserialize <- function(workspace) {
  pipeline_unserialize_values(workspace$subpipeline)
}

workspace_validate <- function(workspace) {
  target_validate(workspace$target)
  pipeline_validate(workspace$subpipeline)
}
