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

workspace_save <- function(workspace, path_store) {
  path <- path_workspace(path_store, target_get_name(workspace$target))
  dir_create(path_workspaces_dir(path_store))
  saveRDS(object = workspace, file = path)
}

workspace_read <- function(name, path_store) {
  path <- path_workspace(path_store = path_store, name = name)
  tar_assert_store(store = path_store)
  tar_assert_path(path, paste0("no workspace found for target ", name, "."))
  readRDS(path)
}

workspace_populate <- function(workspace) {
  target_ensure_deps(workspace$target, workspace$subpipeline)
}

workspace_assign <- function(workspace, envir) {
  frames <- frames_produce(envir, workspace$target, workspace$subpipeline)
  from <- frames_get_envir(frames)
  map(names(from), ~assign(x = .x, value = from[[.x]], envir = envir))
}

workspace_load_packages <- function(workspace) {
  command <- workspace$target$command
  load_packages(packages = command$packages, library = command$library)
}

workspace_set_seed <- function(workspace) {
  tar_seed_set(workspace$target$command$seed)
}

workspace_validate <- function(workspace) {
  target_validate(workspace$target)
  pipeline_validate(workspace$subpipeline)
}
