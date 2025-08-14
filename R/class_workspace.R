workspace_init <- function(target, pipeline) {
  target <- target_workspace_copy(target)
  target$settings$retrieval <- "worker"
  subpipeline <- pipeline_produce_subpipeline(
    pipeline,
    target,
    keep_value = FALSE
  )
  workspace_new(target = target, subpipeline = subpipeline)
}

workspace_new <- function(target = NULL, subpipeline = NULL) {
  out <- new.env(parent = emptyenv(), hash = FALSE)
  out$target <- target
  out$subpipeline <- subpipeline
  out
}

workspace_save <- function(workspace, path_store) {
  path <- path_workspace(path_store, target_get_name(workspace$target))
  dir_create(path_workspaces_dir(path_store))
  saveRDS(object = workspace, file = path)
}

workspace_read <- function(name, path_store) {
  path <- path_workspace(path_store = path_store, name = name)
  tar_assert_store(store = path_store)
  tar_assert_path(
    path,
    paste0(
      "no workspace found for target ",
      name,
      ". If your pipeline uses cloud storage, you may need to run ",
      "tar_workspace_download(",
      name,
      ") first to download the workspace locally, ",
      "then try tar_workspace(",
      name,
      ") again."
    )
  )
  readRDS(path)
}

workspace_populate <- function(workspace) {
  target_ensure_deps_worker(workspace$target, workspace$subpipeline)
}

workspace_assign <- function(workspace, envir) {
  frames <- frames_produce(envir, workspace$target, workspace$subpipeline)
  from <- frames_get_envir(frames)
  map(names(from), ~ assign(x = .x, value = from[[.x]], envir = envir))
}

workspace_load_packages <- function(workspace) {
  command <- workspace$target$command
  load_packages(packages = command$packages, library = command$library)
}

workspace_set_seed <- function(workspace) {
  tar_seed_set(workspace$target$seed)
}

workspace_validate <- function(workspace) {
  target_validate(workspace$target)
  pipeline_validate(workspace$subpipeline)
}
