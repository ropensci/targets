tar_make_interactive <- function(code) {
  targets <- eval(parse(text = code), envir = tar_option_get("envir"))
  pipeline <- as_pipeline(targets)
  pipeline_reset_deployments(pipeline)
  queue <- if_any(
    pipeline_uses_priorities(pipeline),
    "parallel",
    "sequential"
  )
  store <- tar_config$get_value("store")
  tar_config$force_memory(name = "store", value = tempfile())
  tar_config$set_lock()
  on.exit({
    tar_config$unset_lock()
    unlink(tar_config$get_value("store"), recursive = TRUE)
    tar_config$force_memory(name = "store", value = store)
  })
  local_init(
    pipeline = pipeline,
    queue = queue,
    reporter = "silent"
  )$run()
  map(
    pipeline_get_names(pipeline),
    tar_make_interactive_load,
    pipeline = pipeline
  )
  invisible()
}

tar_make_interactive_load <- function(name, pipeline) {
  target <- pipeline_get_target(pipeline, name)
  tar_make_interactive_load_target(target, name, pipeline)
}

tar_make_interactive_load_target <- function(target, name, pipeline) {
  UseMethod("tar_make_interactive_load_target")
}

tar_make_interactive_load_target.tar_target <- function(
  target,
  name,
  pipeline
) {
  target_load_value(target, pipeline)
  object <- target$value$object
  assign(x = name, value = object, envir = tar_option_get("envir"))
}

tar_make_interactive_load_target.tar_bud <- function(
  target,
  name,
  pipeline
) {
}
