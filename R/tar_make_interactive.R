tar_make_interactive <- function(code) {
  targets <- eval(parse(text = code), envir = tar_option_get("envir"))
  pipeline <- as_pipeline(targets)
  pipeline_reset_deployments(pipeline)
  queue <- if_any(
    pipeline_uses_priorities(pipeline),
    "parallel",
    "sequential"
  )
  local_init(
    pipeline = pipeline,
    meta = meta_init(path_store = tempfile()),
    queue = queue,
    reporter = "silent",
    envir = tar_option_get("envir")
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
