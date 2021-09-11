frames_init <- function(imports = memory_init()) {
  targets <- memory_init(new.env(parent = imports$envir))
  frames_new(imports, targets)
}

frames_new <- function(imports = NULL, targets = NULL) {
  force(imports)
  force(targets)
  environment()
}

frames_get_envir <- function(frames) {
  frames$targets$envir
}

frames_set_object <- function(frames, name, object) {
  memory_set_object(frames$targets, name, object)
}

frames_clear_objects <- function(frames) {
  frames$targets <- memory_init(new.env(parent = frames$imports$envir))
}

frames_set_dep <- function(frames, dep, pipeline) {
  value <- dep$value
  if (!is.null(value)) {
    object <- dep$value$object
    frames_set_object(frames, target_get_parent(dep), object)
  }
}

frames_set_deps <- function(frames, target, pipeline) {
  map(
    target_deps_shallow(target, pipeline),
    ~frames_set_dep(frames, pipeline_get_target(pipeline, .x), pipeline)
  )
}

frames_produce <- function(envir, target, pipeline) {
  frames <- frames_init(memory_init(new.env(parent = envir)))
  frames_set_deps(frames = frames, target = target, pipeline = pipeline)
  frames
}

frames_validate_inheritance <- function(frames) {
  envir1 <- parent.env(frames$targets$envir)
  envir2 <- frames$imports$envir
  if (!identical(envir1, envir2)) {
    tar_throw_validate("broken inheritance in the frames.")
  }
}

frames_validate <- function(frames) {
  tar_assert_correct_fields(frames, frames_new)
  memory_validate(frames$imports)
  memory_validate(frames$targets)
  frames_validate_inheritance(frames)
  invisible()
}
