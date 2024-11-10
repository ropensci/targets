frames_init <- function(imports = lookup_new()) {
  targets <- lookup_new(parent = imports)
  frames_new(imports, targets)
}

frames_new <- function(imports = NULL, targets = NULL) {
  out <- new.env(parent = emptyenv(), hash = FALSE)
  out$imports <- imports
  out$targets <- targets
  out
}

frames_get_envir <- function(frames) {
  .subset2(frames, "targets")
}

frames_set_object <- function(frames, name, object) {
  lookup_set(.subset2(frames, "targets"), name, object)
}

frames_clear_objects <- function(frames) {
  frames$targets <- lookup_new(parent = frames$imports)
}

frames_set_dep <- function(frames, dep, pipeline) {
  value <- dep$value
  if (!is.null(value)) {
    object <- store_copy_object(dep$store, dep$value$object)
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
  frames <- frames_init(imports = new.env(parent = envir))
  frames_set_deps(frames = frames, target = target, pipeline = pipeline)
  frames
}

frames_validate_inheritance <- function(frames) {
  envir1 <- parent.env(frames$targets)
  envir2 <- frames$imports
  if (!identical(envir1, envir2)) {
    tar_throw_validate("broken inheritance in the frames.")
  }
}

frames_validate <- function(frames) {
  tar_assert_correct_fields(frames, frames_new)
  lookup_validate(frames$imports)
  lookup_validate(frames$targets)
  frames_validate_inheritance(frames)
  invisible()
}
