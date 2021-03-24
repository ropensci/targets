cache_init <- function(imports = memory_init()) {
  targets <- memory_init(new.env(parent = imports$envir))
  cache_new(imports, targets)
}

cache_new <- function(imports = NULL, targets = NULL) {
  force(imports)
  force(targets)
  environment()
}

cache_get_envir <- function(cache) {
  cache$targets$envir
}

cache_set_object <- function(cache, name, object) {
  memory_set_object(cache$targets, name, object)
}

cache_clear_objects <- function(cache) {
  cache$targets <- memory_init(new.env(parent = cache$imports$envir))
}

cache_clone <- function(cache) {
  cache_new(cache$imports, cache$targets)
}

cache_set_dep <- function(cache, dep, pipeline) {
  object <- dep$value$object
  cache_set_object(cache, target_get_parent(dep), object)
}

cache_set_deps <- function(cache, target, pipeline) {
  map(
    target_deps_shallow(target, pipeline),
    ~cache_set_dep(cache, pipeline_get_target(pipeline, .x), pipeline)
  )
}

cache_produce <- function(envir, target, pipeline) {
  cache <- cache_init(memory_init(new.env(parent = envir)))
  cache_set_deps(cache = cache, target = target, pipeline = pipeline)
  cache
}

cache_validate_inheritance <- function(cache) {
  envir1 <- parent.env(cache$targets$envir)
  envir2 <- cache$imports$envir
  if (!identical(envir1, envir2)) {
    throw_validate("broken inheritance in the cache.")
  }
}

cache_validate <- function(cache) {
  assert_correct_fields(cache, cache_new)
  memory_validate(cache$imports)
  memory_validate(cache$targets)
  cache_validate_inheritance(cache)
  invisible()
}
