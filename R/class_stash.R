stash_init <- function() {
  stash_new(new.env(parent = emptyenv()), new.env(parent = emptyenv()))
}

stash_new <- function(
  caches = NULL,
  values = NULL
) {
  force(caches)
  force(values)
  environment()
}

stash_get_cache <- function(stash, name) {
  get(name, envir = stash$caches)
}

stash_get_value <- function(stash, name) {
  get(name, envir = stash$values)
}

stash_set_cache <- function(stash, name, cache) {
  assign(name, cache, envir = stash$caches)
}

stash_set_value <- function(stash, name, value) {
  assign(name, value, envir = stash$values)
}

stash_del_cache <- function(stash, name) {
  remove(list = name, envir = stash$caches)
}

stash_del_value <- function(stash, name) {
  remove(list = name, envir = stash$values)
}

stash_cache <- function(stash, name, pipeline) {
  target <- pipeline_get_target(pipeline, name)
  cache <- target$cache
  stash_set_cache(stash, name, cache)
  target$cache <- NULL
}

stash_caches <- function(stash, pipeline) {
  lapply(
    pipeline_get_names(pipeline),
    stash_cache,
    stash = stash,
    pipeline = pipeline
  )
}

stash_value <- function(stash, name, pipeline) {
  target <- pipeline_get_target(pipeline, name)
  value <- target$value
  stash_set_value(stash, name, value)
  target$value <- NULL
}

stash_values <- function(stash, pipeline) {
  lapply(
    pipeline_get_names(pipeline),
    stash_value,
    stash = stash,
    pipeline = pipeline
  )
}

stash_targets <- function(stash, pipeline) {
  stash_caches(stash, pipeline)
  stash_values(stash, pipeline)
}

stash_restore_cache <- function(stash, name, pipeline) {
  target <- pipeline_get_target(pipeline, name)
  cache <- stash_get_cache(stash, name)
  target$cache <- cache
  stash_del_cache(stash, name)
}

stash_restore_caches <- function(stash, pipeline) {
  lapply(
    names(stash$caches),
    stash_restore_cache,
    stash = stash,
    pipeline = pipeline
  )
}

stash_restore_value <- function(stash, name, pipeline) {
  target <- pipeline_get_target(pipeline, name)
  value <- stash_get_value(stash, name)
  target$value <- value
  stash_del_value(stash, name)
}

stash_restore_values <- function(stash, pipeline) {
  lapply(
    names(stash$values),
    stash_restore_value,
    stash = stash,
    pipeline = pipeline
  )
}

stash_restore_targets <- function(stash, pipeline) {
  stash_restore_caches(stash, pipeline)
  stash_restore_values(stash, pipeline)
}

stash_validate <- function(stash) {
  assert_correct_fields(stash, stash_new)
  assert_envir(stash$caches)
  assert_envir(stash$values)
}
