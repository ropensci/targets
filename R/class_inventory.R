# This is an abstract inventory. Methods cache_key() and
# cache_prefix() are for testing only. Only the subclasses
# have serious versions of these methods.
inventory_init <- function() {
  out <- inventory_new()
  out$reset()
  out
}

inventory_new <- function() {
  inventory_class$new()
}

inventory_class <- R6::R6Class(
  classname = "tar_inventory",
  class = FALSE,
  portable = FALSE,
  cloneable = FALSE,
  public = list(
    cache = NULL,
    cache_key = function(key, bucket, store) {
      name <- self$name(key = key, bucket = bucket)
      self$cache[[name]] <- paste(store$file$hash, "key")
    },
    cache_prefix = function(key, bucket, store) {
      name <- self$name(key = key, bucket = bucket)
      self$cache[[name]] <- paste(store$file$hash, "prefix")
    },
    name = function(key, bucket) {
      paste(bucket, key, sep = "|")
    },
    list = function() {
      names(self$cache)
    },
    reset = function() {
      self$cache <- new.env(parent = emptyenv())
    },
    get = function(key, bucket, version, store) {
      name <- self$name(key = key, bucket = bucket)
      if (!exists(x = name, envir = self$cache)) {
        if (is.null(version)) {
          self$cache_prefix(
            key = key,
            bucket = bucket,
            store = store
          )
        } else {
          self$cache_key(
            key = key,
            bucket = bucket,
            store = store
          )
        }
      } 
      self$cache[[name]]
    },
    validate = function() {
      tar_assert_envir(self$cache)
      invisible()
    }
  )
)
