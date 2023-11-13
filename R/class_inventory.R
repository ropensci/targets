# This is an abstract inventory. The definitions of
# get_key(), get_bucket(), and set_cache() below
# are abstract and for testing only. Only the subclasses
# have serious versions of these methods.
inventory_init <- function() {
  out <- inventory_new()
  out$reset_cache()
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
    misses = NULL,
    get_key = function(store) {
      "example_key"
    },
    get_bucket = function(store) {
      "example_bucket"
    },
    get_name = function(key, bucket) {
      paste(bucket, key, sep = "|")
    },
    get_cache = function(store) {
      key <- self$get_key(store)
      bucket <- self$get_bucket(store)
      name <- self$get_name(key = key, bucket = bucket)
      if (!exists(x = name, envir = self$cache)) {
        self$misses <- (self$misses %|||% 0L) + 1L
        self$set_cache(store)
      }
      self$cache[[name]]
    },
    list_cache = function() {
      names(self$cache)
    },
    set_cache = function(store) {
      key <- self$get_key(store)
      bucket <- self$get_bucket(store)
      name <- self$get_name(key = key, bucket = bucket)
      self$cache[[name]] <- "example_hash"
    },
    reset_cache = function() {
      self$cache <- new.env(parent = emptyenv())
    },
    validate = function() {
      tar_assert_envir(self$cache)
      invisible()
    }
  )
)
