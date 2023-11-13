# This is an abstract inventory. The definitions of
# get_key(), get_bucket(), and set_cache() below
# are abstract and for testing only. Only the subclasses
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
    prefixes = NULL,
    misses = NULL,
    downloads = NULL,
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
      prefix <- dirname(key)
      bucket <- self$get_bucket(store)
      name <- self$get_name(key = key, bucket = bucket)
      miss <- !exists(x = name, envir = self$cache)
      download <- !counter_exists_name(counter = self$prefixes, name = prefix)
      if (download) {
        counter_set_name(counter = self$prefixes, name = prefix)
        self$set_cache(store)
      }
      self$misses <- self$misses + as.integer(miss)
      self$downloads <- self$downloads + as.integer(download)
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
    reset = function() {
      self$cache <- new.env(parent = emptyenv())
      self$prefixes <- counter_init()
      self$misses <- 0L
      self$downloads <- 0L
    },
    validate = function() {
      tar_assert_envir(self$cache)
      invisible()
    }
  )
)
