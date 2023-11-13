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
    get_key = function(store) {
      "example_key"
    },
    get_bucket = function(store) {
      "example_bucket"
    },
    get_name = function(store) {
      key <- self$get_key(store)
      bucket <- self$get_bucket(store)
      paste(bucket, key, sep = "|")
    },
    get_cache = function(store) {
      name <- self$get_name(store)
      if (!exists(x = name, envir = self$cache)) {
        self$set_cache(store)
      }
      self$cache[[name]]
    },
    list_cache = function() {
      names(self$cache)
    },
    set_cache = function(store) {
      name <- self$get_name(store)
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
