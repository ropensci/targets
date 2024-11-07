# Covered in tests/gcp/test-class_inventory_gcp.R.
# nocov start
inventory_gcp_init <- function() {
  out <- inventory_gcp_new()
  out$reset()
  out
}

inventory_gcp_new <- function() {
  inventory_gcp_class$new()
}

inventory_gcp_class <- R6::R6Class(
  classname = "tar_inventory_gcp",
  inherit = inventory_class,
  class = FALSE,
  portable = FALSE,
  cloneable = FALSE,
  public = list(
    get_key = function(store, file) {
      store_gcp_key(file$path)
    },
    get_bucket = function(store, file) {
      store_gcp_bucket(file$path)
    },
    set_cache = function(store, file) {
      path <- file$path
      bucket <- self$get_bucket(store, file)
      gcp <- store$resources$gcp
      results <- gcp_gcs_list_md5s(
        prefix = dirname(self$get_key(store, file)),
        bucket = bucket,
        verbose = gcp$verbose,
        max_tries = gcp$max_tries
      )
      for (key in names(results)) {
        name <- self$get_name(key = key, bucket = bucket)
        self$cache[[name]] <- hash_object(results[[key]])
      }
    }
  )
)
# nocov end
