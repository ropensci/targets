# Covered in tests/aws/test-class_inventory_aws.R.
# nocov start
inventory_aws_init <- function() {
  out <- inventory_aws_new()
  out$reset()
  out
}

inventory_aws_new <- function() {
  inventory_aws_class$new()
}

inventory_aws_class <- R6::R6Class(
  classname = "tar_inventory_aws",
  inherit = inventory_class,
  class = FALSE,
  portable = FALSE,
  cloneable = FALSE,
  public = list(
    get_key = function(store) {
      store_aws_key(store$file$path)
    },
    get_bucket = function(store) {
      store_aws_bucket(store$file$path)
    },
    set_cache = function(store) {
      path <- store$file$path
      bucket <- self$get_bucket(store)
      aws <- store$resources$aws
      results <- aws_s3_list_etags(
        prefix = dirname(self$get_key(store)),
        bucket = bucket,
        page_size = aws$page_size,
        verbose = aws$verbose,
        region = store_aws_region(path),
        endpoint = store_aws_endpoint(path),
        args = aws$args,
        max_tries = aws$max_tries,
        seconds_timeout = aws$seconds_timeout,
        close_connection = aws$close_connection,
        s3_force_path_style = aws$s3_force_path_style
      )
      for (key in names(results)) {
        name <- self$get_name(key = key, bucket = bucket)
        self$cache[[name]] <- digest_chr64(results[[key]])
      }
    }
  )
)
# nocov end
