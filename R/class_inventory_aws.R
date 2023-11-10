# This is an abstract inventory. Methods cache_key() and
# cache_prefix() are for testing only. Only the subclasses
# have serious versions of these methods.
inventory_aws_init <- function() {
  out <- inventory_new()
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
    cache_key = function(key, bucket, store) {
      path <- store$file$path
      aws <- store$resources$aws
      head <- aws_s3_head(
        key = store_aws_key(path),
        bucket = store_aws_bucket(path),
        region = store_aws_region(path),
        endpoint = store_aws_endpoint(path),
        version = store_aws_version_use(store, path),
        args = aws$args,
        max_tries = aws$max_tries,
        seconds_timeout = aws$seconds_timeout,
        close_connection = aws$close_connection,
        s3_force_path_style = aws$s3_force_path_style
      )
      digest_chr64(head$ETag)
      name <- self$name(key = key, bucket = bucket)
      self$cache[[name]] <- digest_chr64(head$ETag)
    },
    cache_prefix = function(key, bucket, store) {
      path <- store$file$path
      bucket <- store_aws_bucket(path)
      aws <- store$resources$aws
      results <- aws_s3_list_etags(
        prefix = dirname(store_aws_key(path)),
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
        name <- self$name(key = key, bucket = bucket)
        self$cache[[name]] <- digest_chr64(results[[key]])
      }
    }
  )
)
