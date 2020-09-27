#' @export
store_read_object.tar_aws_s3 <- function(store) {
  
}

#' @export
store_write_object.tar_aws_s3 <- function(store, object) {
  
}

#' @export
store_produce_path.tar_aws_s3 <- function(store, name, object) {
  
}

#' @export
store_late_hash.tar_aws_s3 <- function(store) {
  
}

#' @export
store_wait_correct_hash.tar_aws_s3 <- function(store, remote) {
  if (!remote) {
    return()
  }
  
}

#' @export
store_validate_packages.tar_aws_s3 <- function(store) {
  assert_package("aws_s3.s3")
  NextMethod()
}
