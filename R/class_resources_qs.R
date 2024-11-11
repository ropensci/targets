resources_qs_init <- function(
  compress_level = 3L,
  shuffle = TRUE,
  nthreads = 1L
) {
  out <- resources_qs_new(
    compress_level = compress_level,
    shuffle = shuffle,
    nthreads = nthreads
  )
  resources_validate(out)
  out
}

resources_qs_new <- function(
  compress_level = NULL,
  shuffle = NULL,
  nthreads = NULL
) {
  out <- new.env(parent = emptyenv(), hash = FALSE)
  out$compress_level <- compress_level
  out$shuffle <- shuffle
  out$nthreads <- nthreads
  enclass(out, resources_qs_s3_class)
}

resources_qs_s3_class <- c("tar_resources_qs", "tar_resources")

#' @export
resources_validate.tar_resources_qs <- function(resources) {
  for (field in c("compress_level", "nthreads")) {
    tar_assert_scalar(resources[[field]])
    tar_assert_int(resources[[field]])
    tar_assert_finite(resources[[field]])
    tar_assert_none_na(resources[[field]])
    tar_assert_ge(resources[[field]], 1L)
  }
  tar_assert_scalar(resources$shuffle)
  tar_assert_lgl(resources$shuffle)
  tar_assert_none_na(resources$shuffle)
}

#' @export
print.tar_resources_qs <- function(x, ...) {
  cat(
    "<tar_resources_qs>\n ",
    paste0(paste_list(as.list(x)), collapse = "\n  ")
  )
}
