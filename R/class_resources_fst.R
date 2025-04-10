resources_fst_init <- function(
  compress = 50
) {
  resources_fst_new(
    compress = compress
  )
}

resources_fst_new <- function(
  compress = NULL
) {
  out <- new.env(parent = emptyenv(), hash = FALSE)
  out$compress <- compress
  enclass(out, resources_fst_s3_class)
}

resources_fst_s3_class <- c("tar_resources_fst", "tar_resources")

#' @export
resources_validate.tar_resources_fst <- function(resources) {
  tar_assert_scalar(resources$compress)
  tar_assert_dbl(resources$compress)
}

#' @export
print.tar_resources_fst <- function(x, ...) {
  cat(
    "<tar_resources_fst>\n ",
    paste0(paste_list(as.list(x)), collapse = "\n  ")
  )
}
