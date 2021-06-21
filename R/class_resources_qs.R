resources_qs_init <- function(
  preset = "high"
) {
  resources_qs_new(
    preset = preset
  )
}

resources_qs_new <- function(
  preset = NULL
) {
  force(preset)
  enclass(environment(), c("tar_resources_qs", "tar_resources"))
}

#' @export
resources_validate.tar_resources_qs <- function(resources) {
  tar_assert_scalar(resources$preset)
  tar_assert_chr(resources$preset)
}

#' @export
print.tar_resources_qs <- function(x, ...) {
  cat(
    "<tar_resources_qs>\n ",
    paste0(paste_list(as.list(x)), collapse = "\n  ")
  )
}
