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
  enclass(environment(), "tar_resources_qs")
}

#' @export
resources_validate.tar_resources_qs <- function(resources) {
  assert_scalar(resources$preset)
  assert_chr(resources$preset)
}

#' @export
print.tar_resources_qs <- function(x, ...) {
  cat(
    "<tar_resources_qs>\n ",
    paste0(paste_list(as.list(x)), collapse = "\n  ")
  )
}
