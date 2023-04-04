resources_crew_init <- function(
  controller = NULL,
  scale = TRUE,
  seconds_timeout = NULL
) {
  resources_crew_new(
    controller = controller,
    scale = scale,
    seconds_timeout = seconds_timeout
  )
}

resources_crew_new <- function(
  controller = NULL,
  scale = NULL,
  seconds_timeout = NULL
) {
  force(controller)
  force(scale)
  force(seconds_timeout)
  enclass(environment(), c("tar_resources_crew", "tar_resources"))
}

#' @export
resources_validate.tar_resources_crew <- function(resources) {
  if (!is.null(resources$controller)) {
    tar_assert_chr(resources$controller)
    tar_assert_scalar(resources$controller)
    tar_assert_none_na(resources$controller)
    tar_assert_nzchar(resources$controller)
  }
  tar_assert_lgl(resources$scale)
  tar_assert_scalar(resources$scale)
  tar_assert_none_na(resources$scale)
  tar_assert_nzchar(resources$scale)
  if (!is.null(resources$seconds_timeout)) {
    tar_assert_dbl(resources$seconds_timeout)
    tar_assert_scalar(resources$seconds_timeout)
    tar_assert_none_na(resources$seconds_timeout)
    tar_assert_positive(resources$seconds_timeout)
  }
}

#' @export
print.tar_resources_crew <- function(x, ...) {
  cat(
    "<tar_resources_crew>\n ",
    paste0(paste_list(as.list(x)), collapse = "\n  ")
  )
}
