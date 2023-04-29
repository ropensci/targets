runtime_new <- function(
    target = NULL,
    frames = NULL,
    interactive = NULL,
    script = NULL,
    store = NULL,
    working_directory = NULL,
    fun = NULL,
    gcp_auth = NULL
) {
  force(target)
  force(frames)
  force(interactive)
  force(script)
  force(store)
  force(working_directory)
  force(fun)
  force(gcp_auth)
  environment()
}

runtime_validate = function(x) {
  if (!is.null(x$target)) {
    tar_assert_inherits(x$target, "tar_target")
    target_validate(x$target)
  }
  if (!is.null(x$frames)) {
    frames_validate(x$frames)
  }
  if (!is.null(x$interactive)) {
    tar_assert_scalar(x$interactive)
    tar_assert_lgl(x$interactive)
  }
  if (!is.null(x$script)) {
    tar_assert_scalar(x$script)
    tar_assert_chr(x$script)
    tar_assert_nzchar(x$script)
  }
  if (!is.null(x$store)) {
    tar_assert_scalar(x$store)
    tar_assert_chr(x$store)
    tar_assert_nzchar(x$store)
  }
  if (!is.null(x$working_directory)) {
    tar_assert_scalar(x$working_directory)
    tar_assert_chr(x$working_directory)
    tar_assert_nzchar(x$working_directory)
  }
  if (!is.null(x$fun)) {
    tar_assert_scalar(x$fun)
    tar_assert_chr(x$fun)
    tar_assert_nzchar(x$fun)
  }
  if (!is.null(x$gcp_auth)) {
    tar_assert_scalar(x$gcp_auth)
    tar_assert_lgl(x$gcp_auth)
    tar_assert_nzchar(x$gcp_auth)
  }
}

#' @title Get the `tar_runtime` object.
#' @export
#' @keywords internal
#' @description For internal purposes only. Not a user-side function.
#'   Do not invoke directly.
#' @details Manages internal settings
#'   that targets need while they run.
#' @return The internal `tar_runtime` object of class `"tar_runtime"`.
#' @examples
#' tar_runtime_object()
tar_runtime_object <- function() {
  tar_runtime
}

tar_runtime <- runtime_new()
