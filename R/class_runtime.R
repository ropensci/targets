runtime_init <- function(
  target = NULL,
  frames = NULL,
  interactive = NULL,
  script = NULL,
  store = NULL,
  working_directory = NULL,
  fun = NULL,
  gcp_auth = NULL
) {
  runtime_new(
    target = target,
    frames = frames,
    interactive = interactive,
    script = script,
    store = store,
    working_directory = working_directory,
    fun = fun,
    gcp_auth = gcp_auth
  )
}

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
  runtime_class$new(
    target = target,
    frames = frames,
    interactive = interactive,
    script = script,
    store = store,
    working_directory = working_directory,
    fun = fun,
    gcp_auth = gcp_auth
  )
}

runtime_class <- R6::R6Class(
  classname = "tar_runtime",
  class = FALSE,
  portable = FALSE,
  cloneable = FALSE,
  public = list(
    target = NULL,
    frames = NULL,
    interactive = NULL,
    script = NULL,
    store = NULL,
    working_directory = NULL,
    fun = NULL,
    gcp_auth = NULL,
    initialize = function(
      target = NULL,
      frames = NULL,
      interactive = NULL,
      script = NULL,
      store = NULL,
      working_directory = NULL,
      fun = NULL,
      gcp_auth = NULL
    ) {
      self$target <- target
      self$frames <- frames
      self$interactive <- interactive
      self$script <- script
      self$store <- store
      self$working_directory <- working_directory
      self$fun <- fun
      self$gcp_auth <- gcp_auth
    },
    validate = function() {
      if (!is.null(self$target)) {
        tar_assert_inherits(self$target, "tar_target")
        target_validate(self$target)
      }
      if (!is.null(self$frames)) {
        frames_validate(self$frames)
      }
      if (!is.null(self$interactive)) {
        tar_assert_scalar(self$interactive)
        tar_assert_lgl(self$interactive)
      }
      if (!is.null(self$script)) {
        tar_assert_scalar(self$script)
        tar_assert_chr(self$script)
        tar_assert_nzchar(self$script)
      }
      if (!is.null(self$store)) {
        tar_assert_scalar(self$store)
        tar_assert_chr(self$store)
        tar_assert_nzchar(self$store)
      }
      if (!is.null(self$working_directory)) {
        tar_assert_scalar(self$working_directory)
        tar_assert_chr(self$working_directory)
        tar_assert_nzchar(self$working_directory)
      }
      if (!is.null(self$fun)) {
        tar_assert_scalar(self$fun)
        tar_assert_chr(self$fun)
        tar_assert_nzchar(self$fun)
      }
      if (!is.null(self$gcp_auth)) {
        tar_assert_scalar(self$gcp_auth)
        tar_assert_lgl(self$gcp_auth)
        tar_assert_nzchar(self$gcp_auth)
      }
    }
  )
)

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

tar_runtime <- runtime_init()
