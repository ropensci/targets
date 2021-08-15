runtime_init <- function(
  target = NULL,
  frames = NULL,
  interactive = NULL
) {
  runtime_new(
    target = target,
    frames = frames,
    interactive = interactive
  )
}

runtime_new <- function(
  target = NULL,
  frames = NULL,
  interactive = NULL
) {
  runtime_class$new(
    target = target,
    frames = frames,
    interactive = interactive
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
    initialize = function(
      target = NULL,
      frames = NULL,
      interactive = NULL
    ) {
      self$target <- target
      self$frames <- frames
      self$interactive <- interactive
    },
    exists_target = function() {
      !is.null(self$target)
    },
    exists_frames = function() {
      !is.null(self$frames)
    },
    exists_interactive = function() {
      !is.null(self$interactive)
    },
    get_target = function() {
      self$target
    },
    get_frames = function() {
      self$frames
    },
    get_interactive = function() {
      self$interactive
    },
    set_target = function(target) {
      self$target <- target
    },
    set_frames = function(frames) {
      self$frames <- frames
    },
    set_interactive = function(interactive) {
      self$interactive <- interactive
    },
    unset_target = function() {
      self$target <- NULL
    },
    unset_frames = function() {
      self$frames <- NULL
    },
    unset_interactive = function() {
      self$interactive <- NULL
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
    }
  )
)

tar_runtime <- runtime_init()
