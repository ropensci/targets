runtime_init <- function(
  target = NULL,
  frames = NULL
) {
  runtime_new(
    target = target,
    frames = frames
  )
}

runtime_new <- function(
  target = NULL,
  frames = NULL
) {
  runtime_class$new(
    target = target,
    frames = frames
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
    initialize = function(
      target = NULL,
      frames = NULL
    ) {
      self$target <- target
      self$frames <- frames
    },
    exists_target = function() {
      !is.null(self$target)
    },
    exists_frames = function() {
      !is.null(self$frames)
    },
    get_target = function() {
      self$target
    },
    get_frames = function() {
      self$frames
    },
    set_target = function(target) {
      self$target <- target
    },
    set_frames = function(frames) {
      self$frames <- frames
    },
    unset_target = function() {
      self$target <- NULL
    },
    unset_frames = function() {
      self$frames <- NULL
    },
    validate = function() {
      if (!is.null(self$target)) {
        tar_assert_inherits(self$target, "tar_target")
        target_validate(self$target)
      }
      if (!is.null(self$frames)) {
        frames_validate(self$frames)
      }
    }
  )
)

tar_runtime <- runtime_init()
