prelocal_new <- function(
  pipeline = NULL,
  scheduler = NULL,
  meta = NULL,
  garbage_collection = NULL
) {
  prelocal_class$new(
    pipeline = pipeline,
    scheduler = scheduler,
    meta = meta,
    garbage_collection = garbage_collection
  )
}

prelocal_class <- R6::R6Class(
  classname = "tar_prelocal",
  inherit = local_class,
  class = FALSE,
  portable = FALSE,
  cloneable = FALSE,
  public = list(
    start = function() {
    },
    end = function() {
    },
    assert_deployment = function(target) {
      should_abort <- target$settings$deployment == "remote" &&
        inherits(target, "tar_builder")
      if (should_abort) {
        name <- target_get_name(target)
        self$scheduler$queue$enqueue(name, ranks = 0L)
        throw_prelocal("requires remote workers")
      }
    }
  )
)
