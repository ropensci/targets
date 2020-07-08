outdated_init <- function(
  pipeline = NULL,
  meta = meta_init(),
  reporter = "silent"
) {
  scheduler <- pipeline_produce_scheduler(pipeline, reporter = reporter)
  outdated_new(
    pipeline,
    scheduler,
    meta,
    FALSE,
    counter_init(),
    counter_init()
  )
}

outdated_new <- function(
  pipeline = NULL,
  scheduler = NULL,
  meta = NULL,
  garbage_collection = NULL,
  checked = NULL,
  outdated = NULL
) {
  outdated_class$new(
    pipeline = pipeline,
    scheduler = scheduler,
    meta = meta,
    garbage_collection = garbage_collection,
    checked = checked,
    outdated = outdated
  )
}

outdated_class <- R6::R6Class(
  classname = "tar_outdated",
  inherit = algorithm_class,
  class = FALSE,
  portable = FALSE,
  cloneable = FALSE,
  public = list(
    checked = NULL,
    outdated = NULL,
    initialize = function(
      pipeline = NULL,
      scheduler = NULL,
      meta = NULL,
      garbage_collection = NULL,
      checked = NULL,
      outdated = NULL
    ) {
      super$initialize(
        pipeline = pipeline,
        scheduler = scheduler,
        meta = meta,
        garbage_collection = garbage_collection
      )
      self$checked <- checked
      self$outdated <- outdated
    },
    is_outdated = function(name) {
      counter_exists_name(self$outdated, name)
    },
    reset_hash = function(name) {
      record <- self$meta$get_record(name)
      record$data <- NA_character_
      self$meta$set_record(record)
    },
    register_checked = function(name) {
      counter_set_names(self$checked, name)
    },
    register_outdated = function(name) {
      counter_set_names(self$outdated, name)
      if (self$meta$exists_record(name)) {
        self$reset_hash(name)
      }
    },
    process_exists = function(target) {
      tryCatch(
        target_skip(target, self$pipeline, self$scheduler, self$meta),
        error = function(e) warning(e$message)
      )
      target_update_depend(target, self$meta)
      if (target_should_run(target, self$meta)) {
        self$register_outdated(target_get_name(target))
      }
    },
    process_missing = function(target) {
      self$register_outdated(target_get_name(target))
      target_update_queue(target, self$scheduler)
    },
    process_builder = function(target) {
      trn(
        self$meta$exists_record(target_get_name(target)),
        self$process_exists(target),
        self$process_missing(target)
      )
    },
    process_pattern_initial = function(target) {
      trn(
        any(map_lgl(target$settings$dimensions, self$is_outdated)),
        self$process_missing(target),
        target_skip(
          target,
          self$pipeline,
          self$scheduler,
          self$meta
        )
      )
    },
    process_pattern_final = function(target) {
      target_skip(
        target,
        self$pipeline,
        self$scheduler,
        self$meta
      )
      if (any(map_lgl(target_get_children(target), self$is_outdated))) {
        self$register_outdated(target_get_name(target))
      }
    },
    process_pattern = function(target) {
      trn(
        is.null(target$junction),
        self$process_pattern_initial(target),
        self$process_pattern_final(target)
      )
    },
    process_target = function(name) {
      target <- pipeline_get_target(self$pipeline, name)
      trn(
        inherits(target, "tar_pattern"),
        self$process_pattern(target),
        self$process_builder(target)
      )
      self$register_checked(name)
      self$scheduler$reporter$report_outdated(self$checked, self$outdated)
    },
    ensure_meta = function() {
      self$meta$database$ensure_preprocessed(write = FALSE)
      envir <- pipeline_get_envir(self$pipeline)
      self$meta$set_imports(envir, self$pipeline)
    },
    start = function() {
      self$scheduler$reporter$report_start()
      self$ensure_meta()
    },
    end = function() {
      self$scheduler$reporter$report_end()
    },
    run = function() {
      self$start()
      queue <- self$scheduler$queue
      while (queue$should_dequeue()) {
        self$process_target(self$scheduler$queue$dequeue())
      }
      self$end()
    },
    validate = function() {
      super$validate()
      counter_validate(self$outdated)
    }
  )
)
