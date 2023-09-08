passive_new <- function(
  pipeline = NULL,
  meta = NULL,
  names = NULL,
  shortcut = NULL,
  queue = NULL,
  reporter = NULL,
  seconds_meta_append = NULL,
  seconds_meta_upload = NULL,
  seconds_reporter = NULL
) {
  passive_class$new(
    pipeline = pipeline,
    meta = meta,
    names = names,
    shortcut = shortcut,
    queue = queue,
    reporter = reporter,
    seconds_meta_append = seconds_meta_append,
    seconds_meta_upload = seconds_meta_upload,
    seconds_reporter = seconds_reporter
  )
}

passive_class <- R6::R6Class(
  classname = "tar_passive",
  inherit = algorithm_class,
  class = FALSE,
  portable = FALSE,
  cloneable = FALSE,
  public = list(
    ensure_meta = function() {
      self$meta$migrate_database()
      self$meta$validate()
      self$meta$database$ensure_preprocessed(write = FALSE)
      self$meta$set_imports(self$pipeline$imports, self$pipeline)
      self$meta$restrict_records(self$pipeline)
    },
    start = function() {
      pipeline_prune_names(self$pipeline, self$names)
      self$ensure_meta()
      pipeline_reset_priorities(self$pipeline)
      self$update_scheduler()
      self$bootstrap_shortcut_deps()
      self$scheduler$reporter$report_start()
    },
    end = function() {
      self$scheduler$reporter$report_end()
    }
  )
)
