passive_new <- function(
  pipeline = NULL,
  meta = NULL,
  names = NULL,
  shortcut = NULL,
  queue = NULL,
  reporter = NULL,
  seconds_meta_append = NULL,
  seconds_meta_upload = NULL
) {
  passive_class$new(
    pipeline = pipeline,
    meta = meta,
    names = names,
    shortcut = shortcut,
    queue = queue,
    reporter = reporter,
    seconds_meta_append = seconds_meta_append,
    seconds_meta_upload = seconds_meta_upload
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
      self$meta$ensure_preprocessed(write = FALSE)
      self$meta$set_imports(self$pipeline$imports, self$pipeline)
      self$meta$restrict_records(self$pipeline)
    },
    start = function() {
      tar_runtime$active <- TRUE # Needs to be set here for tests.
      pipeline_prune_names(self$pipeline, self$names)
      pipeline_resolve_auto(self$pipeline)
      self$ensure_meta()
      self$set_file_info()
      pipeline_reset_priorities(self$pipeline)
      self$update_scheduler()
      self$bootstrap_shortcut_deps()
      self$scheduler$reporter$report_start()
    },
    end = function() {
      self$meta$database$close()
      self$scheduler$progress$database$close()
      self$scheduler$reporter$report_end()
    }
  )
)
