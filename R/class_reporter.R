reporter_init <- function(reporter = "verbose") {
  switch(
    reporter,
    silent = silent_new(),
    verbose = verbose_new(),
    timestamp = timestamp_new(),
    summary = summary_new(),
    forecast = forecast_new(),
    throw_validate("unsupported reporter")
  )
}

reporter_new <- function() {
  reporter_class$new()
}

reporter_class <- R6::R6Class(
  classname = "tar_reporter",
  class = FALSE,
  portable = FALSE,
  cloneable = FALSE,
  public = list(
    report_start = function() {
    },
    report_end = function(progress = NULL) {
    },
    validate = function() {
    }
  )
)
