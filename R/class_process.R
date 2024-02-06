process_init <- function(path_store = path_store_default()) {
  process_new(database = database_process(path_store = path_store))
}

process_new <- function(database = NULL) {
  process_class$new(database = database)
}

process_class <- R6::R6Class(
  classname = "tar_process",
  class = FALSE,
  portable = FALSE,
  cloneable = FALSE,
  public = list(
    database = NULL,
    initialize = function(database = NULL) {
      self$database <- database
    },
    get_process = function() {
      self$database$get_data()
    },
    read_process = function() {
      self$database$read_data()
    },
    produce_process = function() {
      names <- c("pid", "created", "version_r", "version_targets")
      values <- c(
        as.character(Sys.getpid()),
        time_stamp(ps::ps_create_time()),
        as.character(getRversion()),
        as.character(utils::packageVersion("targets"))
      )
      out <- data_frame(name = names, value = values)
      rownames(out) <- NULL
      out
    },
    set_process = function(process) {
      self$database$set_data(process)
    },
    update_process = function() {
      self$set_process(self$produce_process())
    },
    write_process = function(process) {
      self$database$overwrite_storage(process)
    },
    record_process = function() {
      self$update_process()
      self$write_process(self$get_process())
    },
    assert_unique = function() {
      # Tested in tests/interactive/test-process.R
      # nocov start
      if (!any(file.exists(self$database$path))) {
        return()
      }
      old <- self$read_process()
      if (!all(c("pid", "created") %in% old$name)) {
        # For compatibility with older {targets}, cannot test.
        return() # nocov
      }
      pid <- as.integer(old$value[old$name == "pid"])
      if (identical(pid, as.integer(Sys.getpid()))) {
        return()
      }
      handle <- tryCatch(
        ps::ps_handle(pid = pid),
        error = function(condition) NULL
      )
      if (is.null(handle)) {
        return()
      }
      time_file <- posixct_time(old$value[old$name == "created"])
      time_ps <- ps::ps_create_time(p = handle)
      if (anyNA(time_file) || anyNA(time_ps)) {
        return()
      }
      diff <- abs(difftime(time_file, time_ps, units = "secs"))
      tolerance <- as.difftime(1.01, units = "secs")
      tar_assert_ge(
        x = diff,
        threshold = tolerance,
        msg = paste(
          "Process ID",
          pid,
          "is already running a {targets} pipeline with the",
          dirname(dirname(self$database$path)),
          "folder as the local data store for data and metadata files.",
          "Please do not attempt to run more than one pipeline on the same",
          "data store because it will mangle thos important local files.",
          "Before trying again, check that process",
          pid,
          "is really a {targets} pipeline and not a false positive,",
          "then terminate it manually. In case of a false positive,",
          "remove file",
          self$database$path,
          "and try again."
        )
      )
      # nocov end
    },
    validate = function() {
      self$database$validate()
    }
  )
)

database_process <- function(path_store) {
  database_init(
    path = path_process(path_store = path_store),
    subkey = file.path(basename(path_meta("")), "process"),
    header = header_process()
  )
}

header_process <- function() {
  c("name", "value")
}
