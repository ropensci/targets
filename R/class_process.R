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
        time_stamp_pid(pid = Sys.getpid()),
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
      if (process_pid_is_exempt(pid)) {
        return()
      }
      time_ps <- time_stamp_pid(pid = pid)
      time_file <- old$value[old$name == "created"]
      if (anyNA(time_file) || anyNA(time_ps)) {
        return()
      }
      if (any(time_file == time_ps)) {
        tar_throw_run(
          paste(
            "Process ID",
            pid,
            "is already running a {targets} pipeline with the",
            dirname(dirname(self$database$path)),
            "folder as the local data store for data and metadata files.",
            "Please do not attempt to run more than one pipeline on the",
            "same data store because it will mangle those important local",
            "files. Before trying again, check that process",
            pid,
            "is really a {targets} pipeline and not a false positive,",
            "then terminate it manually. In case of a false positive,",
            "run",
            sprintf(
              "targets::tar_unblock_process(store = \"%s\")",
              dirname(dirname(self$database$path))
            ),
            "(or manually remove",
            shQuote(self$database$path),
            ") and try again.",
            "False positives may happen if you run",
            "different calls to tar_make() in quick succession",
            "or if you run tar_make(callr_function = NULL) in a",
            "different R process and keep that process running."
          )
        )
      }
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

process_pid_is_exempt <- function(pid) {
  if (identical(pid, as.integer(Sys.getpid()))) {
    return(TRUE)
  }
  pid_parent <- tar_runtime$pid_parent
  !is.null(pid_parent) && identical(pid, pid_parent)
}
