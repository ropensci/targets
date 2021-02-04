process_init <- function() {
  process_new(database = database_process())
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
      names <- c("pid", "version_r", "version_targets")
      values <- c(
        as.character(Sys.getpid()),
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
    validate = function() {
      self$database$validate()
    }
  )
)

database_process <- function() {
  database_init(
    path = path_process(),
    header = header_process(),
  )
}

header_process <- function() {
  c("name", "value")
}
