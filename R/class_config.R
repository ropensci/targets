config_init <- function(path = tempfile()) {
  config_new(path = path)
}

config_new <- function(
  path = NULL,
  data = NULL,
  lock = NULL
) {
  config_class$new(
    path = path,
    data = data,
    lock = lock
  )
}

config_class <- R6::R6Class(
  classname = "tar_config",
  class = FALSE,
  portable = FALSE,
  cloneable = FALSE,
  public = list(
    path = NULL,
    data = NULL,
    lock = NULL,
    initialize = function(
      path = NULL,
      data = NULL,
      lock = NULL
    ) {
      self$path <- path
      self$data <- data
      self$lock <- lock
    },
    set_lock = function() {
      self$lock <- TRUE
    },
    unset_lock = function() {
      self$lock <- FALSE
    },
    is_locked = function() {
      identical(as.logical(self$lock), TRUE)
    },
    path_exists = function() {
      file.exists(self$path)
    },
    read = function() {
      as.list(yaml::read_yaml(self$path))
    },
    load = function() {
      self$data <- self$read()
    },
    write = function() {
      yaml::write_yaml(x = self$data, file = self$path)
    },
    unload = function() {
      self$data <- NULL
    },
    update = function() {
      if_any(self$path_exists(), self$load(), self$unload())
    },
    ensure = function() {
      if (self$is_locked()) {
        return()
      }
      self$update()
    },
    get_value = function(name) {
      self$ensure()
      self$data[[name]]
    },
    set_value = function(name, value) {
      assert_scalar(name, "config name field must have length 1.")
      assert_nzchar(name, "config name field must be nonempty.")
      assert_chr(name, "config name field must be a character.")
      if (self$is_locked()) {
        return()
      }
      self$ensure()
      self$data <- as.list(self$data)
      self$data[[name]] <- value
      self$write()
    },
    validate = function() {
      assert_chr(self$path %|||% "")
      assert_scalar(self$path %|||% "")
      assert_list(self$data %|||% list())
    }
  )
)

tar_config <- config_init(path = "_targets.yaml")
