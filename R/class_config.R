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
    export = function() {
      list(
        path = self$path,
        data = self$data,
        lock = self$lock
      )
    },
    import = function(x) {
      self$path <- x$path
      self$data <- x$data
      self$lock <- x$lock
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
    get_script = function() {
      self$get_value(name = "script") %|||% path_script_default()
    },
    get_store = function() {
      self$get_value(name = "store") %|||% path_store_default()
    },
    get_value = function(name) {
      self$ensure()
      self$data[[name]]
    },
    set_script = function(script) {
      self$validate_script(script)
      self$set_value(name = "script", value = as.character(script))
    },
    set_store = function(store) {
      self$validate_store(store)
      self$set_value(name = "store", value = as.character(store))
    },
    set_value = function(name, value) {
      if (self$is_locked()) {
        return()
      }
      self$ensure()
      self$force_memory(name = name, value = value)
      self$write()
    },
    force_memory = function(name, value) {
      assert_scalar(name, "config name field must have length 1.")
      assert_nzchar(name, "config name field must be nonempty.")
      assert_chr(name, "config name field must be a character.")
      self$data <- as.list(self$data)
      self$data[[name]] <- value
    },
    validate_script = function(script) {
      assert_chr(script, "script config must be a character.")
      assert_scalar(script, "script config must have length 1.")
      assert_nzchar(script, "script config must not be empty.")
    },
    validate_store = function(store) {
      assert_chr(store, "store config must be a character.")
      assert_scalar(store, "store config must have length 1.")
      assert_nzchar(store, "store config must not be empty.")
    },
    validate = function() {
      assert_chr(self$path)
      assert_scalar(self$path)
      assert_nzchar(self$path)
      assert_list(self$data %|||% list())
      self$validate_script(self$get_script())
      self$validate_store(self$get_store())
    }
  )
)

tar_config <- config_init(path = "_targets.yaml")
