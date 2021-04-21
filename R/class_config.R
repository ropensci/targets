config_init <- function(path = tempfile()) {
  config_new(path = path)
}

config_new <- function(
  path = NULL,
  data = NULL,
  time = NULL,
  lock = NULL
) {
  config_class$new(
    path = path,
    data = data,
    time = time,
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
    time = NULL,
    lock = NULL,
    initialize = function(
      path = NULL,
      data = NULL,
      time = NULL,
      lock = NULL
    ) {
      self$path <- path
      self$data <- data
      self$time <- time
      self$lock <- lock
    },
    set_lock = function() {
      self$lock <- TRUE
    },
    unset_lock = function() {
      self$lock <- FALSE
    },
    is_unlocked = function() {
      !identical(self$lock, TRUE)
    },
    path_exists = function() {
      file.exists(self$path)
    },
    produce_time = function() {
      if_any(self$path_exists(), file.mtime(self$path), NULL)
    },
    read = function() {
      as.list(yaml::read_yaml(self$path))
    },
    load = function() {
      if (self$is_unlocked()) {
        self$data <- self$read()
        self$time <- self$produce_time()
      }
    },
    write = function() {
      if (self$is_unlocked()) {
        yaml::write_yaml(x = self$data, file = self$path)
      }
      invisible()
    },
    unload = function() {
      if (self$is_unlocked()) {
        self$data <- NULL
        self$time <- NULL
      }
    },
    update = function() {
      if_any(self$path_exists(), self$load(), self$unload())
    },
    outdated = function() {
      if_any(
        self$is_unlocked(),
        !identical(self$time, self$produce_time()),
        FALSE
      )
    },
    ensure = function() {
      if (self$outdated()) {
        self$update()
      }
    },
    get_store = function() {
      self$ensure()
      self$data$store
    },
    set_store = function(store) {
      assert_chr(store, "store must be a character.")
      assert_scalar(store, "store must have length 1.")
      self$ensure()
      if (self$is_unlocked()) {
        self$data$store <- store
      }
      self$write()
    },
    validate = function() {
      assert_chr(self$path %|||% "")
      assert_scalar(self$path %|||% "")
      assert_list(self$data %|||% list())
      if (!is.null(self$time)) {
        assert_inherits(self$time, "POSIXct")
      }
    }
  )
)

tar_config <- config_init(path = "_targets.yaml")
