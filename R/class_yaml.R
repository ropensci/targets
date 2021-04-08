yaml_init <- function(path = "_targets.yaml") {
  yaml_new(path = path)
}

yaml_new <- function(
  path = NULL,
  data = NULL,
  time = NULL
) {
  yaml_class$new(
    path = path,
    data = data,
    time = time
  )
}

yaml_class <- R6::R6Class(
  classname = "tar_yaml",
  class = FALSE,
  portable = FALSE,
  cloneable = FALSE,
  public = list(
    path = NULL,
    data = NULL,
    time = NULL,
    initialize = function(
      path = NULL,
      data = NULL,
      time = NULL
    ) {
      self$path <- path
      self$data <- data
      self$time <- time
    },
    path_exists = function() {
      file.exists(self$path)
    },
    produce_time = function() {
      if_any(self$path_exists(), file.mtime(self$path), NULL)
    },
    load = function() {
      self$data <- as.list(yaml::read_yaml(self$path))
      self$time <- self$produce_time()
    },
    unload = function() {
      self$data <- NULL
      self$time <- NULL
    },
    update = function() {
      if_any(self$path_exists(), self$load(), self$unload())
    },
    outdated = function() {
      !identical(self$time, self$produce_time())
    },
    ensure = function() {
      if (self$outdated()) {
        self$update()
      }
    },
    store = function() {
      self$ensure()
      self$data$store
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

tar_object_yaml <- yaml_init(path = "_targets.yaml")
