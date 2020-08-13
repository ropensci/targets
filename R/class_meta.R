meta_init <- function() {
  database <- database_meta()
  depends <- memory_init()
  meta_new(database = database, depends = depends)
}

meta_new <- function(database = NULL, depends = NULL) {
  meta_class$new(database, depends)
}

meta_class <- R6::R6Class(
  classname = "tar_meta",
  class = FALSE,
  portable = FALSE,
  cloneable = FALSE,
  public = list(
    database = NULL,
    depends = NULL,
    initialize = function(database = NULL, depends = NULL) {
      self$database <- database
      self$depends <- depends
    },
    get_depend = function(name) {
      memory_get_object(self$depends, name)
    },
    get_record = function(name) {
      row <- self$database$get_row(name)
      row <- lapply(row, unlist)
      do.call(record_init, row)
    },
    set_record = function(record) {
      self$database$set_row(record_produce_row(record))
    },
    insert_record = function(record) {
      self$database$insert_row(record_produce_row(record))
    },
    exists_record = function(name) {
      self$database$exists_row(name)
    },
    hash_dep = function(name, target) {
      exists <- self$exists_record(name) && (
        record_is_target(self$get_record(name)) ||
          memory_exists_object(target$cache$imports, name)
      )
      trn(
        exists,
        self$get_record(name)$data,
        ""
      )
    },
    hash_deps = function(deps, target) {
      hashes <- map_chr(sort(deps), self$hash_dep, target = target)
      string <- paste(c(names(hashes), hashes), collapse = "")
      digest_chr64(string)
    },
    produce_depend = function(target) {
      self$hash_deps(sort(target$command$deps), target)
    },
    handle_error = function(record) {
      if (!record_has_error(record) || !self$exists_record(record$name)) {
        return()
      }
      old <- self$get_record(record$name)
      record$path <- old$path
      record$data <- old$data
      record$bytes <- old$bytes
      record$time <- old$time
      record$format <- old$format
      record$iteration <- old$iteration
      record$children <- old$children
    },
    data_imports = function(envir, pipeline) {
      data <- hash_imports(envir)
      data[!(data$name %in% pipeline_get_names(pipeline)),, drop = FALSE] # nolint
    },
    record_imports = function(envir, pipeline) {
      data <- self$data_imports(envir, pipeline)
      self$database$append_data(data)
    },
    set_imports = function(envir, pipeline) {
      data <- self$data_imports(envir, pipeline)
      self$database$set_data(data)
    },
    validate = function() {
      self$database$validate()
      memory_validate(self$depends)
    }
  )
)

database_meta <- function() {
  database_init(
    path = path_meta(),
    header = header_meta(),
    list_columns = c("path", "children")
  )
}

path_meta <- function() {
  file.path("_targets", "meta", "meta")
}

header_meta <- function() {
  c(
    "name",
    "type",
    "data",
    "command",
    "depend",
    "seed",
    "path",
    "bytes",
    "time",
    "format",
    "iteration",
    "parent",
    "children",
    "seconds",
    "warnings",
    "error"
  )
}
