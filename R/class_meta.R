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
    del_records = function(names) {
      self$database$del_rows(names)
    },
    list_records = function() {
      self$database$list_rows()
    },
    restrict_records = function(pipeline) {
      names_envir <- names(pipeline$imports)
      names_records <- self$list_records()
      names_children <- fltr(
        names_records,
        ~self$database$get_row(.x)$type == "branch"
      )
      names_targets <- pipeline_get_names(pipeline)
      names_parents <- intersect(names_records, names_targets)
      names_current <- c(names_envir, names_targets, names_children)
      remove <- setdiff(names_records, names_current)
      self$del_records(remove)
    },
    hash_dep = function(name, pipeline) {
      exists <- self$exists_record(name) &&
        pipeline_exists_object(pipeline, name)
      trn(
        exists,
        self$get_record(name)$data,
        ""
      )
    },
    hash_deps = function(deps, pipeline) {
      hashes <- map_chr(sort(deps), self$hash_dep, pipeline = pipeline)
      string <- paste(c(names(hashes), hashes), collapse = "")
      digest_chr64(string)
    },
    produce_depend = function(target, pipeline) {
      self$hash_deps(sort(target$command$deps), pipeline)
    },
    handle_error = function(record) {
      if (!self$exists_record(record$name)) {
        return()
      }
      old <- self$get_record(record$name)
      record$path <- old$path
      record$data <- old$data
      record$time <- old$time
      record$bytes <- old$bytes
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

header_meta <- function() {
  c(
    "name",
    "type",
    "data",
    "command",
    "depend",
    "seed",
    "path",
    "time",
    "size",
    "bytes",
    "format",
    "iteration",
    "parent",
    "children",
    "seconds",
    "warnings",
    "error"
  )
}
