meta_init <- function(path_store = path_store_default()) {
  database <- database_meta(path_store = path_store)
  depends <- lookup_new()
  meta_new(
    database = database,
    depends = depends,
    store = path_store
  )
}

meta_new <- function(database = NULL, depends = NULL, store = NULL) {
  meta_class$new(database, depends, store = store)
}

meta_class <- R6::R6Class(
  classname = "tar_meta",
  class = FALSE,
  portable = FALSE,
  cloneable = FALSE,
  public = list(
    database = NULL,
    depends = NULL,
    store = NULL,
    repository_cas_lookup_table = NULL,
    initialize = function(
      database = NULL,
      depends = NULL,
      store = NULL
    ) {
      self$database <- database
      self$depends <- depends
      self$store <- store
    },
    get_depend = function(name) {
      lookup_get(.subset2(self, "depends"), name)
    },
    get_record = function(name) {
      record_from_row(
        row = self$database$get_row(name),
        path_store = self$store
      )
    },
    set_record = function(record) {
      self$database$set_row(record_produce_row(record))
    },
    insert_record = function(record) {
      self$database$buffer_row(record_produce_row(record))
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
      if_any(
        exists,
        self$get_record(name)$data,
        ""
      )
    },
    hash_deps = function(deps, pipeline) {
      hashes <- vapply(
        X = sort_chr(deps),
        FUN = self$hash_dep,
        pipeline = pipeline,
        FUN.VALUE = character(1L),
        USE.NAMES = TRUE
      )
      hashes <- hashes[nzchar(hashes)]
      string <- paste(c(names(hashes), hashes), collapse = "")
      hash_object(string)
    },
    produce_depend = function(target, pipeline) {
      self$hash_deps(.subset2(target, "deps"), pipeline)
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
      record$repository <- old$repository
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
    preprocess = function(write = FALSE) {
      data <- self$database$read_condensed_data()
      self$update_repository_cas_lookup_table(data)
      self$database$preprocess(data = data, write = write)
      tar_runtime$meta <- self
    },
    ensure_preprocessed = function(write = FALSE) {
      if (lookup_count(self$database$lookup) < 1L) {
        self$preprocess(write = write)
      }
    },
    update_repository_cas_lookup_table = function(data) {
      data <- data[!is.na(data$repository), c("data", "repository")]
      hashes <- split(x = data$data, f = data$repository)
      lookup_table <- lookup_new()
      for (name in names(hashes)) {
        lookup_set(
          lookup = lookup_table,
          names = name,
          object = .subset2(hashes, name)
        )
      }
      self$repository_cas_lookup_table <- lookup_table
    },
    set_repository_hash_table = function(repository, data) {
      self$repository_key_lookup[[repository]] <- list2env(
        as.list(data),
        parent = tar_envir_empty,
        hash = TRUE
      )
    },
    migrate_database = function() {
      # Add the repository column (> 0.10.0).
      if (!file.exists(self$database$path)) {
        return()
      }
      line <- readLines(self$database$path, n = 1)
      line <- strsplit(line, split = database_sep_outer, fixed = TRUE)[[1]]
      if ("repository" %in% line) {
        return()
      }
      data <- as_data_frame(self$database$read_condensed_data())
      data$repository <- ifelse(
        grepl("^aws_", data$format),
        "aws",
        "local"
      )
      data$repository[data$type %in% c("object", "function")] <- NA_character_
      data$format <- gsub("^aws_", "", data$format)
      data <- data[, self$database$header, drop = FALSE]
      self$database$overwrite_storage(data)
      cli_mark_info(
        paste(
          "Migrated the metadata file to a new data format to include",
          "the new repository column. Up-to-date targets are still up to",
          "date, but version 0.10.0 and below of the targets package",
          "is no longer compatible with this project. If you need to revert",
          "the version of the targets package, then run tar_destroy()",
          "to remove the data store and rerun the pipeline from scratch."
        )
      )
    },
    validate = function() {
      self$database$validate()
      lookup_validate(self$depends)
    }
  )
)

database_meta <- function(path_store) {
  database_init(
    path = path_meta(path_store = path_store),
    subkey = file.path(basename(path_meta("")), "meta"),
    header = header_meta(),
    integer_columns = "seed",
    numeric_columns = c("bytes", "seconds"),
    list_columns = c("path", "children"),
    list_column_modes = c("character", "character")
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
    "repository",
    "iteration",
    "parent",
    "children",
    "seconds",
    "warnings",
    "error"
  )
}
