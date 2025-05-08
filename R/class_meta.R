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
  meta_class$new(database, depends, store = store, lookup = database$lookup)
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
    lookup = NULL,
    repository_cas_lookup_table = NULL,
    local_builders = NULL,
    branches = NULL,
    initialize = function(
      database = NULL,
      depends = NULL,
      store = NULL,
      lookup = NULL
    ) {
      self$database <- database
      self$depends <- depends
      self$store <- store
      self$lookup <- lookup
    },
    get_depend = function(name) {
      .subset2(.subset2(self, "depends"), name)
    },
    get_row = function(name) {
      .subset2(.subset2(self, "database"), "get_row")(name)
    },
    get_record = function(name) {
      record_from_row(
        row = .subset2(self, "get_row")(name),
        path_store = store
      )
    },
    set_record = function(record) {
      self$database$set_row(record_produce_row(record))
    },
    insert_record = function(record) {
      self$database$buffer_row(record_produce_row(record))
    },
    insert_row = function(row) {
      .subset2(.subset2(self, "database"), "buffer_row")(row)
    },
    exists_record = function(name) {
      .subset2(.subset2(self, "database"), "exists_row")(name)
    },
    del_records = function(names) {
      self$database$del_rows(names)
    },
    list_records = function() {
      self$database$list_rows()
    },
    restrict_records = function(pipeline) {
      bar <- cli_local_progress_bar_init(
        label = "restricting metadata records"
      )
      on.exit(cli_local_progress_bar_destroy(bar = bar))
      names_envir <- names(pipeline$imports)
      names_pipeline <- pipeline_get_names(pipeline)
      names_current <- c(names_envir, names_pipeline, self$branches)
      self$del_records(setdiff(self$list_records(), names_current))
    },
    hash_deps = function(deps, pipeline) {
      hash_list <- .subset2(self, "produce_hash_list")(deps, pipeline)
      .subset2(self, "hash_hash_list")(hash_list)
    },
    # produce_hash_list() could be a bottleneck.
    # It currently uses lapply() rather than a simple loop because
    # incrementally growing hash_list element by element
    # makes pattern_produce_data_hash() extremely slow for patterns with
    # 100000+ dynamic branches.
    produce_hash_list = function(deps, pipeline) {
      hash_list <- lapply(
        deps,
        function(name) .subset2(.subset2(lookup, name), "data")
      )
      names(hash_list) <- deps
      hash_list
    },
    hash_hash_list = function(hash_list) {
      hashes <- unlist(hash_list, use.names = TRUE)
      string <- paste(c(names(hashes), hashes), collapse = "")
      hash_object(string)
    },
    produce_depend = function(target, pipeline) {
      .subset2(self, "hash_deps")(.subset2(target, "deps"), pipeline)
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
      if (write) {
        self$database$deduplicate_storage()
        data <- self$database$read_data()
      } else {
        data <- self$database$read_condensed_data()
      }
      self$update_repository_cas_lookup_table(data)
      self$database$set_data(data)
      bar <- cli_local_progress_bar_init(
        label = "noting builders and branches"
      )
      on.exit(cli_local_progress_bar_destroy(bar = bar))
      is_local_builder <- data$type %in% c("stem", "branch") &
        data$repository == "local"
      self$local_builders <- data$name[is_local_builder]
      self$branches <- data$name[data$type == "branch"]
      tar_runtime$meta <- self
    },
    ensure_preprocessed = function(write = FALSE) {
      if (lookup_count(self$database$lookup) < 1L) {
        self$preprocess(write = write)
      }
    },
    update_repository_cas_lookup_table = function(data) {
      rows <- !(data$repository %in% c("local", "aws", "gcp"))
      data <- data[rows, c("data", "repository")]
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
    # suppress a false positive:
    # nocov start
    set_repository_hash_table = function(repository, data) {
      self$repository_key_lookup[[repository]] <- list2env(
        as.list(data),
        parent = tar_envir_empty,
        hash = TRUE
      )
    },
    # nocov end
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
      cli::cli_alert_success(
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
