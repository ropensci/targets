database_init <- function(
  path = tempfile(),
  header = "name",
  list_columns = character(0)
) {
  memory <- memory_init()
  database_new(memory, path, header, list_columns)
}

database_new <- function(
  memory = NULL,
  path = NULL,
  header = NULL,
  list_columns = NULL
) {
  database_class$new(
    memory = memory,
    path = path,
    header = header,
    list_columns = list_columns
  )
}

database_class <- R6::R6Class(
  classname = "tar_database",
  class = FALSE,
  portable = FALSE,
  cloneable = FALSE,
  public = list(
    initialize = function(
      memory = NULL,
      path = NULL,
      header = NULL,
      list_columns = NULL
    ) {
      self$memory <- memory
      self$path <- path
      self$header <- header
      self$list_columns <- list_columns
    },
    memory = NULL,
    path = NULL,
    header = NULL,
    list_columns = NULL,
    get_row = function(name) {
      memory_get_object(self$memory, name)
    },
    set_row = function(row) {
      name <- as.character(row$name)
      memory_set_object(self$memory, name = name, object = as.list(row))
    },
    del_rows = function(names) {
      memory_del_objects(self$memory, names)
    },
    set_data = function(data) {
      list <- lapply(data, as.list)
      map(seq_along(list$name), ~self$set_row(lapply(list, `[[`, i = .x)))
    },
    exists_row = function(name) {
      memory_exists_object(self$memory, name)
    },
    list_rows = function() {
      self$memory$names
    },
    condense_data = function(data) {
      data[!duplicated(data$name, fromLast = TRUE), ]
    },
    read_condensed_data = function() {
      self$condense_data(self$read_data())
    },
    preprocess = function(write = FALSE) {
      data <- self$read_condensed_data()
      self$set_data(data)
      if (write) {
        self$ensure_storage()
        self$overwrite_storage(data)
      }
    },
    insert_row = function(row) {
      self$write_row(row)
      self$set_row(row)
    },
    append_data = function(data) {
      self$append_storage(data)
      self$set_data(data)
    },
    select_cols = function(data) {
      fill <- setdiff(self$header, names(data))
      na_col <- rep(NA_character_, length(data$name))
      for (col in fill) {
        data[[col]] <- na_col
      }
      as.list(data)[self$header]
    },
    write_row = function(row) {
      row <- self$select_cols(row)
      line <- self$produce_line(row)
      write(line, self$path, ncolumns = 1L, append = TRUE, sep = "")
    },
    append_storage = function(data) {
      dir_create(dirname(self$path))
      data.table::fwrite(
        x = data,
        file = self$path,
        sep = database_sep_outer,
        sep2 = c("", database_sep_inner, ""),
        na = "",
        append = TRUE
      )
    },
    overwrite_storage = function(data) {
      dir <- dirname(self$path)
      dir_create(dir)
      tmp <- tempfile(pattern = "temporary_database_", tmpdir = dir)
      data.table::fwrite(
        x = data,
        file = tmp,
        sep = database_sep_outer,
        sep2 = c("", database_sep_inner, ""),
        na = "",
        append = FALSE
      )
      file.rename(from = tmp, to = self$path)
    },
    produce_line = function(row) {
      paste(map_chr(row, self$produce_subline), collapse = database_sep_outer)
    },
    produce_subline = function(element) {
      element <- replace_na(element, "")
      if (is.list(element)) {
        element <- paste(unlist(element), collapse = database_sep_inner)
      }
      as.character(element)
    },
    reset_storage = function() {
      dir_create(dirname(self$path))
      write(self$produce_line(self$header), self$path)
    },
    ensure_storage = function() {
      if (!file.exists(self$path)) {
        self$reset_storage()
      }
    },
    ensure_preprocessed = function(write = FALSE) {
      if (identical(self$memory$count, 0L)) {
        self$preprocess(write = write)
      }
    },
    read_data = function() {
      trn(
        file.exists(self$path),
        self$read_existing_data(),
        self$produce_mock_data()
      )
    },
    read_existing_data = function() {
      # TODO: use sep2 once implemented:
      # https://github.com/Rdatatable/data.table/issues/1162
      # We can also delete the list_columns arg then.
      out <- data.table::fread(
        file = self$path,
        sep = database_sep_outer,
        fill = TRUE,
        na.strings = ""
      )
      out <- as_data_frame(out)
      if (nrow(out) < 1L) {
        return(out)
      }
      for (id in self$list_columns) {
        out[[id]] <- strsplit(
          as.character(out[[id]]),
          split = database_sep_inner,
          fixed = TRUE
        )
      }
      out
    },
    produce_mock_data = function() {
      out <- as_data_frame(map(self$header, ~character(0)))
      colnames(out) <- self$header
      out
    },
    deduplicate_storage = function() {
      if (file.exists(self$path)) {
        self$overwrite_storage(self$condense_data(self$read_data()))
      }
    },
    validate_columns = function(header, list_columns) {
      if (!all(list_columns %in% header)) {
        throw_validate("all list columns must be in the header")
      }
      if (!is.null(header) & !("name" %in% header)) {
        throw_validate("header must have a column called \"name\"")
      }
    },
    validate_file = function() {
      if (!file.exists(self$path)) {
        return()
      }
      line <- readLines(self$path, n = 1L)
      header <- strsplit(line, split = database_sep_outer, fixed = TRUE)[[1]]
      if (identical(header, self$header)) {
        return()
      }
      throw_file(
        "invalid header in ", self$path, "\n",
        "  found:    ", paste(header, collapse = database_sep_outer), "\n",
        "  expected: ", paste(self$header, collapse = database_sep_outer),
        "\nProbably because of a breaking change in the targets package. ",
        "Before running tar_make() again, ",
        "either delete the data store with tar_destroy() ",
        "or downgrade the targets package to an earlier version."
      )
    },
    validate = function() {
      memory_validate(self$memory)
      self$validate_columns(self$header, self$list_columns)
      self$validate_file()
      assert_chr(self$path)
      assert_scalar(self$path)
      assert_chr(self$header)
      assert_chr(self$list_columns)
    }
  )
)

database_sep_outer <- "|"
database_sep_inner <- "*"
