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
  list_columns = NULL,
  queue = NULL
) {
  database_class$new(
    memory = memory,
    path = path,
    header = header,
    list_columns = list_columns,
    queue = queue
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
      list_columns = NULL,
      queue = NULL
    ) {
      self$memory <- memory
      self$path <- path
      self$header <- header
      self$list_columns <- list_columns
      self$queue <- queue
    },
    memory = NULL,
    path = NULL,
    header = NULL,
    list_columns = NULL,
    queue = NULL,
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
    get_data = function() {
      rows <- self$list_rows()
      out <- map(rows, ~as_data_frame(self$get_row(.x)))
      do.call(rbind, out)
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
    enqueue_row = function(row) {
      line <- self$produce_line(self$select_cols(row))
      self$queue <- c(self$queue, line)
    },
    dequeue_rows = function() {
      if (length(self$queue)) {
        on.exit(self$queue <- NULL)
        self$append_lines(self$queue)
      }
    },
    write_row = function(row) {
      line <- self$produce_line(self$select_cols(row))
      self$append_lines(line)
    },
    append_lines = function(lines, max_attempts = 500) {
      attempt <- 0L
      # Tested in tests/interactive/test-database.R
      # nocov start
      while (!is.null(try(self$try_append_lines(lines)))) {
        msg <- paste("Reattempting to append lines to", self$path)
        cli_mark_info(msg)
        Sys.sleep(stats::runif(1, 0.2, 0.25))
        attempt <- attempt + 1L
        if (attempt > max_attempts) {
          tar_throw_run(
            "timed out after ",
            max_attempts,
            " attempts trying to append to ",
            self$path
          )
        }
      }
      # nocov end
    },
    try_append_lines = function(lines) {
      write(lines, self$path, ncolumns = 1L, append = TRUE, sep = "")
      invisible()
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
      tmp <- tempfile(pattern = "tar_temp_", tmpdir = dir)
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
      withr::local_options(.new = list(OutDec = "."))
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
      if_any(
        file.exists(self$path),
        self$read_existing_data(),
        self$produce_mock_data()
      )
    },
    read_existing_data = function() {
      database_read_existing_data(self)
    },
    produce_mock_data = function() {
      out <- as_data_frame(map(self$header, ~character(0)))
      colnames(out) <- self$header
      out
    },
    deduplicate_storage = function() {
      if (file.exists(self$path)) {
        data <- self$condense_data(self$read_data())
        data <- data[order(data$name),, drop = FALSE] # nolint
        self$overwrite_storage(data)
      }
    },
    validate_columns = function(header, list_columns) {
      database_validate_columns(header, list_columns)
    },
    validate_file = function() {
      database_validate_file(self)
    },
    validate = function() {
      memory_validate(self$memory)
      self$validate_columns(self$header, self$list_columns)
      self$validate_file()
      tar_assert_chr(self$path)
      tar_assert_scalar(self$path)
      tar_assert_chr(self$header)
      tar_assert_chr(self$list_columns)
    }
  )
)

# TODO: move these functions inline in the class again
# after https://github.com/jimhester/lintr/issues/804 is solved.
database_read_existing_data <- function(database) {
  # TODO: use sep2 once implemented:
  # https://github.com/Rdatatable/data.table/issues/1162
  # We can also delete the list_columns arg then.
  encoding <- getOption("encoding")
  encoding <- if_any(
    identical(tolower(encoding), "latin1"),
    "Latin-1",
    encoding
  )
  encoding <- if_any(
    encoding %in% c("unknown", "UTF-8", "Latin-1"),
    encoding,
    "unknown"
  )
  out <- data.table::fread(
    file = database$path,
    sep = database_sep_outer,
    fill = TRUE,
    na.strings = "",
    encoding = encoding
  )
  out <- as_data_frame(out)
  if (nrow(out) < 1L) {
    return(out)
  }
  for (id in database$list_columns) {
    out[[id]] <- strsplit(
      as.character(out[[id]]),
      split = database_sep_inner,
      fixed = TRUE
    )
  }
  out
}

database_validate_columns <- function(header, list_columns) {
  if (!all(list_columns %in% header)) {
    tar_throw_validate("all list columns must be in the header")
  }
  if (!is.null(header) && !("name" %in% header)) {
    tar_throw_validate("header must have a column called \"name\"")
  }
}

database_validate_file <- function(database) {
  if (!file.exists(database$path)) {
    return()
  }
  line <- readLines(database$path, n = 1L)
  header <- strsplit(line, split = database_sep_outer, fixed = TRUE)[[1]]
  if (identical(header, database$header)) {
    return()
  }
  tar_throw_file(
    "invalid header in ", database$path, "\n",
    "  found:    ", paste(header, collapse = database_sep_outer), "\n",
    "  expected: ", paste(database$header, collapse = database_sep_outer),
    "\nProbably because of a breaking change in the targets package. ",
    "Before running tar_make() again, ",
    "either delete the data store with tar_destroy() ",
    "or downgrade the targets package to an earlier version."
  )
}

database_sep_outer <- "|"
database_sep_inner <- "*"
