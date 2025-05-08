database_init <- function(
  path = tempfile(),
  subkey = basename(tempfile()),
  header = "name",
  logical_columns = character(0L),
  integer_columns = character(0L),
  numeric_columns = character(0L),
  list_columns = character(0L),
  list_column_modes = character(0L),
  repository = tar_options$get_repository_meta(),
  resources = tar_options$get_resources()
) {
  lookup <- lookup_new()
  key <- file.path(
    resources[[repository]]$prefix %|||% path_store_default(),
    subkey
  )
  switch(
    repository,
    local = database_local_new(
      lookup = lookup,
      path = path,
      key = key,
      header = header,
      logical_columns = logical_columns,
      integer_columns = integer_columns,
      numeric_columns = numeric_columns,
      list_columns = list_columns,
      list_column_modes = list_column_modes,
      resources = resources
    ),
    aws = database_aws_new(
      lookup = lookup,
      path = path,
      key = key,
      header = header,
      logical_columns = logical_columns,
      integer_columns = integer_columns,
      numeric_columns = numeric_columns,
      list_columns = list_columns,
      list_column_modes = list_column_modes,
      resources = resources
    ),
    gcp = database_gcp_new(
      lookup = lookup,
      path = path,
      key = key,
      header = header,
      logical_columns = logical_columns,
      integer_columns = integer_columns,
      numeric_columns = numeric_columns,
      list_columns = list_columns,
      list_column_modes = list_column_modes,
      resources = resources
    ),
    tar_throw_validate(
      "unsupported repository \"",
      repository,
      "\" for `targets` database class."
    )
  )
}

database_class <- R6::R6Class(
  classname = "tar_database",
  class = FALSE,
  portable = FALSE,
  cloneable = FALSE,
  public = list(
    lookup = NULL,
    path = NULL,
    key = NULL,
    header = NULL,
    logical_columns = NULL,
    integer_columns = NULL,
    numeric_columns = NULL,
    list_columns = NULL,
    list_column_modes = NULL,
    resources = NULL,
    buffer = NULL,
    buffer_length = NULL,
    staged = NULL,
    connection = NULL,
    initialize = function(
      lookup = NULL,
      path = NULL,
      key = NULL,
      header = NULL,
      logical_columns = NULL,
      integer_columns = NULL,
      numeric_columns = NULL,
      list_columns = NULL,
      list_column_modes = NULL,
      resources = NULL
    ) {
      self$lookup <- lookup
      self$path <- path
      self$key <- key
      self$header <- header
      self$logical_columns <- logical_columns
      self$integer_columns <- integer_columns
      self$numeric_columns <- numeric_columns
      self$list_columns <- list_columns
      self$list_column_modes <- list_column_modes
      self$resources <- resources
      self$buffer <- new.env(parent = emptyenv(), hash = TRUE)
      self$buffer_length <- 0L
    },
    close = function() {
      if (!is.null(self$connection)) {
        try(base::close(self$connection))
        self$connection <- NULL
      }
    },
    get_row = function(name) {
      lookup_get(lookup, name)
    },
    set_row = function(row) {
      lookup[[.subset2(row, "name")]] <- as.list(row)
    },
    del_rows = function(names) {
      lookup_remove(lookup, names)
    },
    get_data = function() {
      rows <- self$list_rows()
      list_columns <- self$list_columns
      list_column_mode_list <- as.list(self$list_column_modes)
      names(list_column_mode_list) <- list_columns
      out <- map(
        rows,
        ~database_repair_list_columns(
          as.list(get_row(.x)),
          list_columns,
          list_column_mode_list
        )
      )
      out <- as_data_frame(data.table::rbindlist(out, fill = TRUE))
      columns <- base::union(self$header, setdiff(colnames(out), self$header))
      out[, columns, drop = FALSE]
    },
    set_data = function(data) {
      bar <- cli_local_progress_bar_init(label = paste("parsing", self$path))
      on.exit(cli_local_progress_bar_destroy(bar = bar))
      if (nrow(data)) {
        transposed <- data.table::transpose(data)
        names <- names(data)
        named <- lapply(
          transposed,
          function(x) as.list(stats::setNames(x, nm = names))
        )
        names(named) <- data$name
        list2env(x = named, envir = self$lookup)
      }
    },
    exists_row = function(name) {
      lookup_exists(.subset2(self, "lookup"), name)
    },
    list_rows = function() {
      lookup_list(.subset2(self, "lookup"))
    },
    condense_data = function(data) {
      repeats <- duplicated(data$name, fromLast = TRUE)
      if (any(repeats)) {
        data[!repeats, ]
      } else {
        data
      }
    },
    read_condensed_data = function() {
      self$condense_data(self$read_data())
    },
    insert_row = function(row) {
      self$set_row(row)
      self$write_row(row)
    },
    append_data = function(data) {
      self$append_storage(data)
      self$set_data(data)
    },
    select_cols = function(data) {
      fill <- setdiff(.subset2(self, "header"), names(data))
      na_col <- rep(NA_character_, length(.subset2(data, "name")))
      for (col in fill) {
        data[[col]] <- na_col
      }
      as.list(data)[header]
    },
    buffer_row = function(row, fill_missing = TRUE) {
      .subset2(self, "set_row")(row)
      if (fill_missing) {
        row <- .subset2(self, "select_cols")(row)
      }
      new_length <- buffer_length + 1L
      self$buffer[[as.character(new_length)]] <- row
      self$buffer_length <- new_length
    },
    flush_rows = function() {
      buffer_length <- .subset2(self, "buffer_length")
      if (buffer_length == 0L) {
        return()
      }
      lines_list <- vector(mode = "list", length = buffer_length)
      index <- 1L
      buffer <- .subset2(self, "buffer")
      while (index <= buffer_length) {
        row <- .subset2(buffer, as.character(index))
        line <- .subset2(self, "produce_sublines")(row)
        lines_list[[index]] <- paste(line, collapse = database_sep_outer)
        index <- index + 1L
      }
      lines <- as.character(lines_list)
      self$append_lines(lines)
      self$buffer <- new.env(parent = emptyenv(), hash = FALSE)
      self$buffer_length <- 0L
      self$staged <- TRUE
    },
    upload_staged = function() {
      if (!is.null(self$staged) && self$staged) {
        self$upload(verbose = FALSE)
        self$staged <- FALSE
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
      while (!is.null(try(self$try_append_lines(lines), silent = in_test()))) {
        compare_working_directories()
        msg <- paste("Reattempting to append lines to", self$path)
        cli::cli_alert(msg)
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
      if (is.null(self$connection)) {
        self$connection <- file(self$path, open = "a+")
      }
      cat(lines, file = self$connection, append = TRUE, sep = "\n")
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
      file_move(from = tmp, to = self$path)
    },
    produce_sublines = function(row) {
      old <- options(OutDec = ".")
      on.exit(options(old))
      index <- 1L
      n <- length(row)
      sublines <- character(length = n)
      while (index <= n) {
        sublines[index] <- produce_subline(.subset2(row, index))
        index <- index + 1L
      }
      sublines
    },
    produce_line = function(row) {
      sublines <- produce_sublines(row)
      paste(sublines, collapse = database_sep_outer)
    },
    produce_subline = function(element) {
      if (anyNA(element)) {
        element <- replace_na(element, "")
      }
      element <- as.character(unlist(element))
      if (length(element) != 1L) {
        element <- paste(element, collapse = database_sep_inner)
      }
      element
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
    read_data = function() {
      if_any(
        file.exists(self$path),
        self$read_existing_data(),
        self$produce_mock_data()
      )
    },
    get_encoding = function() {
      encoding <- getOption("encoding")
      encoding <- if_any(
        identical(tolower(encoding), "latin1"),
        "Latin-1",
        encoding
      )
      if_any(
        encoding %in% c("unknown", "UTF-8", "Latin-1"),
        encoding,
        "unknown"
      )
    },
    read_existing_data = function() {
      bar <- cli_local_progress_bar_init(label = paste("reading", self$path))
      on.exit(cli_local_progress_bar_destroy(bar = bar))
      # TODO: use sep2 once implemented:
      # https://github.com/Rdatatable/data.table/issues/1162
      encoding <- self$get_encoding()
      out <- data.table::fread(
        file = self$path,
        sep = database_sep_outer,
        fill = TRUE,
        na.strings = "",
        encoding = encoding,
        colClasses = "character"
      )
      out <- as_data_frame(out)
      for (name in self$logical_columns) {
        value <- out[[name]]
        if (!is.null(value)) {
          out[[name]] <- as.logical(value)
        }
      }
      for (name in self$integer_columns) {
        value <- out[[name]]
        if (!is.null(value)) {
          out[[name]] <- as.integer(value)
        }
      }
      for (name in self$numeric_columns) {
        value <- out[[name]]
        if (!is.null(value)) {
          out[[name]] <- as.numeric(value)
        }
      }
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
      bar <- cli_local_progress_bar_init(
        label = paste("de-duplicating", self$path)
      )
      on.exit(cli_local_progress_bar_destroy(bar = bar))
      self$ensure_storage()
      lines <- readLines(
        con = self$path,
        encoding = self$get_encoding(),
        warn = FALSE
      )
      tar_assert_match(
        x = lines[1L],
        pattern = paste0("^name", database_sep_outer_grep),
        msg = paste(
          "bug in targets: cannot deduplicate storage for the database at",
          self$path,
          "because the header does not begin with \"name\"."
        )
      )
      names <- gsub(
        pattern = paste0(database_sep_outer_grep, ".*"),
        replacement = "",
        x = lines
      )
      line_duplicated <- c(FALSE, duplicated(names[-1L], fromLast = TRUE))
      if (any(line_duplicated)) {
        dir <- dirname(self$path)
        dir_create(dir)
        tmp <- tempfile(pattern = "tar_temp_", tmpdir = dir)
        writeLines(text = lines[!line_duplicated], con = tmp)
        file_move(from = tmp, to = self$path)
      }
      invisible()
    },
    nice_upload = function(verbose = TRUE, strict = FALSE) {
      if (all(file.exists(self$path))) {
        return(self$upload(verbose = verbose))
      }
      message <- paste("Path does not exist:", self$path)
      if (strict) {
        tar_throw_run(message)
      } else if (verbose) {
        tar_message_run(message)
      }
    },
    nice_download = function(verbose = TRUE, strict = FALSE) {
      if (self$head()$exists) {
        return(self$download(verbose = verbose))
      }
      message <- paste("Path", self$path, "is not in your cloud bucket.")
      if (strict) {
        tar_throw_run(message)
      } else if (verbose) {
        tar_message_run(message)
      }
    },
    upload = function(verbose = TRUE) {
      if (verbose) {
        tar_message_run("uploading")
      }
      "upload"
    },
    upload_workspace = function(target, meta, reporter) {
      "upload_workspace"
    },
    download = function(verbose = TRUE) {
      if (verbose) {
        tar_message_run("downloading")
      }
      "download"
    },
    download_workspace = function(name, store, verbose = TRUE) {
      "download_workspace"
    },
    head = function() {
      file <- file_init(path = "path_cloud")
      file_ensure_hash(file)
      list(
        exists = file.exists("path_cloud"),
        hash = file$hash,
        size = file$size,
        time = file$time
      )
    },
    sync = function(prefer_local = TRUE, verbose = TRUE) {
      head <- self$head()
      file <- file_init(path = self$path)
      file_ensure_hash(file)
      exists_file <- all(file.exists(self$path))
      exists_object <- head$exists %|||% FALSE
      changed <- !all(file$hash == head$hash)
      if (exists_file && (!exists_object)) {
        self$upload(verbose = verbose)
      } else if ((!exists_file) && exists_object) {
        self$download(verbose = verbose)
      } else if (exists_file && exists_object && changed) {
        time_file <- file_time_posixct(file$time)
        time_head <- file_time_posixct(head$time)
        file_newer <- time_file > time_head
        file_same <- file$time == head$time
        do_upload <- file_newer || (prefer_local && file_same)
        if (do_upload) {
          self$upload(verbose = verbose)
        } else {
          self$download(verbose = verbose)
        }
      } else {
        if (verbose) {
          tar_print(
            "Skipped syncing ",
            self$path,
            " with cloud object ",
            self$key
          )
        }
        invisible()
      }
    },
    validate_columns = function(
      header,
      logical_columns,
      integer_columns,
      numeric_columns,
      list_columns
    ) {
      special_columns <- c(
        logical_columns,
        integer_columns,
        numeric_columns,
        list_columns
      )
      if (!all(special_columns %in% header)) {
        tar_throw_validate(
          "all logical/integer/numeric/list columns must be in the header"
        )
      }
      if (!is.null(header) && !("name" %in% header)) {
        tar_throw_validate("header must have a column called \"name\"")
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
      tar_throw_file(
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
      lookup_validate(self$lookup)
      self$validate_file()
      tar_assert_chr(self$path)
      tar_assert_scalar(self$path)
      tar_assert_none_na(self$path)
      tar_assert_nzchar(self$path)
      tar_assert_chr(self$key)
      tar_assert_scalar(self$key)
      tar_assert_none_na(self$key)
      tar_assert_nzchar(self$key)
      tar_assert_chr(self$header)
      tar_assert_chr(self$logical_columns)
      tar_assert_chr(self$integer_columns)
      tar_assert_chr(self$numeric_columns)
      tar_assert_chr(self$list_columns)
      tar_assert_chr(self$list_column_modes)
      self$validate_columns(
        self$header,
        self$logical_columns,
        self$integer_columns,
        self$numeric_columns,
        self$list_columns
      )
    }
  )
)

compare_working_directories <- function() {
  current <- getwd()
  expected <- tar_runtime$working_directory
  if (!is.null(expected) && (current != expected)) {
    tar_throw_run(
      sprintf(
        paste(
          "at least one of your targets changed your working directory",
          "from '%s' to '%s'. A target must not change the working",
          "directory. If you absolutely must change the working directory,",
          "then please wrap the relevant R code in withr::with_dir()",
          "to restore the working directory to its original state."
        ),
        expected,
        current
      )
    )
  }
}

database_repair_list_columns <- function(x, columns, list_column_mode_list) {
  for (column in columns) {
    na <- NA
    mode(na) <- list_column_mode_list[[column]]
    x[[column]] <- list(.subset2(x, column) %|||% na)
  }
  x
}

database_sep_inner <- "*"
database_sep_outer <- "|"
database_sep_outer_grep <- "\\|"
