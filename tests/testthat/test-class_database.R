tar_test("database$get_row()", {
  out <- database_init()
  row <- list(name = "abc", string = "run(xyz)", children = list(letters))
  out$set_row(row)
  expect_equal(out$get_row("abc"), row)
})

tar_test("database$exists_row()", {
  out <- database_init()
  row <- list(name = "abc", string = "run(xyz)", children = list(letters))
  expect_false(out$exists_row("abc"))
  out$set_row(row)
  expect_true(out$exists_row("abc"))
})

tar_test("database$set_row()", {
  out <- database_init()
  expect_equal(lookup_list(out$lookup), character(0))
  row <- list(name = "abc", string = "run(xyz)", children = list(letters))
  out$set_row(row)
  expect_equal(lookup_list(out$lookup), "abc")
  expect_equal(lookup_get(out$lookup, "abc"), row)
})

tar_test("database$get_data()", {
  db <- database_init()
  expect_equal(lookup_list(db$lookup), character(0))
  row1 <- list(name = "abc", string = "123")
  row2 <- list(name = "xyz", string = "456")
  db$set_row(row1)
  db$set_row(row2)
  out <- db$get_data()
  expect_equal(dim(out), c(2L, 2L))
  expect_equal(sort(out$name), sort(c("abc", "xyz")))
  expect_equal(sort(out$string), sort(c("123", "456")))
  expect_false(file.exists(db$path))
})

tar_test("database$get_data() with list columns", {
  skip_cran()
  pipeline <- pipeline_map()
  algorithm <- local_init(pipeline)
  algorithm$run()
  out <- algorithm$meta$database$get_data()
  expect_true(is.list(out$path))
  expect_true(is.list(out$children))
  expect_length(out$children[[which(out$name == "map1")]][[1L]], 3L)
  expect_false(anyNA(out$name))
  expect_true(all(nzchar(out$name)))
  expect_equal(as.integer(anyDuplicated(out$name)), 0L)
})

tar_test("database$ensure_storage()", {
  out <- database_init(header = c("col1", "col2"))
  path <- out$path
  expect_false(file.exists(path))
  out$ensure_storage()
  expect_true(file.exists(path))
  expect_equal(readLines(out$path), "col1|col2")
  write("new|line", path, append = TRUE)
  expect_equal(readLines(out$path), c("col1|col2", "new|line"))
  out$ensure_storage()
  expect_equal(readLines(out$path), c("col1|col2", "new|line"))
  out$reset_storage()
  expect_equal(readLines(out$path), "col1|col2")
})

tar_test("database$produce_line()", {
  out <- database_init()
  row <- list(
    name = "abc",
    string = "run(xyz)",
    children = list(c("a", "b"))
  )
  expect_equal(out$produce_line(row), "abc|run(xyz)|a*b")
})

tar_test("database$read_data() without rows or header", {
  db <- database_init(path = tempfile(), header = character(0))
  out <- db$read_data()
  expect_false(file.exists(db$path))
  expect_equal(out, data_frame())
})

tar_test("database$read_data() without rows", {
  db <- database_init(path = tempfile(), header = letters)
  out <- db$read_data()
  expect_false(file.exists(db$path))
  expect_equal(nrow(out), 0L)
  expect_equal(colnames(out), letters)
})

tar_test("database$read_data()", {
  path <- tempfile()
  header <- c("name", "col2", "col3")
  lines <- c(
    "name|col2|col3",
    "e11|e12|e13*e14",
    "e21|e22|e23*e24*e25"
  )
  writeLines(lines, path)
  db <- database_init(path, header, list_columns = "col3")
  out <- db$read_data()
  exp <- data_frame(
    name = c("e11", "e21"),
    col2 = c("e12", "e22")
  )
  exp$col3 <- list(c("e13", "e14"), c("e23", "e24", "e25"))
  expect_equal(out, exp)
})

tar_test("database$overwrite_storage()", {
  data <- data.table::data.table(
    name = c("e", "e"),
    col2 = c("e12", "e22"),
    col3 = list(c("e13", "e14"), c("e23", "e24", "e25"))
  )
  db <- database_init(header = c("name", "col2", "col3"))
  exp <- c(
    "name|col2|col3",
    "e|e12|e13*e14",
    "e|e22|e23*e24*e25"
  )
  for (index in seq_len(2L)) {
    db$overwrite_storage(data)
    out <- readLines(db$path)
    expect_equal(out, exp)
  }
})

tar_test("database$append_storage()", {
  data <- data.table::data.table(
    name = c("e", "e"),
    col2 = c("e12", "e22"),
    col3 = list(c("e13", "e14"), c("e23", "e24", "e25"))
  )
  db <- database_init(header = c("name", "col2", "col3"))
  exp <- c(
    "name|col2|col3",
    "e|e12|e13*e14",
    "e|e22|e23*e24*e25",
    "e|e12|e13*e14",
    "e|e22|e23*e24*e25"
  )
  for (index in seq_len(2L)) {
    db$append_storage(data)
    out <- readLines(db$path)
  }
  expect_equal(out, exp)
})

tar_test("database$read_condensed_data()", {
  lines <- c(
    "name|col2|col3",
    "e1|e12|e13*e14",
    "e2|e22|e23*e24*e25",
    "e1|e12|e13*e14",
    "e2|e22|e23*e24*e25"
  )
  tmp <- tempfile()
  writeLines(lines, tmp)
  db <- database_init(
    path = tmp,
    header = c("name", "col2", "col3"),
    list_columns = "col3"
  )
  out <- db$read_condensed_data()
  exp <- data_frame(
    name = c("e1", "e2"),
    col2 = c("e12", "e22")
  )
  exp$col3 <- list(c("e13", "e14"), c("e23", "e24", "e25"))
  rownames(out) <- NULL
  rownames(exp) <- NULL
  expect_equal(out, exp)
})

tar_test("database$condense_data()", {
  data <- data.table::data.table(
    name = c("x", "e", "e", "f"),
    col2 = c("e02", "e12", "e22", "e32"),
    col3 = list(c("1", "2"), c("e13", "e14"), c("e23", "e24", "e25"), "x")
  )
  db <- database_init()
  out <- db$condense_data(data)
  exp <- data.table::data.table(
    name = c("x", "e", "f"),
    col2 = c("e02", "e22", "e32"),
    col3 = list(c("1", "2"), c("e23", "e24", "e25"), "x")
  )
  expect_equal(out, exp)
})

tar_test("database$read_condensed_data()", {
  data <- data.table::data.table(
    name = c("x", "e", "e", "f"),
    col2 = c("e02", "e12", "e22", "e32"),
    col3 = list(c("1", "2"), c("e13", "e14"), c("e23", "e24", "e25"), "x")
  )
  db <- database_init()
  out <- db$condense_data(data)
  exp <- data.table::data.table(
    name = c("x", "e", "f"),
    col2 = c("e02", "e22", "e32"),
    col3 = list(c("1", "2"), c("e23", "e24", "e25"), "x")
  )
  expect_equal(out, exp)
})

tar_test("database$set_data()", {
  data <- data.table::data.table(
    name = c("x", "e", "e", "f"),
    col2 = c("e02", "e12", "e22", "e32"),
    col3 = list(c("1L", "2L"), c("e13", "e14"), c("e23", "e24", "e25"), "x"),
    col4 = seq_len(4L)
  )
  db <- database_init()
  db$set_data(data)
  expect_equal(sort(lookup_list(db$lookup)), sort(c("e", "f", "x")))
  exp <- list(
    name = "e",
    col2 = "e22",
    col3 = c("e23", "e24", "e25"),
    col4 = 3L
  )
  expect_equal(db$get_row("e"), exp)
  exp <- list(name = "f", col2 = "e32", col3 = "x", col4 = 4L)
  expect_equal(db$get_row("f"), exp)
  exp <- list(name = "x", col2 = "e02", col3 = c("1L", "2L"), col4 = 1L)
  expect_equal(db$get_row("x"), exp)
})

tar_test("database$preprocess() on empty data", {
  db <- database_init(
    header = c("name", "col3"),
    list_columns = "col3"
  )
  db$preprocess(write = TRUE)
  expect_equal(lookup_list(db$lookup), character(0))
  expect_equal(readLines(db$path), "name|col3")
})

tar_test("database$preprocess() on different column types", {
  path <- tempfile()
  on.exit(unlink(path))
  lines <- c(
    "name|col2|col3|col4|col5|col6",
    "x|e02|1*2|1|1|TRUE",
    "e|e12|e13*e14|2|2|FALSE",
    "e|e22|e23*e24*e25|3|3|TRUE",
    "f|e32|x|4|4|FALSE"
  )
  for (repository in c("local", "aws", "gcp")) {
    writeLines(lines, path)
    db <- database_init(
      path = path,
      header = colnames(data),
      list_columns = "col3",
      logical_columns = "col6",
      integer_columns = "col4",
      numeric_columns = "col5",
      repository = repository
    )
    db$preprocess(write = TRUE)
    out <- readLines(path)
    expect_equal(out, lines[-3])
    expect_equal(sort(lookup_list(db$lookup)), sort(c("e", "f", "x")))
    exp <- list(
      name = "e",
      col2 = "e22",
      col3 = c("e23", "e24", "e25"),
      col4 = 3L,
      col5 = 3,
      col6 = TRUE
    )
    expect_equal(db$get_row("e"), exp)
    exp <- list(
      name = "f",
      col2 = "e32",
      col3 = "x",
      col4 = 4L,
      col5 = 4,
      col6 = FALSE
    )
    expect_equal(db$get_row("f"), exp)
    exp <- list(
      name = "x",
      col2 = "e02",
      col3 = c("1", "2"),
      col4 = 1L,
      col5 = 1,
      col6 = TRUE
    )
    expect_equal(db$get_row("x"), exp)
  }
})

tar_test("database$write_row()", {
  db <- database_init(
    header = c("name", "col2", "col3"),
    list_columns = "col3"
  )
  row <- list(name = "x", col2 = 1L, col3 = list(c("a", "b")))
  db$ensure_storage()
  db$write_row(row)
  out <- readLines(db$path)
  exp <- c("name|col2|col3", "x|1|a*b")
  expect_equal(out, exp)
})

tar_test("database$insert_row()", {
  db <- database_init(
    header = c("name", "col2", "col3"),
    list_columns = "col3"
  )
  row <- list(name = "x", col2 = 1L, col3 = list(c("a", "b")))
  db$ensure_storage()
  db$insert_row(row)
  db$insert_row(row)
  out <- readLines(db$path)
  exp <- c("name|col2|col3", "x|1|a*b", "x|1|a*b")
  expect_equal(out, exp)
  expect_equal(lookup_list(db$lookup), "x")
  expect_equal(db$get_row("x"), row)
})

tar_test("database$select_cols()", {
  db <- database_init(header = c("name", "col2", "col3"))
  data <- data_frame(col2 = "b", name = "a")
  out <- db$select_cols(data)
  exp <- list(name = "a", col2 = "b", col3 = NA_character_)
  expect_equal(out, exp)
})

tar_test("database$deduplicate_storage()", {
  lines <- c("name|col", "x|1", "x|2", "y|2", "y|1", "name|y", "name|z")
  tmp <- tempfile()
  writeLines(lines, tmp)
  db <- database_init(path = tmp, header = c("name", "col"))
  db$deduplicate_storage()
  out <- readLines(tmp)
  expect_equal(out, c("name|col", "x|2", "y|1", "name|z"))
})

tar_test("database$deduplicate_storage() on non-existent file", {
  db <- database_init(path = tempfile(), header = c("name", "col"))
  expect_silent(db$deduplicate_storage())
})

tar_test("database$validate()", {
  out <- database_init(header = "name")
  expect_silent(out$validate())
})

tar_test("database$validate() with bad list columns", {
  out <- database_init(header = c("name", "a"), list_columns = "b")
  expect_error(out$validate(), class = "tar_condition_validate")
})

tar_test("database$validate() without name column", {
  out <- database_init(header = letters)
  expect_error(out$validate(), class = "tar_condition_validate")
})


tar_test("validate compatible header", {
  tar_script(list())
  tar_make(callr_function = NULL)
  data <- tar_meta()
  meta <- meta_init()
  meta$database$overwrite_storage(data)
  expect_silent(tar_make(callr_function = NULL, reporter = "silent"))
})

tar_test("do not validate header of missing file", {
  out <- database_init(header = "name")
  expect_silent(out$validate())
  expect_false(file.exists(out$path))
})

tar_test("fail to validate incompatible header", {
  tar_script(list())
  tar_make(callr_function = NULL)
  data <- tar_meta()
  data$size <- NULL
  meta <- meta_init()
  meta$database$overwrite_storage(data)
  expect_error(
    tar_make(callr_function = NULL),
    class = "tar_condition_run"
  )
})

tar_test("database buffer", {
  db <- database_init()
  expect_true(is.environment(db$buffer))
  expect_equal(lookup_list(db$lookup), character(0L))
  db$buffer_row(list(name = "x"))
  expect_equal(lookup_list(db$lookup), "x")
  db$buffer_row(list(name = "y"))
  expect_equal(sort(as.character(as.list(db$buffer))), sort(c("x", "y")))
  expect_equal(sort(lookup_list(db$lookup)), sort(c("x", "y")))
  expect_false(file.exists(db$path))
  db$flush_rows()
  lines <- readLines(db$path)
  expect_equal(sort(lines), sort(c("x", "y")))
  db$flush_rows()
  lines <- readLines(db$path)
  expect_equal(sort(lines), sort(c("x", "y")))
})

tar_test("compare_working_directories()", {
  on.exit(tar_runtime$working_directory <- NULL)
  tar_runtime$working_directory <- getwd()
  expect_silent(compare_working_directories())
  tar_runtime$working_directory <- ".."
  expect_error(
    compare_working_directories(),
    class = "tar_condition_run"
  )
})

tar_test("local database cloud methods", {
  database <- database_init(repository = "local")
  expect_null(database$download())
  expect_null(database$upload())
  expect_false(database$head()$exists)
  expect_null(database$delete_cloud())
})

tar_test("database unknown repository", {
  expect_error(
    database_init(repository = "unknown"),
    class = "tar_condition_validate"
  )
})

tar_test("mock download",  {
  x <- database_class$new(path = tempfile())
  expect_equal(x$download(), "download")
})

tar_test("mock upload",  {
  x <- database_class$new(path = tempfile())
  expect_equal(x$upload(), "upload")
})

tar_test("mock head non-existent file",  {
  x <- database_class$new(path = tempfile())
  out <- x$head()
  expect_false(out$exists)
  expect_equal(out$time, file_time(info = list(mtime_numeric = 0)))
})

tar_test("mock head",  {
  x <- database_class$new(path = tempfile())
  file.create("path_cloud")
  out <- x$head()
  expect_true(out$exists)
})

tar_test("mock sync no action", {
  x <- database_class$new(path = tempfile(), key = "x")
  expect_null(x$sync(verbose = TRUE))
})

tar_test("mock sync only cloud", {
  x <- database_class$new(path = tempfile(), key = "x")
  file.create("path_cloud")
  expect_equal(x$sync(verbose = TRUE), "download")
})

tar_test("mock sync only local", {
  x <- database_class$new(path = tempfile(), key = "x")
  file.create(x$path)
  expect_equal(x$sync(verbose = TRUE), "upload")
})

tar_test("mock sync only local", {
  x <- database_class$new(path = tempfile(), key = "x")
  file.create(x$path)
  expect_equal(x$sync(verbose = TRUE), "upload")
})

tar_test("mock sync no action on agreement", {
  x <- database_class$new(path = tempfile())
  writeLines("lines", x$path)
  file.copy(x$path, "path_cloud")
  expect_null(x$sync(verbose = TRUE))
})

tar_test("mock sync cloud file more recent", {
  old <- system.file("CITATION", package = "targets", mustWork = TRUE)
  x <- database_class$new(path = old, key = "x")
  writeLines("lines", "path_cloud")
  expect_equal(x$sync(verbose = TRUE), "download")
})

tar_test("mock sync local file more recent", {
  skip_cran()
  x <- database_class$new(path = tempfile(), key = "x")
  writeLines("lines", x$path)
  old <- system.file("CITATION", package = "targets", mustWork = TRUE)
  file.copy(
    from = old,
    to = "path_cloud",
    copy.date = TRUE,
    overwrite = TRUE
  )
  expect_equal(x$sync(verbose = TRUE), "upload")
})

tar_test("mock failed nice_upload()", {
  x <- database_class$new(path = tempfile(), key = "x")
  expect_silent(tmp <- x$nice_upload(verbose = FALSE, strict = FALSE))
  expect_null(tmp)
  expect_message(
    tmp <- x$nice_upload(verbose = TRUE, strict = FALSE),
    class = "tar_condition_run"
  )
  expect_null(tmp)
  expect_error(
    tmp <- x$nice_upload(verbose = TRUE, strict = TRUE),
    class = "tar_condition_run"
  )
  expect_null(tmp)
  expect_error(
    tmp <- x$nice_upload(verbose = FALSE, strict = TRUE),
    class = "tar_condition_run"
  )
  expect_null(tmp)
})

tar_test("mock succeeded nice_upload()", {
  x <- database_class$new(path = tempfile(), key = "x")
  file.create(x$path)
  for (strict in c(TRUE, FALSE)) {
    expect_silent(tmp <- x$nice_upload(verbose = FALSE, strict = strict))
    expect_equal(tmp, "upload")
    expect_message(
      tmp <- x$nice_upload(verbose = TRUE, strict = strict),
      class = "tar_condition_run"
    )
    expect_equal(tmp, "upload")
  }
})

tar_test("mock failed nice_download()", {
  x <- database_class$new(path = tempfile(), key = "x")
  expect_silent(tmp <- x$nice_download(verbose = FALSE, strict = FALSE))
  expect_null(tmp)
  expect_message(
    tmp <- x$nice_download(verbose = TRUE, strict = FALSE),
    class = "tar_condition_run"
  )
  expect_null(tmp)
  expect_error(
    tmp <- x$nice_download(verbose = TRUE, strict = TRUE),
    class = "tar_condition_run"
  )
  expect_null(tmp)
  expect_error(
    tmp <- x$nice_download(verbose = FALSE, strict = TRUE),
    class = "tar_condition_run"
  )
  expect_null(tmp)
})

tar_test("mock succeeded nice_download()", {
  x <- database_class$new(path = tempfile(), key = "x")
  file.create("path_cloud")
  for (strict in c(TRUE, FALSE)) {
    expect_silent(tmp <- x$nice_download(verbose = FALSE, strict = strict))
    expect_equal(tmp, "download")
    expect_message(
      tmp <- x$nice_download(verbose = TRUE, strict = strict),
      class = "tar_condition_run"
    )
    expect_equal(tmp, "download")
  }
})
