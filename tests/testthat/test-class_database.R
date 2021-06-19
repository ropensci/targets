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
  expect_equal(out$memory$names, character(0))
  row <- list(name = "abc", string = "run(xyz)", children = list(letters))
  out$set_row(row)
  expect_equal(out$memory$names, "abc")
  expect_equal(memory_get_object(out$memory, "abc"), row)
})

tar_test("database$get_data()", {
  db <- database_init()
  expect_equal(db$memory$names, character(0))
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
  expect_equal(sort(db$memory$names), sort(c("e", "f", "x")))
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
  expect_equal(db$memory$names, character(0))
  expect_equal(readLines(db$path), "name|col3")
})

tar_test("database$preprocess()", {
  path <- tempfile()
  lines <- c(
    "name|col2|col3|col4",
    "x|e02|1*2|1",
    "e|e12|e13*e14|2",
    "e|e22|e23*e24*e25|3",
    "f|e32|x|4"
  )
  writeLines(lines, path)
  db <- database_init(
    path = path,
    header = colnames(data),
    list_columns = "col3"
  )
  db$preprocess(write = TRUE)
  out <- readLines(path)
  expect_equal(out, lines[-3])
  expect_equal(sort(db$memory$names), sort(c("e", "f", "x")))
  exp <- list(
    name = "e",
    col2 = "e22",
    col3 = c("e23", "e24", "e25"),
    col4 = 3L
  )
  expect_equal(db$get_row("e"), exp)
  exp <- list(name = "f", col2 = "e32", col3 = "x", col4 = 4L)
  expect_equal(db$get_row("f"), exp)
  exp <- list(name = "x", col2 = "e02", col3 = c("1", "2"), col4 = 1L)
  expect_equal(db$get_row("x"), exp)
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
  expect_equal(db$memory$names, "x")
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
  lines <- c("name|col", "x|1", "x|2", "y|2", "y|1")
  tmp <- tempfile()
  writeLines(lines, tmp)
  db <- database_init(path = tmp, header = c("name", "col"))
  db$deduplicate_storage()
  out <- readLines(tmp)
  expect_equal(out, c("name|col", "x|2", "y|1"))
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
    class = "tar_condition_file"
  )
})

tar_test("database queue", {
  db <- database_init()
  expect_null(db$queue)
  db$enqueue_row(list(name = "x"))
  db$enqueue_row(list(name = "y"))
  expect_equal(db$queue, c("x", "y"))
  expect_false(file.exists(db$path))
  db$dequeue_rows()
  lines <- readLines(db$path)
  expect_equal(lines, c("x", "y"))
  db$dequeue_rows()
  lines <- readLines(db$path)
  expect_equal(lines, c("x", "y"))
})
