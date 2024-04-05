tar_test("meta$database", {
  out <- meta_init()
  expect_silent(out$database$validate())
})

tar_test("meta$depends", {
  out <- meta_init()
  expect_silent(memory_validate(out$depends))
})

tar_test("meta database key", {
  out <- meta_init()
  expect_equal(
    out$database$key,
    file.path(path_store_default(), "meta", "meta")
  )
})

tar_test("meta$get_record() for internal storage", {
  meta <- meta_init()
  row <- list(
    name = "x",
    type = "cross",
    format = "rds",
    path = list(letters)
  )
  meta$database$set_row(row)
  record <- meta$get_record("x")
  expect_silent(record_validate(record))
  expect_equal(record$name, "x")
  expect_equal(record$type, "cross")
  expect_equal(record$path, path_objects(path_store_default(), "x"))
})

tar_test("meta$get_record() for external storage", {
  meta <- meta_init()
  row <- list(
    name = "x",
    type = "cross",
    path = list(letters),
    format = "file"
  )
  meta$database$set_row(row)
  record <- meta$get_record("x")
  expect_silent(record_validate(record))
  expect_equal(record$name, "x")
  expect_equal(record$type, "cross")
  expect_equal(record$path, letters)
})

tar_test("builder metadata recording", {
  out <- meta_init()
  target <- target_init("x", quote(sample.int(100)))
  pipeline <- pipeline_init(list(target), clone_targets = FALSE)
  local <- local_init(pipeline)
  local$run()
  meta <- local$meta
  db <- meta$database
  db$ensure_storage()
  expect_gt(nrow(db$read_data()), 0L)
  db$reset_storage()
  expect_equal(nrow(db$read_data()), 0L)
  meta$insert_record(target_produce_record(target, pipeline, meta))
  expect_true(db$exists_row(target_get_name(target)))
  expect_equal(nrow(db$read_data()), 0L)
  db$flush_rows()
  expect_equal(nrow(db$read_data()), 1L)
})

tar_test("meta$record_imports()", {
  envir <- new.env(parent = emptyenv())
  envir$f <- function(x) g(x) + h(x)
  envir$a <- "x"
  meta <- meta_init()
  meta$database$ensure_storage()
  meta$record_imports(envir, pipeline_order())
  row <- meta$database$get_row("f")
  expect_equal(row$name, "f")
  expect_equal(row$type, "function")
  expect_equal(nchar(row$data), 16L)
  row <- meta$database$get_row("a")
  expect_equal(row$name, "a")
  expect_equal(row$type, "object")
  expect_equal(nchar(row$data), 16L)
  data <- meta$database$read_data()
  expect_equal(nrow(data), 2)
})

tar_test("metadata recorded in local algo", {
  envir <- new.env(parent = baseenv())
  envir$b <- "x"
  tar_option_set(envir = envir)
  target <- target_init(name = "a", expr = quote(c(1, 1)))
  pipeline <- pipeline_init(list(target))
  local <- local_init(pipeline)
  local$run()
  expect_true(file.exists(file.path("_targets", "meta", "meta")))
  db <- local$meta$database
  row <- db$get_row("b")
  expect_equal(row$name, "b")
  expect_equal(row$type, "object")
  expect_equal(nchar(row$data), 16L)
  row <- db$get_row("a")
  expect_equal(row$name, "a")
  expect_equal(row$type, "stem")
  expect_equal(nchar(row$data), 16L)
  data <- db$read_data()
  expect_equal(colnames(data), header_meta())
  data <- db$read_data()
  expect_equal(colnames(data), header_meta())
  expect_equal(sort(data$name), sort(c("a", "b")))
})

tar_test("metadata storage is duplicated", {
  envir <- new.env(parent = baseenv())
  envir$file_create <- function(x) {
    file.create(x)
    x
  }
  tar_option_set(envir = envir)
  for (index in seq_len(5)) {
    unlink(file.path("_targets", "objects"), recursive = TRUE)
    envir$b <- letters[index]
    target <- target_init(
      name = "a",
      expr = quote(file_create(b)),
      format = "file"
    )
    pipeline <- pipeline_init(list(target))
    local <- local_init(pipeline)
    local$run()
  }
  data <- local$meta$database$read_data()
  expect_equal(nrow(data), 3L)
  expect_equal(unique(table(data$name)), 1L)
  expect_equal(sort(unique(unlist(data$path))), sort(c("e")))
})

tar_test("errored targets keep old path and old format in meta", {
  skip_if_not_installed("qs")
  x <- target_init(name = "abc", expr = quote(123), format = "qs")
  local_init(pipeline_init(list(x)))$run()
  x <- target_init(
    name = "abc",
    expr = quote(stop(123)),
    format = "rds",
    error = "continue"
  )
  local_init(pipeline_init(list(x)))$run()
  meta <- meta_init()
  meta$database$preprocess(write = TRUE)
  data <- meta$database$read_data()
  expect_equal(data$path[[1]], NA_character_)
  expect_equal(
    meta$get_record("abc")$path,
    file.path("_targets", "objects", "abc")
  )
  expect_equal(data$format, "qs")
})

tar_test("errored external targets keep old path and old format in meta", {
  file.create("x")
  x <- target_init(name = "abc", expr = quote("x"), format = "file")
  local_init(pipeline_init(list(x)))$run()
  x <- target_init(
    name = "abc",
    expr = quote(stop(123)),
    format = "rds",
    error = "continue"
  )
  local_init(pipeline_init(list(x)))$run()
  meta <- meta_init()
  meta$database$preprocess(write = TRUE)
  data <- meta$database$read_data()
  expect_equal(data$path[[1]], "x")
  expect_equal(meta$get_record("abc")$path, "x")
  expect_equal(data$format, "file")
})

tar_test("can read metadata with a error & a non-error", {
  skip_if_not_installed("qs")
  targets <- list(
    target_init(name = "abc", expr = quote(123), format = "qs"),
    target_init(
      name = "xyz",
      expr = quote(stop(abc)),
      format = "qs",
      error = "continue"
    )
  )
  local_init(pipeline_init(targets))$run()
  data <- meta_init()$database$read_data()
  expect_equal(nrow(data), 2L)
  expect_equal(colnames(data), header_meta())
})

tar_test("meta$produce_depend() empty", {
  envir <- new.env(parent = globalenv())
  tar_option_set(envir = envir)
  x <- target_init(name = "x", expr = quote(1))
  pipeline <- pipeline_init(list(x))
  local <- local_init(pipeline)
  local$run()
  meta <- local$meta
  out <- meta$produce_depend(x)
  expect_equal(length(out), 1L)
  expect_equal(nchar(out), 16L)
})

tar_test("meta$produce_depend() nonempty", {
  envir <- new.env(parent = baseenv())
  envir$w <- "w"
  tar_option_set(envir = envir)
  x <- target_init(name = "x", expr = quote(w))
  y <- target_init(name = "y", expr = quote(c(w, x)))
  pipeline <- pipeline_init(list(x, y))
  local <- local_init(pipeline)
  local$run()
  meta <- local$meta
  out <- meta$produce_depend(y, pipeline)
  expect_equal(length(out), 1L)
  expect_equal(nchar(out), 16L)
})

tar_test("data hash of pattern updates", {
  pipeline <- pipeline_init(
    list(
      target_init(
        name = "data",
        expr = quote(seq_len(3L))
      ),
      target_init(
        name = "map",
        expr = quote(data),
        pattern = quote(map(data))
      )
    )
  )
  local <- local_init(pipeline)
  local$run()
  data <- meta_init()$database$read_data()
  expect_true("map" %in% data$name)
  hash <- data$data[data$name == "map"]
  expect_equal(length(hash), 1L)
  expect_false(is.na(hash))
  expect_false(hash == hash_null)
  pipeline <- pipeline_init(
    list(
      target_init(
        name = "data",
        expr = quote(seq_len(4L))
      ),
      target_init(
        name = "map",
        expr = quote(data),
        pattern = quote(map(data))
      )
    )
  )
  local <- local_init(pipeline)
  local$run()
  data <- meta_init()$database$read_data()
  hash2 <- data$data[max(which(data$name == "map"))]
  expect_false(hash == hash2)
})

tar_test("data hash of pattern does not write same data", {
  for (index in seq_len(2)) {
    pipeline <- pipeline_init(
      list(
        target_init(
          name = "data",
          expr = quote(seq_len(3L))
        ),
        target_init(
          name = "map",
          expr = quote(data),
          pattern = quote(map(data))
        )
      )
    )
    local <- local_init(pipeline)
    local$run()
  }
  data <- meta_init()$database$read_data()
  expect_equal(sum(data$name == "map"), 1L)
})

tar_test("migrate meta database", {
  tar_script(tar_target(x, "value"))
  tar_make(callr_function = NULL)
  expect_equal(tar_outdated(callr_function = NULL), character(0))
  meta <- meta_init()
  data <- as_data_frame(meta$database$read_condensed_data())
  expect_false(is.null(data$repository))
  data$repository <- NULL
  expect_true(is.null(data$repository))
  meta$database$overwrite_storage(data)
  expect_equal(tar_outdated(callr_function = NULL), character(0))
  expect_equal(tar_read(x), "value")
})

tar_test("meta$validate()", {
  out <- meta_init()
  expect_silent(out$validate())
})
