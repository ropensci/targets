tar_test("meta$database", {
  out <- meta_init()
  expect_silent(out$database$validate())
})

tar_test("meta$depends", {
  out <- meta_init()
  expect_silent(memory_validate(out$depends))
})

tar_test("meta$get_record()", {
  meta <- meta_init()
  row <- list(name = "x", type = "cross", path = list(letters))
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
  pipeline <- pipeline_init(list(target))
  local <- local_init(pipeline)
  local$run()
  meta <- local$meta
  db <- meta$database
  db$ensure_storage()
  db$reset_storage()
  data <- db$read_data()
  expect_equal(nrow(data), 0L)
  meta$insert_record(target_produce_record(target, pipeline, meta))
  expect_true(db$exists_row(target_get_name(target)))
  data <- db$read_data()
  expect_equal(nrow(data), 1L)
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
  target <- target_init(name = "a", expr = quote(c(1, 1)), envir = envir)
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

tar_test("metadata storage is not duplicated", {
  envir <- new.env(parent = baseenv())
  envir$file_create <- function(x) {
    file.create(x)
    x
  }
  for (index in seq_len(5)) {
    unlink(file.path("_targets", "objects"), recursive = TRUE)
    envir$b <- letters[index]
    target <- target_init(
      name = "a",
      expr = quote(file_create(b)),
      envir = envir,
      format = "file"
    )
    pipeline <- pipeline_init(list(target))
    local <- local_init(pipeline)
    local$run()
  }
  data <- local$meta$database$read_data()
  expect_equal(nrow(data), 6L)
  expect_equal(unique(table(data$name)), 2L)
  expect_equal(sort(unique(unlist(data$path))), sort(c("d", "e")))
})

tar_test("errored targets get old path and old format in meta", {
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
  expect_equal(data$path[[1]], file.path("_targets", "objects", "abc"))
  expect_equal(data$format, "qs")
})

tar_test("can read old metadata with a error & a non-error", {
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
  data <- meta_init()$database$read_data()
  expect_equal(nrow(data), 2L)
  expect_equal(colnames(data), header_meta())
})

tar_test("meta$produce_depend() empty", {
  envir <- new.env(parent = globalenv())
  x <- target_init(name = "x", expr = quote(1), envir = envir)
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
  x <- target_init(name = "x", expr = quote(w), envir = envir)
  y <- target_init(name = "y", expr = quote(c(w, x)), envir = envir)
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
  expect_false(hash == null64)
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

tar_test("meta$validate()", {
  out <- meta_init()
  expect_silent(out$validate())
})
