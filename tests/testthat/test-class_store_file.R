tar_test("dynamic files work", {
  envir <- new.env(parent = environment())
  x <- target_init(
    name = "abc",
    expr = quote(f()),
    format = "file",
    envir = envir
  )
  envir$f <- function() {
    file <- tempfile()
    writeLines("lines", con = file)
    file
  }
  builder_update_build(x)
  expect_equal(x$store$file$path, character(0))
  expect_true(is.na(x$store$file$hash))
  builder_update_paths(x)
  expect_true(file.exists(x$store$file$path))
  expect_true(nzchar(x$store$file$path))
  expect_false(is.na(x$store$file$hash))
  expect_silent(store_validate(x$store))
  expect_true(file.exists(x$store$file$path))
  expect_false(is.na(x$store$file$hash))
  builder_update_object(x)
  expect_false(file.exists(file.path("_targets", "objects", "abc")))
  expect_equal(readLines(x$value$object), "lines")
  out <- target_read_value(x)$object
  exp <- x$store$file$path
  expect_equal(out, exp)
})

tar_test("dynamic files must return characters", {
  x <- target_init(
    name = "abc",
    expr = quote(list(list("illegal"))),
    format = "file"
  )
  pipeline <- pipeline_init(list(x))
  local <- local_init(pipeline = pipeline)
  expect_error(local$run(), class = "condition_validate")
})
