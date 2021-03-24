tar_test("parquet format", {
  skip_if_not_installed("arrow")
  envir <- new.env(parent = baseenv())
  envir$f <- function() {
    data.frame(x = 1, y = 2)
  }
  x <- target_init(
    name = "abc",
    expr = quote(f()),
    format = "parquet"
  )
  builder_update_build(x, envir)
  builder_update_paths(x)
  builder_update_object(x)
  exp <- envir$f()
  expect_equal(arrow::read_parquet(x$store$file$path), exp)
  expect_equal(target_read_value(x)$object, exp)
  expect_silent(target_validate(x))
})

tar_test("bad compression level throws error", {
  skip_if_not_installed("arrow")
  tar_script({
    list(
      tar_target(
        abc,
        data.frame(x = 1, y = 2),
        format = "parquet",
        resources = list(compression = "bad")
      )
    )
  })
  expect_error(tar_make(callr_function = NULL))
})

tar_test("parquet packages", {
  x <- tar_target(x, 1, format = "parquet")
  out <- store_get_packages(x$store)
  expect_equal(out, "arrow")
})
