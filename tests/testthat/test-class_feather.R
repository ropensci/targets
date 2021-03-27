tar_test("feather format", {
  skip_on_cran()
  skip_if_not_installed("arrow")
  envir <- new.env(parent = baseenv())
  envir$f <- function() {
    data.frame(x = 1, y = 2)
  }
  x <- target_init(
    name = "abc",
    expr = quote(f()),
    format = "feather"
  )
  builder_update_build(x, envir = envir)
  builder_update_paths(x)
  builder_update_object(x)
  exp <- envir$f()
  expect_equal(arrow::read_feather(x$store$file$path), exp)
  expect_equal(target_read_value(x)$object, exp)
  expect_silent(target_validate(x))
})

tar_test("bad compression level throws error", {
  skip_on_cran()
  skip_if_not_installed("arrow")
  tar_script({
    list(
      tar_target(
        abc,
        data.frame(x = 1, y = 2),
        format = "feather",
        resources = list(compression = "bad")
      )
    )
  })
  expect_error(tar_make(callr_function = NULL))
})

tar_test("feather packages", {
  x <- tar_target(x, 1, format = "feather")
  out <- store_get_packages(x$store)
  expect_equal(out, "arrow")
})
