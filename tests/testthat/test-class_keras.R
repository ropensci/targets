tar_test("validate keras format", {
  skip_cran()
  skip_if_not_installed("keras")
  x <- target_init(name = "abc", expr = quote(f()), format = "keras")
  expect_silent(target_validate(x))
})

tar_test("keras packages", {
  skip_cran()
  x <- tar_target(x, 1, format = "keras")
  out <- store_get_packages(x$store)
  expect_equal(out, "keras")
})

tar_test("does not inherit from tar_external", {
  skip_cran()
  store <- tar_target(x, "x_value", format = "keras")$store
  expect_false(inherits(store, "tar_external"))
})

tar_test("store_row_path()", {
  skip_cran()
  store <- tar_target(x, "x_value", format = "keras")$store
  store$file$path <- "path"
  expect_equal(store_row_path(store), NA_character_)
})

tar_test("store_path_from_record()", {
  skip_cran()
  store <- tar_target(x, "x_value", format = "keras")$store
  record <- record_init(name = "x", path = "path", format = "keras")
  expect_equal(
    store_path_from_record(store, record, path_store_default()),
    path_objects(path_store_default(), "x")
  )
})
