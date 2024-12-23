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
  target <- tar_target(x, "x_value", format = "keras")
  store <- target$store
  file <- target$file
  file$path <- "path"
  expect_equal(store_row_path(store, file), NA_character_)
})

tar_test("store_path_from_name()", {
  skip_cran()
  store <- tar_target(x, "x_value", format = "keras")$store
  out <- store_path_from_name(
    store,
    format = store$format,
    name = "x",
    path = "path",
    path_store = path_store_default()
  )
  expect_equal(
    out,
    path_objects(path_store_default(), "x")
  )
})
