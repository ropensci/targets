tar_test("rds update_object()", {
  x <- target_init(name = "abc", expr = quote(a), format = "rds")
  cache_set_object(x$cache, "a", "123")
  builder_update_build(x)
  builder_update_paths(x)
  expect_false(file.exists(x$store$file$path))
  expect_true(is.na(x$store$file$hash))
  builder_update_object(x)
  expect_true(file.exists(x$store$file$path))
  expect_false(is.na(x$store$file$hash))
  path <- file.path("_targets", "objects", "abc")
  expect_equal(readRDS(path), "123")
  expect_equal(target_read_value(x)$object, "123")
})

tar_test("misspelled format", {
  expect_error(tar_target(x, 1, format = "r2ds"), class = "condition_validate")
})

tar_test("rds packages", {
  x <- tar_target(x, 1, format = "rds")
  out <- store_get_packages(x$store)
  expect_equal(out, character(0))
})
