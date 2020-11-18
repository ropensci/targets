tar_test("store_wait_correct_hash()", {
  tmp <- tempfile()
  file <- file_init(path = tmp)
  writeLines("lines", tmp)
  store <- store_init()
  store$file <- file
  expect_error(
    store_wait_correct_hash(store, timeout = 0.02),
    class = "condition_targets"
  )
  file_update_hash(file)
  expect_silent(store_wait_correct_hash(store))
})

tar_test("default serialization/unserialization methods", {
  store <- store_init()
  expect_equal(store_serialize_object(store, "x"), "x")
  expect_equal(store_unserialize_object(store, "x"), "x")
})

tar_test("store_validate()", {
  expect_silent(store_validate(store_new(resources = list())))
})

tar_test("store_file packages", {
  x <- tar_target(x, "x", format = "file")
  out <- store_get_packages(x$store)
  expect_equal(out, character(0))
})
