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

tar_test("store_validate()", {
  store_validate(store_new(resources = list()))
})
