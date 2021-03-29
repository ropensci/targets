tar_test("value$object", {
  x <- value_init(object = "abc", iteration = "list")
  expect_equal(x$object, "abc")
})

tar_test("misspell list", {
  expect_error(
    value_init(object = "abc", iteration = "lst_dlkfjks"),
    class = "tar_condition_validate"
  )
})

tar_test("value_count_slices(list)", {
  x <- value_init(object = "abc", iteration = "list")
  x$object <- data_frame(x = seq_len(26), y = letters)
  expect_equal(value_count_slices(x), 2L)
})

tar_test("value_produce_slice(list)", {
  x <- value_init(object = "abc", iteration = "list")
  x$object <- data_frame(x = seq_len(26), y = letters)
  expect_equal(value_produce_slice(x, 1L), seq_len(26))
  expect_equal(value_produce_slice(x, 2L), letters)
})

tar_test("value_hash_slice(list)", {
  x <- value_init(object = "abc", iteration = "list")
  x$object <- data_frame(x = seq_len(26), y = letters)
  expect_equal(value_hash_slice(x, 1L), digest_obj32(seq_len(26)))
  expect_equal(value_hash_slice(x, 2L), digest_obj32(letters))
})

tar_test("value_hash_slices(list)", {
  x <- value_init(object = "abc", iteration = "list")
  x$object <- data_frame(x = seq_len(26), y = letters)
  exp <- c(digest_obj32(seq_len(26)), digest_obj32(letters))
  expect_equal(value_hash_slices(x), exp)
})

tar_test("list$validate()", {
  x <- value_init(object = "abc", iteration = "list")
  expect_silent(value_validate(x))
})
