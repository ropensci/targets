tar_test("value_count_slices(vector)", {
  x <- value_init(object = "abc", iteration = "vector")
  x$object <- data_frame(x = seq_len(26), y = letters)
  expect_equal(value_count_slices(x), 26L)
})

tar_test("value_produce_slice(vector)", {
  x <- value_init(object = "abc", iteration = "vector")
  object <- data_frame(x = seq_len(26), y = letters)
  x$object <- object
  for (index in seq_len(nrow(object))) {
    slice <- object[index, ]
    rownames(slice) <- NULL
    expect_equal(value_produce_slice(x, index), slice)
  }
})

tar_test("value_hash_slice(vector)", {
  x <- value_init(object = "abc", iteration = "vector")
  object <- data_frame(x = seq_len(26), y = letters)
  x$object <- object
  for (index in seq_len(nrow(object))) {
    slice <- vctrs::vec_slice(x = object, i = index)
    expect_equal(value_hash_slice(x, index), digest_obj32(slice))
  }
})

tar_test("value_hash_slices(vector)", {
  x <- value_init(object = "abc", iteration = "vector")
  object <- data_frame(x = seq_len(26), y = letters)
  x$object <- object
  out <- value_hash_slices(x)
  expect_equal(length(out), 26)
  for (index in seq_len(nrow(object))) {
    slice <- vctrs::vec_slice(x = object, i = index)
    exp <- digest_obj32(slice)
    expect_equal(out[index], exp)
  }
})

tar_test("vector$validate()", {
  x <- value_init(object = "abc", iteration = "vector")
  expect_silent(value_validate(x))
})
