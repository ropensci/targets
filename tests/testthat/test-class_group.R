tar_test("value$object", {
  x <- value_init(object = "abc", iteration = "group")
  expect_equal(x$object, "abc")
})

tar_test("value_count_slices(group)", {
  object <- data_frame(
    x = letters[seq_len(6)],
    tar_group = rep(seq_len(3), each = 2)
  )
  x <- value_init(object = object, iteration = "group")
  expect_equal(value_count_slices(x), 3)
})

tar_test("value_produce_slice(group)", {
  object <- data_frame(
    x = letters[seq_len(6)],
    tar_group = rep(seq_len(3), each = 2)
  )
  x <- value_init(object = object, iteration = "group")
  for (index in seq_len(3)) {
    exp <- object[object$tar_group == index, ]
    expect_equiv(value_produce_slice(x, index), exp)
  }
})

tar_test("value_hash_slice(group)", {
  object <- data_frame(
    x = letters[seq_len(6)],
    tar_group = rep(seq_len(3), each = 2)
  )
  x <- value_init(object = object, iteration = "group")
  for (index in seq_len(3)) {
    exp <- digest_obj32(object[object$tar_group == index, ])
    expect_equiv(value_hash_slice(x, index), exp)
  }
})

tar_test("value_hash_slices(group)", {
  object <- data_frame(
    x = letters[seq_len(6)],
    tar_group = rep(seq_len(3), each = 2)
  )
  x <- value_init(object = object, iteration = "group")
  exp <- map_chr(seq_len(3), function(index) {
    digest_obj32(object[object$tar_group == index, ])
  })
  expect_equal(value_hash_slices(x), exp)
})

tar_test("group aggregation", {
  object <- data_frame(
    x = letters[seq_len(6)],
    tar_group = rep(seq_len(3), each = 2)
  )
  out <- value_produce_aggregate.tar_group(1, list(object))
  expect_equal(out, object)
})

tar_test("group validate()", {
  object <- data_frame(
    x = letters[seq_len(6)],
    tar_group = rep(seq_len(3), each = 2)
  )
  x <- value_init(object = object, iteration = "group")
  expect_silent(value_validate(x))
})

tar_test("group validate() on a non data frame", {
  x <- value_init(object = "abc", iteration = "group")
  expect_error(value_validate(x), class = "tar_condition_validate")
})

tar_test("group validate() without tar_group column", {
  object <- data_frame(x = letters[seq_len(6)])
  x <- value_init(object = object, iteration = "group")
  expect_error(value_validate(x), class = "tar_condition_validate")
})
