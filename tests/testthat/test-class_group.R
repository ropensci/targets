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
    exp_object <- object[object$tar_group == index, ]
    exp_object$tar_group <- NULL
    exp <- hash_object(exp_object)
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
    exp_object <- object[object$tar_group == index, ]
    exp_object$tar_group <- NULL
    hash_object(exp_object)
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

tar_test("group branch invalidation (#507)", {
  tar_script({
    list(
      tar_target(
        x,
        data.frame(
          x = c("b", "c"),
          tar_group = c(1L, 2L),
          stringsAsFactors = FALSE
        ),
        iteration = "group"
      ),
      tar_target(y, x, pattern = map(x))
    )
  })
  tar_make(callr_function = NULL)
  tar_script({
    list(
      tar_target(
        x,
        data.frame(
          x = c("b", "c", "a"),
          tar_group = c(2L, 3L, 1L),
          stringsAsFactors = FALSE
        ),
        iteration = "group"
      ),
      tar_target(y, x, pattern = map(x))
    )
  })
  tar_make(callr_function = NULL)
  out <- tar_progress()
  out <- out$name[out$progress == "completed" & grepl("^y_", out$name)]
  expect_equal(length(out), 1L)
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
