tar_test("inner map method", {
  x <- data.frame(x = seq_len(2))
  methods <- dynamic_init()
  expect_equal(methods$map(x), x)
})

tar_test("tar_dynamic_map() input edge cases", {
  x <- data.frame(x = seq_len(2))
  y <- data.frame(
    y = head(letters, 2),
    z = tail(LETTERS, 2)
  )
  expect_silent(out <- tar_dynamic_map(x, y))
  expect_true(is.data.frame(out))
  expect_error(tar_dynamic_map(seq_len(4), 2), class = "condition_validate")
  expect_error(tar_dynamic_map(list(x, y)), class = "condition_validate")
  x <- data.frame(y = seq_len(2))
  expect_error(tar_dynamic_map(x, y), class = "condition_validate")
  colnames(x) <- ""
  expect_error(tar_dynamic_map(x, y), class = "condition_validate")
  colnames(x) <- NULL
  expect_error(tar_dynamic_map(x, y), class = "condition_validate")
})

tar_test("tar_dynamic_map() unequal length inputs", {
  x <- data.frame(x = seq_len(3))
  y <- data.frame(
    y = head(letters, 2),
    z = tail(LETTERS, 2)
  )
  expect_error(tar_dynamic_map(x, y), class = "condition_validate")
})

tar_test("tar_dynamic_map() desired answer", {
  x <- data_frame(x = seq_len(2))
  y <- data_frame(
    y = head(letters, 2),
    z = tail(LETTERS, 2)
  )
  exp <- data_frame(
    y = head(letters, 2),
    z = tail(LETTERS, 2),
    x = seq_len(2)
  )
  out <- tar_dynamic_map(y, x)
  expect_equal(out, exp)
})
