tar_test("inner cross method", {
  x <- data.frame(x = seq_len(2))
  y <- data.frame(y = seq_len(2))
  methods <- dynamic_init()
  out <- methods$cross(x, y)
  exp <- data.frame(
    x = rep(seq_len(2), each = 2),
    y = rep(seq_len(2), times = 2)
  )
  expect_equal(out, exp)
})

tar_test("tar_dynamic_cross() input edge cases", {
  x <- data.frame(x = seq_len(2))
  y <- data.frame(
    y = head(letters, 2),
    z = tail(LETTERS, 2)
  )
  expect_silent(out <- tar_dynamic_cross(x, y))
  expect_true(is.data.frame(out))
  expect_error(tar_dynamic_cross(seq_len(4), 2), class = "condition_validate")
  expect_error(tar_dynamic_cross(list(x, y)), class = "condition_validate")
  x <- data.frame(y = seq_len(2))
  expect_error(tar_dynamic_cross(x, y), class = "condition_validate")
  colnames(x) <- ""
  expect_error(tar_dynamic_cross(x, y), class = "condition_validate")
  colnames(x) <- NULL
  expect_error(tar_dynamic_cross(x, y), class = "condition_validate")
})

tar_test("simple tar_dynamic_cross()", {
  x <- data_frame(x = seq_len(2))
  y <- data_frame(
    y = head(letters, 2),
    z = tail(LETTERS, 2)
  )
  exp <- data_frame(
    y = rep(head(letters, 2), each = 2),
    z = rep(tail(LETTERS, 2), each = 2),
    x = rep(seq_len(2), times = 2)
  )
  out <- tar_dynamic_cross(y, x)
  expect_equal(out, exp)
})

tar_test("composed tar_dynamic_cross()", {
  x <- data_frame(x = seq_len(2))
  y <- data_frame(y = head(letters, 2))
  z <- data_frame(z = tail(LETTERS, 2))
  exp <- data_frame(
    y = rep(head(letters, 2), each = 2),
    z = rep(tail(LETTERS, 2), each = 2),
    x = rep(seq_len(2), times = 2)
  )
  out <- tar_dynamic_cross(tar_dynamic_map(y, z), x)
  expect_equal(out, exp)
})
