tar_test("inner tail method", {
  x <- data.frame(x = seq_len(10))
  methods <- dynamic_init()
  expect_equal(methods$tail(x, n = 2), data.frame(x = c(9L, 10L)))
})

tar_test("tar_dynamic_tail() with one var", {
  x <- data_frame(x = seq_len(26))
  out <- tar_dynamic_tail(x, n = 3)
  exp <- data_frame(x = tail(seq_len(26), 3))
  expect_equal(out, exp)
})

tar_test("tar_dynamic_tail() with many vars", {
  x <- data_frame(x = seq_len(26))
  y <- data_frame(y = letters, z = LETTERS)
  out <- tar_dynamic_tail(tar_dynamic_map(x, y), n = 3)
  exp <- data_frame(
    x = tail(seq_len(26), 3),
    y = tail(letters, 3),
    z = tail(LETTERS, 3)
  )
  expect_equal(out, exp)
})

tar_test("tail pattern in target", {
  tar_script({
    tar_pipeline(
      tar_target(x, seq_len(26)),
      tar_target(dynamic, x, pattern = tail(x, n = 2))
    )
  })
  tar_make(callr_function = NULL)
  expect_equal(tar_read(dynamic), tail(seq_len(26), 2))
})
