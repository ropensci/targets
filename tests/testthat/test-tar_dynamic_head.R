tar_test("tar_dynamic_head() with one var", {
  x <- data_frame(x = seq_len(26))
  out <- tar_dynamic_head(x, n = 3)
  exp <- data_frame(x = seq_len(3))
  expect_equal(out, exp)
})

tar_test("tar_dynamic_head() with many vars", {
  x <- data_frame(x = seq_len(26))
  y <- data_frame(y = letters, z = LETTERS)
  out <- tar_dynamic_head(tar_dynamic_map(x, y), n = 3)
  exp <- data_frame(
    x = seq_len(3),
    y = head(letters, 3),
    z = head(LETTERS, 3)
  )
  expect_equal(out, exp)
})

tar_test("head pattern in target", {
  tar_script({
    tar_pipeline(
      tar_target(x, seq_len(26)),
      tar_target(dynamic, x, pattern = head(x, n = 2))
    )
  })
  tar_make(callr_function = NULL)
  expect_equal(tar_read(dynamic), seq_len(2))
})
