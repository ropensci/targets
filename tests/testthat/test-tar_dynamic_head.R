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
