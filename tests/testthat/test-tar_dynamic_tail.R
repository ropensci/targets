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
