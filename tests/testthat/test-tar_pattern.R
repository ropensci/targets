tar_test("tar_pattern() input edge cases", {
  expect_error(tar_pattern(2), class = "condition_validate")
  expect_error(tar_pattern(map(x), x = "x"), class = "condition_validate")
  expect_error(tar_pattern(map(x), 3, x = 1), class = "condition_validate")
})

tar_test("tar_pattern() map unequal length inputs", {
  expect_error(
    tar_pattern(map(x, y), x = 2, y = 3),
    class = "condition_validate"
  )
})

tar_test("tar_pattern() map", {
  exp <- data_frame(
    y = paste("y", seq_len(2), sep = "_"),
    x = paste("x", seq_len(2), sep = "_")
  )
  out <- tar_pattern(map(y, x), x = 2, y = 2)
  expect_equiv(out, exp)
})

tar_test("tar_pattern() cross", {
  exp <- data_frame(
    y = rep(paste("y", seq_len(2), sep = "_"), each = 2),
    x = rep(paste("x", seq_len(2), sep = "_"), times = 2)
  )
  out <- tar_pattern(cross(y, x), x = 2, y = 2)
  expect_equiv(out, exp)
})

tar_test("tar_pattern() composed cross", {
  exp <- data_frame(
    y = rep(paste("y", seq_len(2), sep = "_"), each = 2),
    z = rep(paste("z", seq_len(2), sep = "_"), each = 2),
    x = rep(paste("x", seq_len(2), sep = "_"), times = 2)
  )
  out <- tar_pattern(cross(map(y, z), x), x = 2, y = 2, z = 2)
  expect_equiv(out, exp)
})

tar_test("tar_pattern() head with one var", {
  out <- tar_pattern(head(x, n = 3), x = 26)
  exp <- data_frame(x = paste("x", seq_len(3), sep = "_"))
  expect_equiv(out, exp)
})

tar_test("tar_pattern() with many vars", {
  out <- tar_pattern(head(map(x, y, z), n = 3), x = 3, y = 3, z = 3)
  exp <- data_frame(
    x = paste("x", seq_len(3), sep = "_"),
    y = paste("y", seq_len(3), sep = "_"),
    z = paste("z", seq_len(3), sep = "_")
  )
  expect_equiv(out, exp)
})

tar_test("tar_pattern() tail with one var", {
  out <- tar_pattern(tail(x, n = 3), x = 26)
  exp <- data_frame(x = tail(paste("x", seq_len(26), sep = "_"), 3))
  expect_equiv(out, exp)
})

tar_test("tar_pattern() tail with many vars", {
  out <- tar_pattern(tail(map(x, y, z), n = 3), x = 26, y = 26, z = 26)
  exp <- data_frame(
    x = tail(paste("x", seq_len(26), sep = "_"), 3),
    y = tail(paste("y", seq_len(26), sep = "_"), 3),
    z = tail(paste("z", seq_len(26), sep = "_"), 3)
  )
  expect_equiv(out, exp)
})

tar_test("tar_pattern() sample with one var", {
  out <- tar_pattern(sample(x, n = 3), x = 26, seed = 0)
  out2 <- tar_pattern(sample(x, n = 3), x = 26, seed = 0)
  out3 <- tar_pattern(sample(x, n = 3), x = 26, seed = 1)
  expect_equal(out, out2)
  expect_true(any(out$x != out3$x))
  expect_true(is.data.frame(out))
  expect_equal(dim(out), c(3L, 1L))
  expect_equal(colnames(out), "x")
  expect_true(all(out$x %in% paste("x", seq_len(26), sep = "_")))
})

tar_test("tar_pattern() sample with many vars", {
  out <- tar_pattern(sample(map(x, y, z), n = 3), x = 26, y = 26, z = 26)
  expect_true(is.data.frame(out))
  expect_equal(dim(out), c(3L, 3L))
  expect_equal(colnames(out), c("x", "y", "z"))
  expect_true(all(out$x %in% paste("x", seq_len(26), sep = "_")))
  expect_true(all(out$y %in% paste("y", seq_len(26), sep = "_")))
  expect_true(all(out$z %in% paste("z", seq_len(26), sep = "_")))
})
