tar_test("tar_pattern() input edge cases", {
  x <- data_frame(x = as.character(seq_len(2)))
  y <- head(letters, 2)
  z <- tail(LETTERS, 2)
  expect_error(tar_pattern(2), class = "condition_validate")
  expect_error(tar_pattern(map(x), y), class = "condition_validate")
  expect_error(tar_pattern(map(x), y, z = z), class = "condition_validate")
})

tar_test("tar_pattern() map unequal length inputs", {
  x <- head(letters, 3)
  y <- tail(LETTERS, 2)
  expect_error(tar_pattern(map(x, y), x = x, y = y), class = "condition_validate")
})

tar_test("tar_pattern() map", {
  x <- as.character(seq_len(2))
  y <- head(letters, 2)
  exp <- data_frame(
    y = head(letters, 2),
    x = as.character(seq_len(2))
  )
  out <- tar_pattern(map(y, x), x = x, y = y)
  expect_equal(out, exp)
})

tar_test("tar_pattern() cross", {
  x <- as.character(seq_len(2))
  y <- head(letters, 2)
  exp <- data_frame(
    y = rep(head(letters, 2), each = 2),
    x = rep(as.character(seq_len(2)), times = 2)
  )
  out <- tar_pattern(cross(y, x), x = x, y = y)
  expect_equal(out, exp)
})

tar_test("tar_pattern() composed cross", {
  x <- as.character(seq_len(2))
  y <- head(letters, 2)
  z <- tail(LETTERS, 2)
  exp <- data_frame(
    y = rep(head(letters, 2), each = 2),
    z = rep(tail(LETTERS, 2), each = 2),
    x = rep(as.character(seq_len(2)), times = 2)
  )
  out <- tar_pattern(cross(map(y, z), x), x = x, y = y, z = z)
  expect_equal(out, exp)
})

tar_test("tar_pattern() head with one var", {
  x <- as.character(seq_len(26))
  out <- tar_pattern(head(x, n = 3), x = x)
  exp <- data_frame(x = as.character(seq_len(3)))
  expect_equal(out, exp)
})

tar_test("tar_pattern() with many vars", {
  x <- as.character(seq_len(26))
  y <- letters
  z <- LETTERS
  out <- tar_pattern(head(map(x, y, z), n = 3), x = x, y = y, z = z)
  exp <- data_frame(
    x = as.character(seq_len(3)),
    y = head(letters, 3),
    z = head(LETTERS, 3)
  )
  expect_equal(out, exp)
})

tar_test("tar_pattern() tail with one var", {
  x <- as.character(seq_len(26))
  out <- tar_pattern(tail(x, n = 3), x = x)
  exp <- data_frame(x = tail(as.character(seq_len(26)), 3))
  expect_equal(out, exp)
})

tar_test("tar_pattern() tail with many vars", {
  x <- as.character(seq_len(26))
  y <- letters
  z <- LETTERS
  out <- tar_pattern(tail(map(x, y, z), n = 3), x = x, y = y, z = z)
  exp <- data_frame(
    x = tail(as.character(seq_len(26)), 3),
    y = tail(letters, 3),
    z = tail(LETTERS, 3)
  )
  expect_equal(out, exp)
})

tar_test("tar_pattern() sample with one var", {
  x <- as.character(seq_len(26))
  out <- tar_pattern(sample(x, n = 3), x = x, seed = 0)
  out2 <- tar_pattern(sample(x, n = 3), x = x, seed = 0)
  out3 <- tar_pattern(sample(x, n = 3), x = x, seed = 1)
  expect_equal(out, out2)
  expect_true(any(out$x != out3$x))
  expect_true(is.data.frame(out))
  expect_equal(dim(out), c(3L, 1L))
  expect_equal(colnames(out), "x")
  expect_true(all(out$x %in% seq_len(26)))
})

tar_test("tar_pattern() sample with many vars", {
  x <- as.character(seq_len(26))
  y <- letters
  z <- LETTERS
  out <- tar_pattern(sample(map(x, y, z), n = 3), x = x, y = y, z = z)
  expect_true(is.data.frame(out))
  expect_equal(dim(out), c(3L, 3L))
  expect_equal(colnames(out), c("x", "y", "z"))
  expect_true(all(out$x %in% seq_len(26)))
  expect_true(all(out$y %in% letters))
  expect_true(all(out$z %in% LETTERS))
})
