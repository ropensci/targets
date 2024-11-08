tar_test("junction with deps", {
  x <- junction_init("x", letters, list(a = LETTERS, b = rev(letters)))
  out <- x$deps
  exp <- data_frame(a = LETTERS, b = rev(letters))
  expect_equal(out, exp)
  expect_true(x$has_deps)
})

tar_test("junction without deps", {
  skip_cran()
  x <- junction_init("x", letters, list())
  expect_equal(x$deps, data.frame())
  expect_false(x$has_deps)
})

tar_test("junction_length()", {
  x <- junction_init("x", letters, list(a = LETTERS, b = rev(letters)))
  expect_equal(junction_length(x), length(letters))
})

tar_test("junction_splits()", {
  x <- junction_init("x", letters, list(a = LETTERS, b = rev(letters)))
  expect_equal(junction_splits(x), letters)
})

tar_test("junction_extract_index()", {
  x <- junction_init("x", letters, list(a = LETTERS, b = rev(letters)))
  expect_equal(junction_extract_index(x, "j"), 10L)
})

tar_test("junction_extract_deps()", {
  x <- junction_init("x", letters, list(a = LETTERS, b = rev(letters)))
  expect_equal(junction_extract_deps(x, 10L), c("J", "q"))
})

tar_test("junction_invalidate()", {
  x <- junction_init("x", letters, list(a = LETTERS, b = rev(letters)))
  junction_invalidate(x)
  expect_equal(junction_splits(x), rep(NA_character_, length(x$index)))
})

tar_test("junction_upstream_edges()", {
  names <-  paste0("child_", seq_len(3))
  x <- paste0("x_", seq_len(3))
  y <- paste0("y_", seq_len(3))
  junction <- junction_init("parent", names, list(x, y))
  out <- junction_upstream_edges(junction)
  exp <- data_frame(from = c(x, y), to = c(names, names))
  expect_equal(out, exp)
})

tar_test("junction_validate()", {
  x <- junction_init("x", letters, list(LETTERS, rev = rev(letters)))
  expect_silent(junction_validate(x))
})

tar_test("junction_validate() with an extra field", {
  x <- junction_init("x", letters, list(LETTERS, rev = rev(letters)))
  x$bad <- 123
  expect_error(junction_validate(x), class = "tar_condition_validate")
})

tar_test("junction_validate() with bad deps", {
  x <- junction_init("x", letters, list(LETTERS, rev = rev(letters)))
  x$deps <- list(x = 123)
  expect_error(junction_validate(x), class = "tar_condition_validate")
})
