tar_test("junction deps", {
  x <- junction_init("x", letters, list(a = LETTERS, b = rev(letters)))
  out <- x$deps
  exp <- data_frame(a = LETTERS, b = rev(letters))
  expect_equal(out, exp)
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

tar_test("junction_transpose() without deps", {
  names <- paste0("child_", seq_len(3))
  junction <- junction_init("parent", names)
  out <- junction_transpose(junction)
  exp <- list(deps = character(0), split = "child_1", index = 1L)
  expect_equal(out[[1]], exp)
  exp <- list(deps = character(0), split = "child_2", index = 2L)
  expect_equal(out[[2]], exp)
  exp <- list(deps = character(0), split = "child_3", index = 3L)
  expect_equal(out[[3]], exp)
})

tar_test("junction transpose() with deps", {
  names <-  paste0("child_", seq_len(3))
  x <- paste0("x_", seq_len(3))
  y <- paste0("y_", seq_len(3))
  junction <- junction_init("parent", names, list(x, y))
  out <- junction_transpose(junction)
  exp <- list(deps = sort(c("x_1", "y_1")), split = "child_1", index = 1L)
  expect_equal(out[[1]], exp)
  exp <- list(deps = sort(c("x_2", "y_2")), split = "child_2", index = 2L)
  expect_equal(out[[2]], exp)
  exp <- list(deps = sort(c("x_3", "y_3")), split = "child_3", index = 3L)
  expect_equal(out[[3]], exp)
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
