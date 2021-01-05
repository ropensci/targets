tar_test("tar_branches() with a nontrivial pattern", {
  tar_script(
    list(
      tar_target(x, seq_len(2)),
      tar_target(y, head(letters, 2)),
      tar_target(z, head(LETTERS, 2)),
      tar_target(dynamic, c(x, y, z), pattern = cross(z, map(x, y)))
    )
  )
  tar_make(callr_function = NULL)
  out <- tar_branches(dynamic, pattern = cross(z, map(x, y)))
  expect_equal(dim(out), c(4L, 4L))
  expect_true(all(c("dynamic", "z", "x", "y") %in% colnames(out)))
  exp <- tar_meta(dynamic, children)$children[[1]]
  expect_equal(out$dynamic, exp)
  children <- tar_meta(x, children)$children[[1]]
  exp <- rep(children, times = 2)
  expect_equal(out$x, exp)
  children <- tar_meta(y, children)$children[[1]]
  exp <- rep(children, times = 2)
  expect_equal(out$y, exp)
  children <- tar_meta(z, children)$children[[1]]
  exp <- rep(children, each = 2)
  expect_equal(out$z, exp)
})

tar_test("tar_branches() with a random pattern", {
  tar_script(
    list(
      tar_target(w, letters),
      tar_target(x, w, pattern = map(w)),
      tar_target(dynamic, x, pattern = sample(x, 10))
    )
  )
  tar_make(callr_function = NULL)
  out <- tar_branches(dynamic, pattern = sample(x, 10))
  expect_equal(dim(out), c(10L, 2L))
  expect_true(all(c("dynamic", "x") %in% colnames(out)))
  exp <- tar_meta(dynamic, children)$children[[1]]
  expect_equal(out$dynamic, exp)
  for (i in seq_len(nrow(out))) {
    dynamic <- tar_read_raw(out$dynamic[i])
    x <- tar_read_raw(out$x[i])
    expect_equal(dynamic, x)
  }
})
