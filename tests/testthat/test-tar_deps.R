tar_test("tar_deps(expr)", {
  out <- tar_deps(x <- y + z)
  expect_true(all(c("y", "z") %in% out))
  expect_false("x" %in% out)
})

tar_test("tar_deps(fun)", {
  out <- tar_deps(function(a = b) map_dfr(data, ~do_row(.x)))
  expect_true(all(c("b", "data", "do_row", "map_dfr") %in% out))
  expect_false(any(c("a", "~") %in% out))
})

tar_test("tar_deps_raw(expr)", {
  out <- tar_deps_raw(quote(x <- y + z))
  expect_true(all(c("y", "z") %in% out))
  expect_false("x" %in% out)
})

tar_test("tar_deps_raw(fun)", {
  out <- tar_deps_raw(function(a = b) map_dfr(data, ~do_row(.x)))
  expect_true(all(c("b", "data", "do_row", "map_dfr") %in% out))
  expect_false(any(c("a", "~") %in% out))
})
