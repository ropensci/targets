tar_test("%||%", {
  expect_equal("x" %||% "y", "x")
  expect_equal(character(0) %||% "y", "y")
  expect_equal(NA_character_ %||% "y", NA_character_)
  expect_equal(NULL %||% "y", "y")
})

tar_test("%|||%", {
  expect_equal("x" %|||% "y", "x")
  expect_equal(character(0) %|||% "y", character(0))
  expect_equal(NA_character_ %|||% "y", NA_character_)
  expect_equal(NULL %|||% "y", "y")
})

tar_test("%||NA%", {
  expect_equal("x" %||NA% "y", "x")
  expect_equal(character(0) %||NA% "y", character(0))
  expect_equal(NA_character_ %||NA% "y", "y")
  expect_null(NULL %||NA% "y")
})

tar_test("%||nf%", {
  expect_equal("x" %||nf% "y", "x")
  expect_equal(character(0) %||nf% "y", "y")
  expect_equal(NA_character_ %||nf% "y", "y")
  expect_equal(NULL %||nf% "y", "y")
})

tar_test("if_any()", {
  expect_equal(if_any(TRUE, "x", "y"), "x")
  expect_equal(if_any(FALSE, "x", "y"), "y")
})
