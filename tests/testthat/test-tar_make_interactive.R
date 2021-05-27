tar_test("tar_make_interactive()", {
  dir.create(path_store_default())
  code <- c("list(", "tar_target(x, \"a\"),", "tar_target(y, \"b\")", ")")
  tar_make_interactive(code)
  expect_equal(tar_option_get("envir")$x, "a")
  expect_equal(tar_option_get("envir")$y, "b")
  expect_true(file.exists("_targets"))
})

tar_test("tar_make_interactive() with dynamic branching", {
  dir.create(path_store_default())
  code <- c(
    "list(",
    "tar_target(x, letters[seq_len(2)]),",
    "tar_target(y, x, pattern = map(x))",
    ")"
  )
  tar_make_interactive(code)
  expect_equal(tar_option_get("envir")$x, letters[seq_len(2)])
  y <- tar_option_get("envir")$y
  expect_equal(tar_option_get("envir")$y, letters[seq_len(2)])
  expect_true(file.exists("_targets"))
})