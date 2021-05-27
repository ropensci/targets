tar_test("tar_make_interactive()", {
  dir.create(path_store_default())
  code <- c("list(", "tar_target(x, \"a\"),", "tar_target(y, \"b\")", ")")
  tar_make_interactive(code)
  expect_true(file.exists("_targets"))
  expect_equal(tar_option_get("envir")$x, "a")
  expect_equal(tar_option_get("envir")$y, "b")
})

tar_test("tar_make_interactive() with dynamic branching", {
  dir.create(path_store_default())
  code <- c(
    "list(",
    "tar_target(x, letters[seq_len(2)]),",
    "tar_target(y, toupper(x), pattern = map(x))",
    ")"
  )
  tar_make_interactive(code)
  expect_true(file.exists("_targets"))
  expect_equal(tar_option_get("envir")$x, letters[seq_len(2)])
  y <- tar_option_get("envir")$y
  expect_equal(unname(tar_option_get("envir")$y), LETTERS[seq_len(2)])
  y <- tar_option_get("envir")$y
  y1 <- names(y[y == "A"])
  y2 <- names(y[y == "B"])
  expect_equal(unname(tar_option_get("envir")[[y1]]), "A")
  expect_equal(unname(tar_option_get("envir")[[y2]]), "B")
})
