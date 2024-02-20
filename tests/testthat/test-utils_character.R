tar_test("truncate_character()", {
  x <- c(
    "not so very long",
    "very very very very very very long",
    NA_character_,
    "short"
  )
  expect_equal(truncate_character(x, 1e3L), x)
  out <- truncate_character(x, 4L)
  expect_equal(out, c("n...", "v...", NA_character_, "s..."))
  out <- truncate_character(x, 5L)
  expect_equal(out, c("no...", "ve...", NA_character_, "short"))
  out <- truncate_character(x, 7L)
  expect_equal(out, c("not ...", "very...", NA_character_, "short"))
})
