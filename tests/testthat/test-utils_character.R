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

tar_test("string_wrap()", {
  x <- "a b c d e f g h i j k l m n o p q r s t u v w x y z"
  out <- string_wrap(x, width = 10L, separator = "\n")
  expect_equal(out, "a b c d e\nf g h i j\nk l m n o\np q r s t\nu v w x y\nz")
})
