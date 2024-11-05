tar_test("class lookup", {
  lookup <- lookup_init(true = letters, false = LETTERS)
  for (x in letters) {
    expect_true(lookup_exists(lookup, x))
    expect_true(lookup_true(lookup, x))
  }
  for (x in LETTERS) {
    expect_true(lookup_exists(lookup, x))
    expect_false(lookup_true(lookup, x))
  }
  expect_false(lookup_exists(lookup, "abc"))
  lookup_set(lookup, "abc", FALSE)
  expect_true(lookup_exists(lookup, "abc"))
  expect_false(lookup_true(lookup, "abc"))
  expect_silent(lookup_validate(lookup))
  lookup$x <- 123L
  expect_error(lookup_validate(lookup), class = "tar_condition_validate")
})
