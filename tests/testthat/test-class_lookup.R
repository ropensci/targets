tar_test("class lookup", {
  lookup <- lookup_init(letters)
  for (x in letters) {
    expect_true(lookup[[x]])
    expect_true(lookup_exists(lookup, x))
  }
  for (x in LETTERS) {
    expect_null(lookup[[x]])
    expect_false(lookup_exists(lookup, x))
  }
  lookup_set(lookup, "A")
  expect_true(lookup_exists(lookup, "A"))
  expect_silent(lookup_validate(lookup))
  lookup$x <- FALSE
  expect_error(lookup_validate(lookup), class = "tar_condition_validate")
})
