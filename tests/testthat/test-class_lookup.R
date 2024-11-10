tar_test("class lookup", {
  lookup <- lookup_new()
  expect_equal(lookup_count(lookup), 0L)
  lookup_set(lookup, names = letters, object = TRUE)
  lookup_set(lookup, names = LETTERS, object = FALSE)
  expect_equal(lookup_count(lookup), length(letters) + length(LETTERS))
  expect_equal(sort(lookup_list(lookup)), sort(c(letters, LETTERS)))
  for (x in letters) {
    expect_true(lookup_exists(lookup, x))
    expect_false(lookup_missing(lookup, x))
    expect_true(lookup_get(lookup, x))
  }
  for (x in LETTERS) {
    expect_true(lookup_exists(lookup, x))
    expect_false(lookup_missing(lookup, x))
    expect_false(lookup_get(lookup, x))
  }
  expect_false(lookup_exists(lookup, "abc"))
  expect_true(lookup_missing(lookup, "abc"))
  lookup_set(lookup, "abc", FALSE)
  expect_true(lookup_exists(lookup, "abc"))
  expect_false(lookup_get(lookup, "abc"))
  lookup_unset(lookup, "abc")
  expect_false(lookup_exists(lookup, "abc"))
  expect_null(lookup_get(lookup, "abc"))
  expect_silent(lookup_validate(lookup))
})

tar_test("class lookup remove method", {
  lookup <- lookup_new()
  lookup_set(lookup, names = letters, object = TRUE)
  expect_equal(sort(lookup_list(lookup)), sort(letters))
  lookup_remove(lookup, letters)
  expect_equal(lookup_list(lookup), character(0L))
})

tar_test("class lookup methods on NULL objects", {
  expect_false(lookup_exists(NULL, "a"))
  expect_true(lookup_missing(NULL, "a"))
  expect_null(lookup_get(NULL, "a"))
  expect_equal(lookup_list(NULL), character(0L))
})
