tar_test("class lookup_table", {
  lookup_table <- lookup_table_new()
  expect_silent(lookup_table_validate(lookup_table))
  expect_false(lookup_table_exists(lookup_table, "a", "x"))
  expect_false(lookup_table_exists_category(lookup_table, "a"))
  expect_null(lookup_table_true(lookup_table, "a", "x"))
  expect_equal(lookup_table_list(lookup_table, "a"), character(0L))
  expect_false(lookup_table_exists_category(lookup_table, "category1"))
  expect_false(lookup_table_exists_category(lookup_table, "category2"))
  lookup_table_set(lookup_table, "category1", letters, TRUE)
  lookup_table_set(lookup_table, "category1", LETTERS, FALSE)
  lookup_table_set(lookup_table, "category2", LETTERS, TRUE)
  lookup_table_set(lookup_table, "category2", letters, FALSE)
  expect_true(lookup_table_exists_category(lookup_table, "category1"))
  expect_true(lookup_table_exists_category(lookup_table, "category2"))
  expect_silent(lookup_table_validate(lookup_table))
  for (name in letters) {
    expect_true(lookup_table_exists(lookup_table, "category1", name))
    expect_true(lookup_table_exists(lookup_table, "category2", name))
    expect_true(lookup_table_true(lookup_table, "category1", name))
    expect_false(lookup_table_true(lookup_table, "category2", name))
  }
  for (name in LETTERS) {
    expect_true(lookup_table_exists(lookup_table, "category1", name))
    expect_true(lookup_table_exists(lookup_table, "category2", name))
    expect_false(lookup_table_true(lookup_table, "category1", name))
    expect_true(lookup_table_true(lookup_table, "category2", name))
  }
})
