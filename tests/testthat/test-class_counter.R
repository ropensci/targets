tar_test("counter_get_names()", {
  out <- counter_get_names(counter_init(letters))
  expect_equal(sort(out), sort(letters))
})

tar_test("counter$exists_name()", {
  out <- counter_init(letters)
  expect_true(counter_exists_name(out, "b"))
  expect_false(counter_exists_name(out, "abcde"))
})

tar_test("counter_exist_names()", {
  out <- counter_init(letters)
  expect_equal(
    counter_exist_names(out, c("b", "abcde", "a")),
    c(TRUE, FALSE, TRUE)
  )
})

tar_test("counter_filter_exists()", {
  x <- counter_init(letters)
  out <- counter_filter_exists(x, c("b", "xyz", "a", "cde"))
  expect_equal(sort(out), sort(c("a", "b")))
  out <- counter_filter_exists(x, c("nothing"))
  expect_equal(out, character(0))
})

tar_test("counter_count_exists()", {
  x <- counter_init(letters)
  out <- counter_count_exists(x, c("b", "xyz", "a", "cde"))
  expect_equal(out, 2L)
  out <- counter_count_exists(x, c("nothing"))
  expect_equal(out, 0L)
})

tar_test("counter_set_name(new)", {
  x <- counter_init(letters)
  expect_false(counter_exists_name(x, "abc"))
  expect_equal(x$count, 26L)
  counter_set_name(x, "abc")
  expect_true(counter_exists_name(x, "abc"))
  expect_equal(x$count, 27L)
})

tar_test("counter_set_new_names()", {
  x <- counter_init(letters)
  expect_false(counter_exists_name(x, "abc"))
  expect_equal(x$count, 26L)
  counter_set_new_names(x, c("abc", "xyz"))
  expect_true(counter_exists_name(x, "abc"))
  expect_true(counter_exists_name(x, "xyz"))
  expect_equal(x$count, 28L)
})

tar_test("counter_set_name(new) on an empty counter", {
  x <- counter_init()
  expect_false(counter_exists_name(x, "abc"))
  expect_equal(x$count, 0L)
  counter_set_name(x, "abc")
  expect_true(counter_exists_name(x, "abc"))
  expect_equal(x$count, 1L)
})

tar_test("counter_set_name(old)", {
  x <- counter_init(letters)
  expect_true(counter_exists_name(x, "b"))
  expect_equal(x$count, 26L)
  counter_set_name(x, "b")
  expect_true(counter_exists_name(x, "b"))
  expect_equal(x$count, 26L)
})

tar_test("counter$del_name(new)", {
  x <- counter_init(letters)
  expect_false(counter_exists_name(x, "abc"))
  expect_equal(x$count, 26L)
  counter_del_name(x, "abc")
  expect_false(counter_exists_name(x, "abc"))
  expect_equal(x$count, 26L)
})

tar_test("counter$del_name(old)", {
  x <- counter_init(letters)
  expect_true(counter_exists_name(x, "b"))
  expect_equal(x$count, 26L)
  counter_del_name(x, "b")
  expect_false(counter_exists_name(x, "b"))
  expect_equal(x$count, 25L)
})

tar_test("counter$del_name(old, nonexistent)", {
  x <- counter_init(letters)
  expect_equal(sort(counter_get_names(x)), sort(letters))
  expect_equal(x$count, 26L)
  counter_del_name(x, "bcde")
  expect_equal(sort(counter_get_names(x)), sort(letters))
  expect_equal(x$count, 26L)
})

tar_test("counter_validate() nonempty counter", {
  x <- counter_init(letters)
  expect_silent(counter_validate(x))
})

tar_test("counter_validate() empty counter", {
  x <- counter_init()
  expect_silent(counter_validate(x))
})

tar_test("counter_validate() with a count mismatch", {
  x <- counter_init(letters)
  x$count <- 0L
  expect_error(counter_validate(x), class = "tar_condition_validate")
})
