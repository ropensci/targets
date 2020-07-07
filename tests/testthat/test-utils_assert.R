tar_test("assert_package()", {
  expect_error(assert_package("_illegal"), class = "condition_targets")
})

tar_test("assert_chr_no_delim()", {
  expect_error(assert_chr_no_delim(0L), class = "condition_validate")
  expect_error(assert_chr_no_delim("a|b"), class = "condition_validate")
  expect_error(assert_chr_no_delim("a*b"), class = "condition_validate")
})

tar_test("assert_function()", {
  expect_error(assert_function("not"), class = "condition_validate")
})

tar_test("assert_df()", {
  expect_silent(assert_df(data_frame(x = 1)))
  expect_error(assert_df(TRUE), class = "condition_validate")
})

tar_test("assert_lgl()", {
  expect_silent(assert_lgl(FALSE))
  expect_error(assert_lgl("abc"), class = "condition_validate")
})

tar_test("assert_list()", {
  expect_silent(assert_list(list("abc")))
  expect_error(assert_list("abc"), class = "condition_validate")
})

tar_test("assert_nonempty()", {
  expect_silent(assert_nonempty("abc"))
  expect_silent(assert_nonempty(NA_character_))
  expect_error(assert_nonempty(NULL), class = "condition_validate")
  expect_error(assert_nonempty(character(0)), class = "condition_validate")
})

tar_test("assert_nonmissing()", {
  expect_silent(assert_nonmissing("abc"))
  expect_error(assert_nonmissing(NA_character_), class = "condition_validate")
})

tar_test("assert_path()", {
  file.create("x")
  expect_error(assert_path(c("x", "y")), class = "condition_validate")
  file.create("y")
  expect_silent(assert_path(c("x", "y")))
})

tar_test("assert_store()", {
  expect_error(assert_store(), class = "condition_validate")
  dir.create("_targets")
  expect_silent(assert_store())
})

tar_test("assert_target_script()", {
  expect_error(assert_target_script(), class = "condition_validate")
  file.create("_targets.R")
  expect_silent(assert_target_script())
})

tar_test("assert_true()", {
  expect_silent(assert_true(TRUE, "x"))
  expect_error(assert_true(FALSE, "x"), class = "condition_validate")
})

tar_test("assert_unique_targets()", {
  expect_silent(assert_unique_targets(letters))
  expect_error(
    assert_unique_targets(c("a", "a", "b", "b")),
    class = "condition_validate"
  )
})

tar_test("warn_template()", {
  expect_warning(
    tar_target(x, 1, template = list(x = 1)),
    class = "condition_validate"
  )
})
