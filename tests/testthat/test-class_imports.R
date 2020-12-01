tar_test("imports_validate()", {
  expect_silent(imports_validate(imports_new(new.env())))
  expect_error(imports_validate(new.env()), class = "condition_validate")
  expect_error(imports_validate(123), class = "condition_validate")
})

tar_test("imports_set_object()", {
  imports <- imports_new(new.env(parent = emptyenv()))
  envir <- new.env(parent = emptyenv())
  envir$a <- "x"
  expect_null(imports$a)
  imports_set_object(imports = imports, name = "a", envir = envir)
  expect_equal(imports$a, "x")
})
