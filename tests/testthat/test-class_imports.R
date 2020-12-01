tar_test("imports_set_object()", {
  imports <- imports_new(new.env(parent = emptyenv()))
  envir <- new.env(parent = emptyenv())
  envir$a <- "x"
  expect_null(imports$a)
  imports_set_object(imports = imports, name = "a", envir = envir)
  expect_equal(imports$a, "x")
})

tar_test("imports_set_envir()", {
  imports <- imports_new(new.env(parent = emptyenv()))
  envir <- new.env(parent = emptyenv())
  envir$.a <- "x"
  envir$b <- "y"
  envir$c <- "z"
  expect_null(imports$.a)
  expect_null(imports$b)
  expect_null(imports$c)
  imports_set_envir(imports = imports,envir = envir)
  expect_equal(imports$.a, "x")
  expect_equal(imports$b, "y")
  expect_equal(imports$c, "z")
})

tar_test("imports_set_package()", {
  imports <- imports_new(new.env(parent = emptyenv()))
  expect_null(imports$head)
  imports_set_package(imports = imports, package = "utils")
  expect_true(is.function(imports$head))
})

tar_test("imports_set_package()", {
  imports <- imports_new(new.env(parent = emptyenv()))
  expect_null(imports$head)
  imports_set_package(imports = imports, package = "utils")
  expect_true(is.function(imports$head))
})

tar_test("imports_init()", {
  tar_option_set(imports = c("utils", "digest"))
  envir <- new.env(parent = emptyenv())
  envir$head <- "abc"
  expect_null(envir$tail)
  expect_null(envir$digest)
  imports <- imports_init(envir)
  expect_equal(imports$head, "abc")
  expect_true(is.function(imports$tail))
  expect_true(is.function(imports$digest))
  expect_null(envir$tail)
  expect_null(envir$digest)
  expect_true(inherits(imports, "tar_imports"))
  expect_false(inherits(envir, "tar_imports"))
})

tar_test("imports_init() idempotence", {
  tar_option_set(imports = c("utils", "digest"))
  imports <- imports_init(imports_new(new.env(parent = emptyenv())))
  expect_true(inherits(imports, "tar_imports"))
  expect_equal(length(imports), 0L)
  expect_null(imports$head)
})

tar_test("imports_validate()", {
  expect_silent(imports_validate(imports_new(new.env())))
  expect_error(imports_validate(new.env()), class = "condition_validate")
  expect_error(imports_validate(123), class = "condition_validate")
})

