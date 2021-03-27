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
  imports_set_envir(imports = imports, envir = envir)
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

tar_test("imports setting works", {
  old <- Sys.getenv("TAR_WARN")
  on.exit(Sys.setenv(TAR_WARN = old))
  Sys.setenv(TAR_WARN = "false") # This test has a good reason for load_all().
  skip_if_not_installed("pkgload")
  dir_create("pkg")
  dir_create(file.path("pkg", "R"))
  writeLines(
    "f <- function(x) g(x); g <- function(x) x + 1L",
    file.path("pkg", "R", "fun.R")
  )
  writeLines(
    c(
      "Package: pkgabcdefg",
      "Maintainer: John Doe <e@mail.com>",
      "Type: Package",
      "Version: 0.0.1"
    ),
    file.path("pkg", "DESCRIPTION")
  )
  tar_script({
    pkgload::load_all("pkg", quiet = TRUE)
    tar_option_set(imports = "pkgabcdefg")
    list(tar_target(x, f(1L)))
  })
  out <- tar_network(callr_function = NULL)$edges
  expect_true(any(out$from == "g" & out$to == "f"))
  expect_true(any(out$from == "f" & out$to == "x"))
  tar_make(callr_function = NULL)
  meta <- tar_meta(names = c("f", "g", "x"))
  expect_true(all(c("f", "g", "x") %in% meta$name))
  expect_equal(tar_read(x), 2L)
  # Should be up to date.
  tar_make(callr_function = NULL)
  expect_equal(nrow(tar_progress()), 0L)
  out <- tar_outdated(callr_function = NULL, targets_only = FALSE)
  expect_equal(out, character(0))
  # Change the inner function.
  writeLines(
    "f <- function(x) g(x); g <- function(x) x + 2L",
    file.path("pkg", "R", "fun.R")
  )
  out <- tar_outdated(callr_function = NULL, targets_only = FALSE)
  expect_true(all(c("f", "g", "x") %in% out))
  tar_make(callr_function = NULL)
  expect_equal(tar_progress()$name, "x")
  expect_equal(tar_read(x), 3L)
})
