tar_test("command$update_string()", {
  command <- command_init(quote(a <- b + c))
  expect_equal(command$string, "expression(a <- b + c)")
})

tar_test("command$hash", {
  command <- command_init(quote(a <- b + c))
  out <- command$hash
  expect_true(is.character(out))
  expect_equal(length(out), 1L)
  expect_equal(nchar(out), 16L)
})

tar_test("command_produce_build()", {
  command <- command_init(expr = quote(a <- b + c))
  b <- 1L
  c <- 2L
  envir <- environment()
  build <- command_produce_build(command, envir)
  expect_silent(build_validate(build))
  expect_equal(build$object, 3L)
  expect_true(is.numeric(build$metrics$seconds))
})

tar_test("command$produce_build() uses seed", {
  x <- command_init(expr = quote(sample.int(1e9, 1L)))
  x$seed <- 0L
  exp0 <- withr::with_seed(0L, sample.int(1e9, 1L))
  for (i in seq_len(2)) {
    out <- command_produce_build(x, environment())$object
    expect_equal(out, exp0)
  }
  x$seed <- 1L
  exp1 <- withr::with_seed(1L, sample.int(1e9, 1))
  for (i in seq_len(2)) {
    out <- command_produce_build(x, environment())$object
    expect_equal(out, exp1)
  }
  expect_false(exp0 == exp1)
})

tar_test("command$produce_build() does not change working dir", {
  dir <- getwd()
  x <- command_init(expr = quote(setwd(tempdir()))) # nolint
  command_produce_build(x, environment())
  expect_equal(getwd(), dir)
})

tar_test("command_init(deps)", {
  command <- command_init(quote(a <- b + c), deps = "custom")
  expect_equal(command$deps, "custom")
})

tar_test("command_init() with automatic deps", {
  command <- command_init(quote(a <- b + c))
  expect_true(all(c("b", "c") %in% command$deps))
  expect_false("a" %in% command$deps)
})

tar_test("command_init() inspects formulas", {
  command <- command_init(quote(map_dfr(data, ~do_row(.x, dataset))))
  expect_true(all(c("dataset", "do_row") %in% command$deps))
  expect_false("~" %in% command$deps)
})

tar_test("command_init(string)", {
  command <- command_init(quote(a <- b + c), string = "custom")
  expect_equal(command$string, "custom")
})

tar_test("command_validate() on a good expr", {
  command <- command_init(quote(a <- b + c))
  expect_silent(command_validate(command))
})

tar_test("command_validate() with an extra field", {
  command <- command_init(quote(a <- b + c))
  command$nope <- 123
  expect_error(command_validate(command), class = "tar_condition_validate")
})

tar_test("command_validate() with empty expr field", {
  command <- command_init()
  command$expr <- NULL
  expect_error(command_validate(command), class = "tar_condition_validate")
})

tar_test("command_validate() with bad packages field", {
  command <- command_init(expr = quote(a <- b + c), packages = 123)
  expect_error(command_validate(command), class = "tar_condition_validate")
})

tar_test("command validation with packages (test 2)", {
  command_good <- command_init(quote(a <- b + c))
  expect_silent(command_validate(command_good))
  command_bad <- command_init(quote(a <- b + c), packages = 123)
  expect_error(command_validate(command_bad), class = "tar_condition_validate")
  expect_error(command_validate_packages(command_bad))
})

tar_test("command_validate() with bad library field", {
  command <- command_init(expr = quote(a <- b + c), library = 123)
  expect_error(command_validate(command), class = "tar_condition_validate")
})

tar_test("command_validate() with bad deps field", {
  command <- command_init(expr = quote(a <- b + c))
  command$deps <- 123L
  expect_error(command_validate(command), class = "tar_condition_validate")
})

tar_test("command_validate() with bad string field", {
  command <- command_new(
    expr = quote(a <- b + c),
    packages = character(0),
    deps = character(0),
    seed = 0L
  )
  expect_error(command_validate(command), class = "tar_condition_validate")
})

tar_test("command_validate() with bad hash field", {
  command <- command_new(
    expr = quote(a <- b + c),
    packages = character(0),
    deps = character(0),
    string = "abcde",
    seed = 0L
  )
  expect_error(command_validate(command), class = "tar_condition_validate")
})

tar_test("command_validate() with a bad seed", {
  x <- command_init(expr = quote(a <- b + c))
  x$seed <- "123"
  expect_error(command_validate(x), class = "tar_condition_validate")
  x$seed <- integer(0)
  expect_error(command_validate(x), class = "tar_condition_validate")
})
