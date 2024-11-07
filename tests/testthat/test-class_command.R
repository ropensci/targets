tar_test("command$update_string()", {
  command <- command_init(quote(a <- b + c))
  expect_equal(command$string, "expression(a <- b + c)")
})

tar_test("command$hash", {
  command <- command_init(quote(a <- b + c))
  out <- command$hash
  expect_true(is.character(out))
  expect_length(out, 1L)
  expect_equal(nchar(out), 16L)
})

tar_test("command_produce_build()", {
  command <- command_init(expr = quote(a <- b + c))
  b <- 1L
  c <- 2L
  envir <- environment()
  build <- command_produce_build(command, 1L, envir)
  expect_silent(build_validate(build))
  expect_equal(build$object, 3L)
  expect_true(is.numeric(build$metrics$seconds))
})

tar_test("command$produce_build() uses seed", {
  x <- command_init(expr = quote(sample.int(1e9, 1L)))
  seed <- 0L
  sample_with_seed <- function(seed) {
    tar_seed_set(seed)
    sample.int(1e9, 1L)
  }
  exp0 <- sample_with_seed(0L)
  for (i in seq_len(2)) {
    out <- command_produce_build(x, seed, environment())$object
    expect_equal(out, exp0)
  }
  seed <- 1L
  exp1 <- sample_with_seed(1L)
  for (i in seq_len(2)) {
    out <- command_produce_build(x, seed, environment())$object
    expect_equal(out, exp1)
  }
  expect_false(exp0 == exp1)
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

tar_test("command_validate() with bad string field", {
  command <- command_new(
    expr = quote(a <- b + c),
    packages = character(0)
  )
  expect_error(command_validate(command), class = "tar_condition_validate")
})

tar_test("command_validate() with bad hash field", {
  command <- command_new(
    expr = quote(a <- b + c),
    packages = character(0),
    string = "abcde"
  )
  expect_error(command_validate(command), class = "tar_condition_validate")
})
