tar_test("%||%", {
  expect_equal("x" %||% "y", "x")
  expect_equal(character(0) %||% "y", "y")
  expect_equal(NA_character_ %||% "y", NA_character_)
  expect_equal(NULL %||% "y", "y")
})

tar_test("%|||%", {
  expect_equal("x" %|||% "y", "x")
  expect_equal(character(0) %|||% "y", character(0))
  expect_equal(NA_character_ %|||% "y", NA_character_)
  expect_equal(NULL %|||% "y", "y")
})

tar_test("%||NA%", {
  expect_equal("x" %||NA% "y", "x")
  expect_equal(character(0) %||NA% "y", character(0))
  expect_equal(NA_character_ %||NA% "y", "y")
  expect_equal(NULL %||NA% "y", NULL)
})

tar_test("%||nf%", {
  expect_equal("x" %||nf% "y", "x")
  expect_equal(character(0) %||nf% "y", "y")
  expect_equal(NA_character_ %||nf% "y", "y")
  expect_equal(NULL %||nf% "y", "y")
})

tar_test("if_any()", {
  expect_equal(if_any(TRUE, "x", "y"), "x")
  expect_equal(if_any(FALSE, "x", "y"), "y")
})

tar_test("retry() with argless function", {
  expect_silent(retry(fun = function() TRUE))
  expect_error(
    retry(
      fun = function() FALSE,
      seconds_interval = 0.01,
      seconds_timeout = 0.05
    ),
    class = "tar_condition_expire"
  )
})

tar_test("retry() on a file", {
  skip_on_cran()
  tmp <- tempfile()
  fun <- function(x) file.exists(x)
  args <- list(x = tmp)
  expect_error(
    retry(
      fun = ~FALSE,
      args = args,
      seconds_interval = 0.01,
      seconds_timeout = 0.05
    ),
    class = "tar_condition_expire"
  )
  file.create(tmp)
  on.exit(unlink(tmp))
  expect_silent(retry(fun = fun, args = args))
})

tar_test("retry(catch_error = TRUE)", {
  skip_on_cran()
  tmp <- tempfile()
  fun <- function(x) file.exists(x)
  args <- list(x = tmp)
  expect_error(
    retry(
      fun = ~rlang::abort(message = "error", class = "ad_hoc"),
      args = args,
      seconds_interval = 0.01,
      seconds_timeout = 0.05,
      catch_error = TRUE
    ),
    class = "tar_condition_expire"
  )
})

tar_test("retry(catch_error = FALSE)", {
  skip_on_cran()
  tmp <- tempfile()
  fun <- function(x) file.exists(x)
  args <- list(x = tmp)
  expect_error(
    retry(
      fun = ~rlang::abort(message = "error", class = "ad_hoc"),
      args = args,
      seconds_interval = 0.01,
      seconds_timeout = 0.05,
      catch_error = FALSE
    ),
    class = "ad_hoc"
  )
})
