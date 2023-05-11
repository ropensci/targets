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

tar_test("retry_until_true() with argless function", {
  expect_silent(retry_until_true(fun = function() TRUE))
  expect_error(
    retry_until_true(
      fun = function() FALSE,
      seconds_interval = 0.01,
      seconds_timeout = 0.05
    ),
    class = "tar_condition_expire"
  )
})

tar_test("retry_until_true() on a file", {
  skip_on_cran()
  tmp <- tempfile()
  fun <- function(x) file.exists(x)
  args <- list(x = tmp)
  expect_error(
    retry_until_true(
      fun = ~FALSE,
      args = args,
      seconds_interval = 0.01,
      seconds_timeout = 0.05
    ),
    class = "tar_condition_expire"
  )
  file.create(tmp)
  on.exit(unlink(tmp))
  expect_silent(retry_until_true(fun = fun, args = args))
})

tar_test("retry_until_true(catch_error = TRUE)", {
  skip_on_cran()
  tmp <- tempfile()
  fun <- function(x) file.exists(x)
  args <- list(x = tmp)
  expect_error(
    retry_until_true(
      fun = ~rlang::abort(message = "error", class = "ad_hoc"),
      args = args,
      seconds_interval = 0.01,
      seconds_timeout = 0.05,
      catch_error = TRUE
    ),
    class = "tar_condition_expire"
  )
})

tar_test("retry_until_true(catch_error = FALSE)", {
  skip_on_cran()
  tmp <- tempfile()
  fun <- function(x) file.exists(x)
  args <- list(x = tmp)
  expect_error(
    retry_until_true(
      fun = ~rlang::abort(message = "error", class = "ad_hoc"),
      args = args,
      seconds_interval = 0.01,
      seconds_timeout = 0.05,
      catch_error = FALSE
    ),
    class = "ad_hoc"
  )
})

tar_test("retry_until_true() max_tries", {
  skip_on_cran()
  envir <- new.env(parent = emptyenv())
  envir$count <- 0L
  expect_error(
    retry_until_true(
      fun = ~{
        envir$count <- envir$count + 1L
        FALSE
      },
      seconds_interval = 0.001,
      seconds_timeout = 60,
      max_tries = 5L
    ),
    class = "tar_condition_expire"
  )
  expect_equal(envir$count, 5L)
})
