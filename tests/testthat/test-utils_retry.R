tar_test("retry_until_true() with argless function", {
  skip_on_cran()
  expect_silent(retry_until_true(fun = function() TRUE))
  expect_error(
    retry_until_true(
      fun = function() FALSE,
      seconds_interval = 0.1,
      seconds_timeout = 0.05,
      verbose = TRUE
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

tar_test("retry_until_success() success", {
  skip_on_cran()
  out <- retry_until_success(
    fun = function() {
      "done"
    }
  )
  expect_equal(out, "done")
})

tar_test("retry_until_success() uncaught class", {
  skip_on_cran()
  tmp <- tempfile()
  fun <- function(x) file.exists(x)
  args <- list(x = tmp)
  envir <- new.env(parent = emptyenv())
  envir$count <- 0L
  expect_error(
    retry_until_success(
      fun = ~{
        envir$count <- envir$count + 1L
        rlang::abort(message = "error", class = "ad_hoc")
      },
      args = args,
      seconds_interval = 0.001,
      seconds_timeout = 60,
      max_tries = 5L
    ),
    class = "tar_condition_expire"
  )
  expect_equal(envir$count, 1L)
})

tar_test("retry_until_success() caught class", {
  skip_on_cran()
  tmp <- tempfile()
  fun <- function(x) file.exists(x)
  args <- list(x = tmp)
  envir <- new.env(parent = emptyenv())
  envir$count <- 0L
  expect_error(
    retry_until_success(
      fun = ~{
        envir$count <- envir$count + 1L
        class <- ifelse(envir$count %% 2L, "class1", "class2")
        rlang::abort(message = "error", class = class)
      },
      args = args,
      seconds_interval = 0.001,
      seconds_timeout = 60,
      max_tries = 5L,
      classes_retry = c("class1", "class2")
    ),
    class = "tar_condition_expire"
  )
  expect_equal(envir$count, 5L)
})

tar_test("retry_until_success() caught class", {
  skip_on_cran()
  tmp <- tempfile()
  fun <- function(x) file.exists(x)
  args <- list(x = tmp)
  envir <- new.env(parent = emptyenv())
  envir$count <- 0L
  out <- retry_until_success(
    fun = ~{
      envir$count <- envir$count + 1L
      class <- ifelse(envir$count %% 2L, "class1", "class2")
      if (envir$count < 2L) {
        rlang::abort(message = "error", class = class)
      }
      "returned"
    },
    args = args,
    seconds_interval = 0.001,
    seconds_timeout = 60,
    max_tries = 5L,
    classes_retry = c("class1", "class2")
  )
  expect_equal(envir$count, 2L)
  expect_equal(out, "returned")
})
