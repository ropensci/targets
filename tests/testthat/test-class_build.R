tar_test("keep track of seconds", {
  build <- build_init(quote(1L + 1L), baseenv())
  out <- build$metrics$seconds
  expect_true(is.numeric(out))
  expect_equal(length(out), 1L)
})

tar_test("run without error", {
  build <- build_init(quote(1L + 1L), baseenv())
  expect_equal(build$object, 2L)
  expect_null(build$metrics$error)
  expect_null(build$metrics$traceback)
})

tar_test("run with error", {
  build <- build_init(quote(stop(12345)), baseenv())
  expect_null(build$object)
  expect_true(any(grepl("12345", build$metrics$error)))
  expect_true(any(grepl("12345", build$metrics$traceback)))
})

tar_test("error with no message", {
  build <- build_init(quote(stop()), baseenv())
  expect_null(build$object)
  expect_equal(build$metrics$error, ".")
  expect_true(is.character(build$metrics$traceback))
  expect_true(length(build$metrics$traceback) > 0L)
  expect_true(any(nzchar(build$metrics$traceback)))
})

tar_test("run with warning, warning recorded", {
  skip_cran()
  build <- suppressWarnings(
    build_init(quote(warning("12345")), baseenv())
  )
  expect_true(any(grepl("12345", build$metrics$warnings)))
})

tar_test("run with warning, warning thrown", {
  skip_cran()
  expect_warning(
    build_init(quote(warning("12345")), baseenv()),
    regexp = "12345"
  )
})

tar_test("warning with no message, warning recorded", {
  skip_cran()
  build <- suppressWarnings(build_init(quote(warning()), baseenv()))
  expect_equal(build$metrics$warnings, ".")
})

tar_test("warning with no message, warning thrown", {
  skip_cran()
  build <- expect_warning(build_init(quote(warning()), baseenv()))
})

tar_test("build seeds", {
  expr <- quote(sample.int(n = 1e9, size = 1L))
  env <- baseenv()
  sample.int(n = 1)
  old <- .Random.seed
  on.exit(.Random.seed <- old)
  set.seed(1L)
  out <- integer(0)
  out[1] <- build_init(expr, env, 1L)$object
  set.seed(2L)
  out[2] <- build_init(expr, env, 1L)$object
  out[3] <- build_init(expr, env, 1L)$object
  out[4] <- build_init(expr, env, 2L)$object
  set.seed(3L)
  out[5] <- build_init(expr, env, NA)$object
  out[6] <- build_init(expr, env, NA)$object
  set.seed(3L)
  out[7] <- build_init(expr, env, NA)$object
  set.seed(4L)
  out[8] <- build_init(expr, env, NA)$object
  expect_equal(out[1], out[2])
  expect_equal(out[2], out[3])
  expect_equal(out[5], out[7])
  expect_false(out[3] == out[4])
  for (i in c(5, 6, 7, 8)) {
    for (j in setdiff(seq_len(8), c(5L, 7L))) {
      if (i != j) {
        expect_false(out[i] == out[j])
      }
    }
  }
})

tar_test("warning character limit", {
  skip_cran()
  tar_script(
    tar_target(
      a,
      for (i in 1:1e3) warning(paste(rep("a", 65), collapse = ""))
    )
  )
  suppressWarnings(tar_make(callr_function = NULL))
  out <- tar_meta(a, warnings)$warnings
  expect_equal(nchar(out), build_message_max_nchar)
})

tar_test("validate good builds", {
  build <- build_init(quote(1L + 1L), baseenv())
  expect_silent(build_validate(build))
})

tar_test("validate builds with bad metrics", {
  build <- build_init(quote(1L + 1L), baseenv())
  build$metrics$seconds <- NULL
  expect_error(build_validate(build), class = "tar_condition_validate")
})
