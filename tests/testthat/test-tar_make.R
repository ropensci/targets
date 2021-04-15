tar_test("empty tar_make() works even with names", {
  tar_script(list())
  expect_silent(
    tar_make(
      names = x,
      reporter = "silent",
      callr_function = NULL
    )
  )
})

tar_test("tar_make() works", {
  tar_script(
    list(
      tar_target(y1, 1L + 1L),
      tar_target(y2, 1L + 1L),
      tar_target(z, y1 + y2)
    )
  )
  tar_make(
    reporter = "silent",
    callr_arguments = list(show = FALSE)
  )
  out <- tar_read(z)
  expect_equal(out, 4L)
})

tar_test("tar_make() deduplicates metadata", {
  tar_script({
    tar_option_set(envir = new.env(parent = baseenv()))
    list(tar_target(x, 1L, cue = tar_cue(mode = "always")))
  })
  for (index in seq_len(3L)) {
    tar_make(callr_function = NULL)
  }
  out <- meta_init()$database$read_data()
  expect_equal(nrow(out), 1L)
})

tar_test("tar_make() can use tidyselect", {
  tar_script(
    list(
      tar_target(y1, 1 + 1),
      tar_target(y2, 1 + 1),
      tar_target(z, y1 + y2)
    )
  )
  tar_make(
    names = starts_with("y"),
    reporter = "silent",
    callr_arguments = list(show = FALSE)
  )
  out <- sort(list.files(file.path("_targets", "objects")))
  expect_equal(out, sort(c("y1", "y2")))
})

tar_test("tar_make() finds the correct environment", {
  tar_script({
    f <- function(x) {
      g(x) + 1L
    }
    g <- function(x) {
      x + 1L
    }
    a <- 1L
    list(tar_target(y, f(!!a), tidy_eval = TRUE))
  })
  tar_make(
    reporter = "silent",
    callr_arguments = list(show = FALSE)
  )
  out <- tar_read(y)
  expect_equal(out, 3L)
})

tar_test("tar_make() handles callr errors", {
  skip_on_cran()
  withr::local_envvar(list(TAR_TEST = "false")) # covers some lines
  tar_script({
    list(
      tar_target(x, "x"),
      tar_target(y, stop(x))
    )
  })
  # TODO: when https://github.com/r-lib/callr/issues/196 and
  # https://github.com/r-lib/callr/issues/197 are fixed,
  # go back to expecting forwarded errors of class "tar_condition_validate".
  try(
    tar_make(reporter = "silent", callr_arguments = list(show = FALSE)),
    silent = TRUE
  )
  expect_null(NULL)
})
