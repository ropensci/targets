tar_test("tar_assert_store_noninvalidating()", {
  rstudioapi::restartSession()
  # remotes::install_version("targets", version = "1.6.0") # nolint
  rstudioapi::restartSession()
  targets::tar_script(tar_target(x, 1))
  targets::tar_make()
  rstudioapi::restartSession()
  # pkgload::load_all() # nolint
  expect_equal(tar_outdated(callr_function = NULL), "x")
  expect_message(
    tar_outdated(callr_function = NULL),
    class = "tar_condition_run"
  )
  tar_make() # Select 1. Should stop pipeline.
  tar_make(callr_function = NULL) # Select 2. Should rerun pipeline.
  expect_equal(tar_outdated(callr_function = NULL), character(0L))
  expect_silent(tar_outdated(callr_function = NULL))
  expect_silent(tar_make(callr_function = NULL, reporter = "silent"))
})
