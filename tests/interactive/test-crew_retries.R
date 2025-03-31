test_that("crew retries", {
  tar_script({
    tar_option_set(controller = crew::crew_controller_local())
    tar_target(x, {
      Sys.sleep(15)
      "completed despite retries"
    })
  })
  # Run each of the following interactively.
  # Terminate the crew worker (open htop, search "crew::crew_worker")
  # and observe the retries.
  tar_destroy()
  tar_make(reporter = "silent")
  expect_equal(tar_read(x), "completed despite retries")
  tar_destroy()
  tar_make(reporter = "verbose")
  expect_equal(tar_read(x), "completed despite retries")
  tar_destroy()
  tar_make(reporter = "verbose_positives")
  expect_equal(tar_read(x), "completed despite retries")
  tar_destroy()
  tar_make(reporter = "timestamp")
  expect_equal(tar_read(x), "completed despite retries")
  tar_destroy()
  tar_make(reporter = "timestamp_positives")
  expect_equal(tar_read(x), "completed despite retries")
  tar_destroy()
  tar_make(reporter = "balanced")
  expect_equal(tar_read(x), "completed despite retries")
  tar_destroy()
  tar_make(reporter = "verbose") # Max out the retry quota on this last one.
  tar_destroy()
})
