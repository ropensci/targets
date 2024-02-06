test_that("crew SGE with many tasks and many workers", {
  skip_on_cran()
  skip_if_not_installed("crew.cluster")
  tar_script({
    library(targets)
    controller <- crew.cluster::crew_controller_sge(
      workers = 25,
      tasks_max = 100,
      script_lines = paste0("module load R/", getRversion())
    )
    tar_option_set(controller = crew::crew_controller_group(controller))
    list(
      tar_target(x, seq_len(10000)),
      tar_target(y, Sys.sleep(0.1), pattern = map(x))
    )
  })
  tar_make()
  expect_equal(tar_outdated(reporter = "forecast"), character(0))
})
