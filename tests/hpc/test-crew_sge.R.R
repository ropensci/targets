tar_test("crew SGE with many tasks and many workers", {
  skip_on_cran()
  skip_if_not_installed("crew.cluster")
  tar_script({
    library(crew.cluster)
    library(targets)
    controller <- crew_controller_sge(
      workers = 25,
      tasks_max = 100,
      script_lines = "module load R/4.2.2"
    )
    tar_option_set(controller = controller)
    list(
      tar_target(x, seq_len(10000)),
      tar_target(y, Sys.sleep(1), pattern = map(x))
    )
  })
  tar_make()
  expect_equal(tar_outdated(reporter = "forecast"), character(0))
})
