library(targets)
tar_script({
  sleep_run <- function(...) {
    Sys.sleep(10)
  }
  tar_pipeline(
    tar_target(settings, sleep_run()),
    tar_target(data1, sleep_run(settings)),
    tar_target(data2, sleep_run(settings)),
    tar_target(data3, sleep_run(settings)),
    tar_target(model1, sleep_run(data1)),
    tar_target(model2, sleep_run(data2)),
    tar_target(model3, sleep_run(data3)),
    tar_target(figure1, sleep_run(model1)),
    tar_target(figure2, sleep_run(model2)),
    tar_target(figure3, sleep_run(model3)),
    tar_target(conclusions, sleep_run(c(figure1, figure2, figure3)))
  )
})
px <- tar_make(callr_function = callr::r_bg)
tar_watch(seconds = 10, outdated = FALSE, targets_only = TRUE)
tar_destroy()
unlink("_targets.R")
