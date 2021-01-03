library(targets)
tar_script({
  sleep_run <- function(...) Sys.sleep(10)
  list(
    tar_target(data1, sleep_run()),
    tar_target(data2, sleep_run()),
    tar_target(model1, sleep_run(data1)),
    tar_target(model2, sleep_run(data2)),
    tar_target(conclusions, sleep_run(c(model1, model2)))
  )
})
# Should just launch the app in a blocking process:
tar_watch(background = FALSE)
# Should not block the main process:
tar_watch(
  seconds = 10,
  outdated = FALSE,
  targets_only = TRUE,
  height = "450px"
)
# The main process should be free to run the pipeline.
tar_make()
# Restarting the session should terminate the app.
rstudioapi::restartSession()
tar_destroy()
unlink("_targets.R")
