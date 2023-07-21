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
# Should exclude data1 in the graph.
tar_watch(background = FALSE, exclude = "data1")
# Should exclude sleep_run() and data2 in the graph.
px <- tar_watch(background = TRUE, exclude = c("sleep_run", "data2"))
px$kill()
# Should not block the main process:
tar_watch(
  seconds = 10,
  outdated = FALSE,
  targets_only = TRUE,
  height = "450px"
)
# The main process should be free to run the pipeline.
tar_make()
# Run it again and see skipped targets.
tar_make()
# Restarting the session should terminate the app.
rstudioapi::restartSession()
tar_destroy()
unlink("_targets.R")

# View the progress tables. Should see some of each status except started.
library(targets)
tar_script({
  list(
    tar_target(x, seq_len(2)),
    tar_target(y, stop("error message"), pattern = map(x), error = "continue"),
    tar_target(z, tar_cancel(), pattern = map(x)),
    tar_target(w, x, pattern = map(x)),
    # nolint start
    tar_target(
      forever, {
      Sys.sleep(Inf)
      c(x, y, z, w)
    })
    # nolint end
  )
})
tar_make() # Stop early.
tar_watch(background = FALSE)

# Should see started branches in the table.
library(targets)
tar_destroy()
tar_script({
  tar_option_set(error = "continue")
  sleep_run <- function(...) Sys.sleep(10)
  list(
    tar_target(batch, seq_len(4)),
    tar_target(data1, sleep_run(batch), pattern = map(batch))
  )
})
# Select the "branches" box:
tar_watch(
  seconds = 10,
  outdated = FALSE,
  targets_only = TRUE,
  height = "450px"
)
# The main process should be free to run the pipeline.
tar_make()
# Run it again to see skipped branches.
tar_make()
# Look at the canceled branches in the "branches" view.
tar_script({
  list(
    tar_target(batch, seq_len(4)),
    tar_target(data1, tar_cancel(), pattern = map(batch))
  )
})
tar_make()

# Restarting the session should terminate the app.
rstudioapi::restartSession()
tar_destroy()
unlink("_targets.R")

# Launch using different config settings.
tar_script({
  sleep_run <- function(...) Sys.sleep(0.5)
  list(
    tar_target(x, seq_len(2)),
    tar_target(y, x, pattern = map(x)),
    tar_target(data1, sleep_run()),
    tar_target(data2, sleep_run()),
    tar_target(model1, sleep_run(data1)),
    tar_target(model2, sleep_run(data2)),
    tar_target(conclusions, sleep_run(c(model1, model2)))
  )
}, script = "example/script.R")
tar_config_set(
  script = "example/script.R",
  store = "example/store",
  config = "example/config.yaml"
)
px <- tar_watch(
  seconds = 5,
  targets_only = TRUE,
  height = "450px",
  config = "example/config.yaml"
)
# Should show pipeline progress in app.
tar_make(
  script = "example/script.R",
  store = "example/store"
)
# Files should go in the correct places.
library(testthat)
expect_false(file.exists("_targets"))
expect_false(file.exists("_targets.R"))
expect_true(file.exists("example/config.yaml"))
expect_true(file.exists("example/script.R"))
expect_true(file.exists("example/store"))
# Switching the config affects the app.
tar_config_set(
  script = tempfile(),
  store = tempfile(),
  config = "example/config.yaml"
)
px$kill()
# No targets file should be detected.
px <- tar_watch(
  seconds = 5,
  targets_only = TRUE,
  height = "450px"
)
px$kill()
unlink("example", recursive = TRUE)
