# Should be silent.
tar_destroy()
for (index in seq_len(2L)) {
  local_init(pipeline_map(), reporter = "silent")$run()
}

# Should be silent except for the error message.
tar_destroy()
pipeline <- pipeline_init(list(target_init("x", quote(stop(123)))))
local <- local_init(pipeline, reporter = "silent")$run()

# Should be silent except for the error message.
tar_destroy()
pipeline <- pipeline_init(
  list(target_init("x", quote(stop(123)), error = "workspace"))
)
local <- local_init(pipeline, reporter = "silent")$run()

# Should be silent.
tar_destroy()
for (index in seq_len(2L)) {
  x <- target_init("x", quote(targets::tar_cancel()))
  pipeline <- pipeline_init(list(x))
  local <- local_init(pipeline, reporter = "silent")$run()
}

# Should show one build message per target.
tar_destroy()
local_init(pipeline_map(), reporter = "verbose")$run()

# Should show one skip message per target.
local_init(pipeline_map(), reporter = "verbose")$run()

# Should show a failure message and an error message.
tar_destroy()
pipeline <- pipeline_init(list(target_init("x", quote(stop(123)))))
local <- local_init(pipeline, reporter = "verbose")$run()

# Should show a regular warning and a meta-warning with a tip about tar_meta().
tar_destroy()
pipeline <- pipeline_init(list(target_init("x", quote(warning(123)))))
local <- local_init(pipeline, reporter = "verbose")$run()

# Should show error message but not save workspace.
tar_destroy()
pipeline <- pipeline_init(list(target_init("x", quote(stop(123)))))
local <- local_init(pipeline, reporter = "verbose")$run()

# Should show error message and save workspace.
tar_destroy()
pipeline <- pipeline_init(
  list(target_init("x", quote(stop(123)), error = "workspace"))
)
local <- local_init(pipeline, reporter = "verbose")$run()

# Warnings are relayed immediately if the warn option is 1.
tar_destroy()
pipeline <- pipeline_init(
  list(
    target_init("x", quote({
      warning("abc")
      Sys.sleep(2)
    })),
    target_init("y", quote(x))
  )
)
options(warn = 1)
local <- local_init(pipeline, reporter = "verbose")$run()

# Warnings are delayed if the warn option is 0.
tar_destroy()
pipeline <- pipeline_init(
  list(
    target_init("x", quote({
      warning("abc")
      Sys.sleep(2)
    })),
    target_init("y", quote(x))
  )
)
options(warn = 0)
local <- local_init(pipeline, reporter = "verbose")$run()

# Should show a cancellation message.
tar_destroy()
pipeline <- pipeline_init(list(target_init("x", quote(targets::tar_cancel()))))
local <- local_init(pipeline, reporter = "verbose")$run()

# Should show one timestamped message per target.
tar_destroy()
local_init(pipeline_map(), reporter = "timestamp")$run()

# Should show one timestamped skip message per target.
local_init(pipeline_map(), reporter = "timestamp")$run()

# Should show a timestamped failure message and an error message.
tar_destroy()
pipeline <- pipeline_init(list(target_init("x", quote(stop(123)))))
local <- local_init(pipeline, reporter = "timestamp")$run()

# Should show a timestamped failure message and an error message
# and save workspace.
tar_destroy()
pipeline <- pipeline_init(
  list(target_init("x", quote(stop(123)), error = "workspace"))
)
local <- local_init(pipeline, reporter = "timestamp")$run()

# Should show a timestamped message, a warning,
# and a meta-warning w/ tip about tar_meta().
tar_destroy()
pipeline <- pipeline_init(list(target_init("x", quote(warning(123)))))
local <- local_init(pipeline, reporter = "timestamp")$run()

# Should show a timestamped cancellation message.
tar_destroy()
pipeline <- pipeline_init(list(target_init("x", quote(targets::tar_cancel()))))
local <- local_init(pipeline, reporter = "timestamp")$run()

# Should show counts per target status category
# and show all built at the end.
tar_destroy()
local_init(pipeline_map(), reporter = "summary")$run()

# Should a running target for 2 seconds.
tar_destroy()
pipeline <- pipeline_init(list(target_init("x", quote(Sys.sleep(2)))))
local <- local_init(pipeline, reporter = "summary")$run()

# Should show one errored target.
tar_destroy()
pipeline <- pipeline_init(list(target_init("x", quote(stop(123)))))
local <- local_init(pipeline, reporter = "summary")$run()

# Should show one warned target.
tar_destroy()
pipeline <- pipeline_init(list(target_init("x", quote(warning(123)))))
local <- local_init(pipeline, reporter = "summary")$run()

# Should show one cancelled target.
tar_destroy()
pipeline <- pipeline_init(list(target_init("x", quote(targets::tar_cancel()))))
local <- local_init(pipeline, reporter = "summary")$run()

# Should show same number check as outdated.
tar_destroy()
out <- outdated_init(pipeline_map(), reporter = "forecast")
tmp <- out$run()

# Should show more checked and no outdated.
local <- local_init(pipeline_map())
local$run()
out <- outdated_init(pipeline_map(), reporter = "forecast")
tmp <- out$run()

# Should show more checked than outdated.
data1 <- readRDS("_targets/objects/data1")
data1 <- data1 + max(data1) + 10
saveRDS(data1, "_targets/objects/data1")
out <- outdated_init(pipeline_map(), reporter = "forecast")
tmp <- out$run()

# tar_outdated() uses forecast reporter.
tar_script(
  list(
    tar_target(y1, 1 + 1),
    tar_target(y2, 1 + 1),
    tar_target(z, y1 + y2)
  )
)
tar_outdated(reporter = "forecast")

# tar_visnetwork() uses forecast reporter.
tar_visnetwork(reporter = "forecast")

tar_destroy()
unlink("_targets.R")
