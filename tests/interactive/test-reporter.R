# Should be silent.
tar_destroy()
for (index in seq_len(2L)) {
  algorithm_init("local", pipeline_map(), reporter = "silent")$run()
}

# Should be silent except for the error message.
tar_destroy()
pipeline <- pipeline_init(list(target_init("x", quote(stop(123)))))
local <- algorithm_init("local", pipeline, reporter = "silent")$run()

# Should be silent.
tar_destroy()
for (index in seq_len(2L)) {
  x <- target_init("x", quote(targets::tar_cancel()))
  pipeline <- pipeline_init(list(x))
  local <- algorithm_init("local", pipeline, reporter = "silent")$run()
}

# Should show one build message per target.
tar_destroy()
algorithm_init("local", pipeline_map(), reporter = "verbose")$run()

# Should show one skip message per target.
algorithm_init("local", pipeline_map(), reporter = "verbose")$run()

# Should show a failure message and an error message.
tar_destroy()
pipeline <- pipeline_init(list(target_init("x", quote(stop(123)))))
local <- algorithm_init("local", pipeline, reporter = "verbose")$run()

# Should show a regular warning and a meta-warning with a tip about tar_meta().
tar_destroy()
pipeline <- pipeline_init(list(target_init("x", quote(warning(123)))))
local <- algorithm_init("local", pipeline, reporter = "verbose")$run()

# Should show a cancellation message.
tar_destroy()
pipeline <- pipeline_init(list(target_init("x", quote(targets::tar_cancel()))))
local <- algorithm_init("local", pipeline, reporter = "verbose")$run()

# Should show one timestamp-stamped message per target.
tar_destroy()
algorithm_init("local", pipeline_map(), reporter = "timestamp")$run()

# Should show one time-stamped skip message per target.
algorithm_init("local", pipeline_map(), reporter = "timestamp")$run()

# Should show a timestamp-stamped failure message and an error message.
tar_destroy()
pipeline <- pipeline_init(list(target_init("x", quote(stop(123)))))
local <- algorithm_init("local", pipeline, reporter = "timestamp")$run()

# Should show a timestamped warning and a meta-warning w/ tip about tar_meta().
tar_destroy()
pipeline <- pipeline_init(list(target_init("x", quote(warning(123)))))
local <- algorithm_init("local", pipeline, reporter = "timestamp")$run()

# Should show a timestamp-stamped cancellation message.
tar_destroy()
pipeline <- pipeline_init(list(target_init("x", quote(targets::tar_cancel()))))
local <- algorithm_init("local", pipeline, reporter = "timestamp")$run()

# Should show counts per target status category
# and show all built at the end.
tar_destroy()
algorithm_init("local", pipeline_map(), reporter = "summary")$run()

# Should a running target for 2 seconds.
tar_destroy()
pipeline <- pipeline_init(list(target_init("x", quote(Sys.sleep(2)))))
local <- algorithm_init("local", pipeline, reporter = "summary")$run()

# Should show one errored target.
tar_destroy()
pipeline <- pipeline_init(list(target_init("x", quote(stop(123)))))
local <- algorithm_init("local", pipeline, reporter = "summary")$run()

# Should show one cancelled target.
tar_destroy()
pipeline <- pipeline_init(list(target_init("x", quote(targets::tar_cancel()))))
local <- algorithm_init("local", pipeline, reporter = "summary")$run()

# Should show same number check as outdated.
tar_destroy()
out <- outdated_init(pipeline_map(), reporter = "forecast")
tmp <- out$run()

# Should show more checked and no outdated.
local <- algorithm_init("local", pipeline_map())
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
  tar_pipeline(
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
