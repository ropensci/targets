library(proffer)
library(targets)

px <- pprof(
  targets <- lapply(
    paste0("x", seq_len(1e3)), function(name) {
      tar_target_raw(name, command = quote(1 + 1))
    }
  )
)
large_pipeline <- pipeline_init(targets)
px <- pprof(local_init(pipeline = large_pipeline)$run())
tar_destroy()

# With interface and priorities
tar_script({
  x0 <- 1
  targets <- lapply(seq_len(1e3), function(id) {
    name <- paste0("x", as.character(id))
    dep <- paste0("x", as.character(id - 1L))
    command <- as.expression(rlang::sym(dep))
    tar_target_raw(name, command = command)
  })
  list(targets, tar_target(y, 1, priority = 0.37))
})
px <- pprof(tar_make(reporter = "summary", callr_function = NULL))
tar_destroy()
unlink("_targets.R")
rm(x0)

# Same, but with a target chain that fails early.
# Should not see overhead due to topo_sort_by_priority()
# since all priorities are equal.
tar_script({
  target_x0 <- tar_target(x0, stop())
  out <- lapply(seq_len(1e3), function(id) {
    name <- paste0("x", as.character(id))
    dep <- paste0("x", as.character(id - 1L))
    command <- as.expression(rlang::sym(dep))
    tar_target_raw(name, command = command)
  })
  list(target_x0, out)
})
system.time(try(tar_make(reporter = "summary", callr_function = NULL)))
px <- pprof(try(tar_make(reporter = "summary", callr_function = NULL)))
tar_destroy()
unlink("_targets.R")

# Same, but with unequal priorities.
tar_script({
  target_x0 <- tar_target(x0, stop(), priority = 1)
  out <- lapply(seq_len(1e3), function(id) {
    name <- paste0("x", as.character(id))
    dep <- paste0("x", as.character(id - 1L))
    command <- as.expression(rlang::sym(dep))
    tar_target_raw(name, command = command, priority = 0)
  })
  list(target_x0, out)
})
system.time(try(tar_make(reporter = "summary", callr_function = NULL)))
px <- pprof(try(tar_make(reporter = "summary", callr_function = NULL)))

# Should not see topo sort overhead for tar_outdated().
system.time(try(tar_outdated(callr_function = NULL)))
px <- pprof(try(tar_outdated(callr_function = NULL)))

# Should not see topo sort overhead for tar_sitrep().
system.time(try(tar_sitrep(callr_function = NULL)))
px <- pprof(try(tar_sitrep(callr_function = NULL)))

# Should not see topo sort overhead for tar_make_future().
system.time(try(tar_make_future(callr_function = NULL)))
px <- pprof(try(tar_make_future(callr_function = NULL)))

tar_destroy()
unlink("_targets.R")
