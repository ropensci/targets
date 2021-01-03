library(proffer)
library(targets)

px <- pprof(
  targets <- lapply(
    as.character(seq_len(1e3)), function(name) {
      tar_target_raw(name, command = quote(1 + 1))
    }
  )
)
large_pipeline <- pipeline_init(targets)
px <- pprof(local_init(pipeline = large_pipeline)$run())
tar_destroy()

# With API.
tar_script({
  x0 <- 1
  lapply(seq_len(1e3), function(id) {
    name <- paste0("x", as.character(id))
    dep <- paste0("x", as.character(id - 1L))
    command <- as.expression(rlang::sym(dep))
    tar_target_raw(name, command = command)
  })
})
px <- pprof(tar_make(reporter = "summary", callr_function = NULL))
tar_destroy()
unlink("_targets.R")

# Same, but with a target chain.
tar_script({
  target_x0 <- tar_target(x0, stop())
  lapply(seq_len(1e3), function(id) {
    name <- paste0("x", as.character(id))
    dep <- paste0("x", as.character(id - 1L))
    command <- as.expression(rlang::sym(dep))
    tar_target_raw(name, command = command)
  })
})
system.time(try(tar_make(reporter = "summary", callr_function = NULL)))
px <- pprof(try(tar_make(reporter = "summary", callr_function = NULL)))
tar_destroy()
unlink("_targets.R")

# Same, but with setup overhead due to custom topo sort.
# Should see the overhead in the graph.
tar_script({
  target_x0 <- tar_target(x0, stop(), priority = 1)
  lapply(seq_len(1e3), function(id) {
    name <- paste0("x", as.character(id))
    dep <- paste0("x", as.character(id - 1L))
    command <- as.expression(rlang::sym(dep))
    tar_target_raw(name, command = command)
  })
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
