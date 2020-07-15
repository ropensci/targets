library(proffer)
library(targets)

px <- pprof(
  targets <- lapply(
    as.character(seq_len(1e3)), function(name) {
      target_init(name, expr = quote(1 + 1))
    }
  )
)
large_pipeline <- pipeline_init(targets)
px <- pprof(algorithm_init("local", pipeline = large_pipeline)$run())
tar_destroy()

# With API.
tar_script({
  x0 <- 1
  targets <- lapply(seq_len(1e3), function(id) {
    name <- paste0("x", as.character(id))
    dep <- paste0("x", as.character(id - 1L))
    expr <- as.expression(rlang::sym(dep))
    tar_target_external(name, expr = expr)
  })
  tar_pipeline(targets)
})
px <- pprof(tar_make(reporter = "summary", callr_function = NULL))
tar_destroy()
unlink("_targets.R")

# Same, but with a target chain.
tar_script({
  target_x0 <- tar_target(x0, stop())
  targets <- lapply(seq_len(1e3), function(id) {
    name <- paste0("x", as.character(id))
    dep <- paste0("x", as.character(id - 1L))
    expr <- as.expression(rlang::sym(dep))
    tar_target_external(name, expr = expr)
  })
  tar_pipeline(c(targets, target_x0))
})
system.time(try(tar_make(reporter = "summary", callr_function = NULL)))
px <- pprof(try(tar_make(reporter = "summary", callr_function = NULL)))
tar_destroy()
unlink("_targets.R")

# Same, but with setup overhead due to custom topo sort.
# Should see the overhead in the graph.
tar_script({
  target_x0 <- tar_target(x0, stop(), priority = 1)
  targets <- lapply(seq_len(1e3), function(id) {
    name <- paste0("x", as.character(id))
    dep <- paste0("x", as.character(id - 1L))
    expr <- as.expression(rlang::sym(dep))
    tar_target_external(name, expr = expr)
  })
  tar_pipeline(c(targets, target_x0))
})
system.time(try(tar_make(reporter = "summary", callr_function = NULL)))
px <- pprof(try(tar_make(reporter = "summary", callr_function = NULL)))

# Should not see topo sort overhead for tar_outdated().
system.time(try(tar_outdated(callr_function = NULL)))
px <- pprof(try(tar_outdated(callr_function = NULL)))

# Should not see topo sort overhead for tar_make_future().
system.time(try(tar_make_future(callr_function = NULL)))
px <- pprof(try(tar_make_future(callr_function = NULL)))

tar_destroy()
unlink("_targets.R")
