tar_test("scheduler$graph", {
  s <- scheduler_init()
  expect_silent(s$graph$validate())
})

tar_test("scheduler$queue", {
  s <- scheduler_init()
  expect_silent(s$queue$validate())
})

tar_test("parallel queue has correct names", {
  pipeline <- pipeline_order()
  s <- scheduler_init(pipeline, queue = "parallel")
  out <- c(names(s$queue$data), s$queue$ready$data)
  exp <- pipeline_get_names(pipeline)
  expect_equal(sort(out), sort(exp))
})

tar_test("sequential queue has correct names", {
  pipeline <- pipeline_order()
  s <- scheduler_init(pipeline, queue = "sequential")
  out <- s$queue$data
  exp <- pipeline_get_names(pipeline)
  expect_equal(sort(out), sort(exp))
})

tar_test("parallel queue has correct ranks", {
  queue <- scheduler_init(pipeline_order(), queue = "parallel")$queue
  expect_equal(sort(queue$ready$data), sort(c("data1", "data2")))
  out <- as.list(queue$data)
  exp <- list(
    min1 = 1,
    min2 = 1,
    max1 = 1,
    max2 = 1,
    mins = 2,
    maxes = 2,
    all = 2
  )
  expect_equal(sort(names(out)), sort(names(exp)))
  expect_equal(out[names(out)], exp[names(out)])
})

tar_test("scheduler$count_unfinished_deps()", {
  pipeline <- pipeline_order()
  target <- pipeline_get_target(pipeline, "mins")
  local <- local_init(pipeline)
  on.exit(local$meta$database$close())
  on.exit(local$scheduler$progress$database$close(), add = TRUE)
  pipeline_prune_names(local$pipeline, local$names)
  local$update_scheduler()
  scheduler <- local$scheduler
  expect_equal(scheduler$count_unfinished_deps("mins"), 2L)
  local$ensure_meta()
  local$set_file_info()
  local$process_target("data1")
  local$process_target("data2")
  expect_equal(scheduler$count_unfinished_deps("mins"), 2L)
  local$process_target("min1")
  expect_equal(scheduler$count_unfinished_deps("mins"), 1L)
  local$process_target("min2")
  expect_equal(scheduler$count_unfinished_deps("mins"), 0L)
  local$process_target("mins")
  local$process_target("max1")
  local$process_target("max2")
  local$process_target("maxes")
  local$process_target("all")
  expect_equal(scheduler$count_unfinished_deps("mins"), 0L)
})

tar_test("initial_ranks()", {
  pipeline <- pipeline_init(
    list(
      target_init("x", quote(1), priority = 0.5),
      target_init("y1", quote(x), priority = 0.2),
      target_init("y2", quote(x), priority = 0.1),
      target_init("y3", quote(x), priority = 0.3),
      target_init("z1", quote(x), priority = 0.8),
      target_init("z2", quote(x), priority = 0.7),
      target_init("z3", quote(x), priority = 0.9),
      target_init("w", quote(c(y1, y2, y3, z1, z2, z3)), priority = 1)
    )
  )
  edges <- pipeline_upstream_edges(pipeline, targets_only = TRUE)
  graph <- graph_init(remove_loops(edges))
  igraph <- igraph::simplify(igraph::graph_from_data_frame(edges))
  priorities <- pipeline_get_priorities(pipeline)
  names <- topo_sort_by_priority(igraph, priorities)
  out <- initial_ranks(names, graph, priorities)
  exp <- c(
    x = 0 - (0.5 / 2),
    z3 = 1 - (0.9 / 2),
    z1 = 1 - (0.8 / 2),
    z2 = 1 - (0.7 / 2),
    y3 = 1 - (0.3 / 2),
    y1 = 1 - (0.2 / 2),
    y2 = 1 - (0.1 / 2),
    w = 6 - (1 / 2)
  )
  expect_equal(out, exp)
})

tar_test("validate empty scheduler", {
  s <- scheduler_init()
  expect_silent(s$validate())
})

tar_test("validate nonempty scheduler", {
  s <- scheduler_init(pipeline_order())
  expect_silent(s$validate())
})
