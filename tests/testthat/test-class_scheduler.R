tar_test("scheduler$graph", {
  s <- scheduler_init()
  expect_silent(s$graph$validate())
})

tar_test("scheduler$queue", {
  s <- scheduler_init()
  expect_silent(s$queue$validate())
})

tar_test("queue has correct names", {
  pipeline <- pipeline_order()
  s <- scheduler_init(pipeline)
  out <- s$queue$get_names()
  exp <- pipeline_get_names(pipeline)
  expect_equal(sort(out), sort(exp))
})

tar_test("queue has correct ranks", {
  queue <- scheduler_init(pipeline_order())$queue
  out <- queue$data
  exp <- c(
    data1 = 0L,
    data2 = 0L,
    min1 = 1L,
    min2 = 1L,
    max1 = 1L,
    max2 = 1L,
    mins = 2L,
    maxes = 2L,
    all = 2L
  )
  expect_equal(out[sort(names(out))], exp[sort(names(exp))])
})

tar_test("scheduler$count_unfinished_deps()", {
  pipeline <- pipeline_order()
  target <- pipeline_get_target(pipeline, "mins")
  local <- algorithm_init("local", pipeline)
  scheduler <- local$scheduler
  expect_equal(scheduler$count_unfinished_deps("mins"), 2L)
  local$ensure_meta()
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

tar_test("validate empty scheduler", {
  s <- scheduler_init()
  expect_silent(s$validate())
})

tar_test("validate nonempty scheduler", {
  s <- scheduler_init(pipeline_order())
  expect_silent(s$validate())
})
