tar_test("algorithm can instantiate", {
  expect_true(is.environment(algorithm_new()))
})

tar_test("active can instantiate", {
  expect_true(is.environment(active_new()))
})

tar_test("passive can instantiate", {
  expect_true(is.environment(passive_new()))
})

tar_test("builder can instantiate", {
  expect_true(is.environment(builder_new()))
})

tar_test("value can instantiate", {
  expect_true(is.environment(value_new()))
})

tar_test("pattern can instantiate", {
  expect_true(is.environment(pattern_new()))
})

tar_test("target can instantiate", {
  expect_true(is.environment(target_new()))
})

tar_test("queue can instantiate", {
  queue <- queue_new(1)
  expect_true(is.environment(queue))
  expect_null(queue$is_nonempty())
  expect_null(queue$dequeue())
  expect_silent(queue$append())
  expect_silent(queue$prepend())
  expect_silent(queue$dequeue())
  expect_silent(queue$increment_ranks())
})

tar_test("reporter can instantiate", {
  expect_true(is.environment(reporter_new()))
})

tar_test("network can instantiate", {
  expect_true(is.environment(network_new(pipeline_init())))
})

tar_test("visual can instantiate", {
  vis <- visual_new(glimpse_init(pipeline_init()))
  expect_true(is.environment(vis))
})
