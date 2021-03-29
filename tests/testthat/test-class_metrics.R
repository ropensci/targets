tar_test("metrics_has_error() with error", {
  metrics <- metrics_new(error = "123L")
  expect_true(metrics_has_error(metrics))
})

tar_test("metrics_has_error() without error", {
  metrics <- metrics_new()
  expect_false(metrics_has_error(metrics))
})

tar_test("metrics_has_cancel() with cancel", {
  metrics <- metrics_new(cancel = "123L")
  expect_true(metrics_has_cancel(metrics))
})

tar_test("metrics_has_cancel() without cancel", {
  metrics <- metrics_new()
  expect_false(metrics_has_cancel(metrics))
})

tar_test("metrics_terminated_early() with error", {
  metrics <- metrics_new(error = "123L")
  expect_true(metrics_terminated_early(metrics))
})

tar_test("metrics_terminated_early() with cancel", {
  metrics <- metrics_new(cancel = "123L")
  expect_true(metrics_terminated_early(metrics))
})

tar_test("metrics_terminated_early() without cancel", {
  metrics <- metrics_new()
  expect_false(metrics_terminated_early(metrics))
})

tar_test("validate metrics with bad seconds", {
  metrics <- metrics_new()
  expect_error(metrics_validate(metrics), class = "tar_condition_validate")
})

tar_test("validate metrics with bad warnings", {
  metrics <- metrics_new(seconds = 1, warnings = 123)
  expect_error(metrics_validate(metrics), class = "tar_condition_validate")
})

tar_test("validate metrics with bad error", {
  metrics <- metrics_new(seconds = 1, error = 123)
  expect_error(metrics_validate(metrics), class = "tar_condition_validate")
})

tar_test("validate metrics with bad traceback", {
  metrics <- metrics_new(seconds = 1, traceback = 123)
  expect_error(metrics_validate(metrics), class = "tar_condition_validate")
})
