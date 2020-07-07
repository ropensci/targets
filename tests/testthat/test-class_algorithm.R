tar_test("unsupported algorithm", {
  expect_error(
    algorithm_init("invalid", pipeline_order()),
    class = "condition_validate"
  )
})
