# Wanted to automate these tests, but there is a mysterious segfault
# on GitHub Actions: https://github.com/wlandau/targets/runs/1193098948 # nolint
tar_test("torch format", {
  skip_on_cran()
  skip_if_not_installed("torch")
  tar_script({
    tar_pipeline(tar_target(a, torch::torch_tensor(c(1, 2)), format = "torch"))
  })
  tar_make(callr_function = NULL)
  out <- tar_read(a)
  expect_true(inherits(out, "torch_tensor"))
  expect_equal(torch::as_array(out), c(1, 2))
})

tar_test("torch format with in-memory serialization", {
  skip_on_cran()
  skip_if_not_installed("torch")
  future::plan(future::sequential)
  tar_script({
    tar_pipeline(tar_target(a, torch::torch_tensor(c(1, 2)), format = "torch"))
  })
  tar_make_future(callr_function = NULL)
  out <- tar_read(a)
  expect_true(inherits(out, "torch_tensor"))
  expect_equal(torch::as_array(out), c(1, 2))
})
