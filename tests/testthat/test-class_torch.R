tar_test("torch format", {
  skip_if_not_installed("torch")
  tar_script({
    f <- function() {
      x <- array(runif(8), dim = c(2, 2, 2))
      torch::torch_tensor(x, dtype = torch::torch_float64())
    }
    tar_pipeline(tar_target(abc, f(), format = "torch"))
  })
  tar_make(callr_function = NULL)
  out <- tar_read(abc)
  expect_true(inherits(out, "torch_tensor"))
})

tar_test("torch format with in-memory serialization", {
  skip_if_not_installed("torch")
  tar_script({
    f <- function() {
      x <- array(runif(8), dim = c(2, 2, 2))
      torch::torch_tensor(x, dtype = torch::torch_float64())
    }
    tar_pipeline(tar_target(abc, f(), format = "torch"))
  })
  tar_make_future(callr_function = NULL)
  out <- tar_read(abc)
  expect_true(inherits(out, "torch_tensor"))
})

tar_test("validate torch format", {
  skip_if_not_installed("torch")
  x <- target_init(name = "abc", expr = quote(f()), format = "torch")
  expect_silent(target_validate(x))
})
