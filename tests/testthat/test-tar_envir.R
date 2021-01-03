tar_test("tar_envir() automatic default", {
  out <- tar_envir()
  exp <- environment()
  expect_equal(out, exp)
})

tar_test("tar_envir() custom default", {
  exp <- new.env(parent = emptyenv())
  out <- tar_envir(default = exp)
  expect_equal(out, exp)
})

tar_test("tar_envir() in a pipeline with automatic default", {
  tar_script({
    f <- function(z) {
      g(z)
    }
    g <- function(z) {
      ls(tar_envir())
    }
    list(
      tar_target(x_target, "x_value"),
      tar_target(y, f(x_target))
    )
  })
  tar_make(callr_function = NULL)
  expect_equal(tar_read(y), "x_target")
})

tar_test("tar_envir() in a pipeline with custom default", {
  tar_script({
    f <- function(z) {
      g(z)
    }
    g <- function(z) {
      default <- new.env(parent = emptyenv())
      ls(tar_envir(default = default))
    }
    list(
      tar_target(x_target, "x_value"),
      tar_target(y, f(x_target))
    )
  })
  tar_make(callr_function = NULL)
  expect_equal(tar_read(y), "x_target")
})
