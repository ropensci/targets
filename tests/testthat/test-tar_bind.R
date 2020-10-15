test_that("tar_bind() works", {
  tar_script({
    pipeline1 <- tar_pipeline(
      tar_target(a, 1L),
      list(tar_target(b, 2L), tar_target(c, 3L))
    )
    pipeline2 <- tar_pipeline(tar_target(d, a + b + c))
    pipeline3 <- tar_pipeline(tar_target(e, a + d))
    tar_bind(pipeline1, list(pipeline2, pipeline3))
  })
  tar_make(callr_function = NULL)
  expect_equal(tar_read(a), 1L)
  expect_equal(tar_read(b), 2L)
  expect_equal(tar_read(c), 3L)
  expect_equal(tar_read(d), 6L)
  expect_equal(tar_read(e), 7L)
})

test_that("tar_bind() errors if names are duplicated", {
  tar_script({
    pipeline1 <- tar_pipeline(tar_target(a, 1))
    pipeline2 <- tar_pipeline(tar_target(a, 2))
    tar_bind(pipeline1, pipeline2)
  })
  expect_error(tar_make(callr_function = NULL), class = "condition_validate")
})
