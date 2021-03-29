tar_test("tar_bind() works", {
  tar_script({
    pipeline1 <- tar_pipeline(
      tar_target(a, 1L),
      list(tar_target(b, 2L), tar_target(c, 3L))
    )
    pipeline2 <- tar_pipeline(tar_target(d, a + b + c))
    pipeline3 <- tar_pipeline(tar_target(e, a + d))
    tar_bind(pipeline1, list(pipeline2, pipeline3))
  })
  suppressWarnings(
    expect_warning(
      tar_make(callr_function = NULL),
      class = "tar_condition_deprecate"
    )
  )
  expect_equal(tar_read(a), 1L)
  expect_equal(tar_read(b), 2L)
  expect_equal(tar_read(c), 3L)
  expect_equal(tar_read(d), 6L)
  expect_equal(tar_read(e), 7L)
})

tar_test("tar_bind() errors if names are duplicated", {
  tar_script({
    pipeline1 <- tar_pipeline(tar_target(a, 1))
    pipeline2 <- tar_pipeline(tar_target(a, 2))
    tar_bind(pipeline1, pipeline2)
  })
  suppressWarnings(
    expect_error(
      tar_make(callr_function = NULL),
      class = "tar_condition_validate"
    )
  )
})

tar_test("valid pipeline with patterns only (#245)", {
  expect_silent(
    out1 <- pipeline_init(list(tar_target(x, y, pattern = map(y))))
  )
  expect_silent(
    out2 <- pipeline_init(list(tar_target(z, w, pattern = map(w))))
  )
  suppressWarnings(
    expect_warning(
      out3 <- tar_bind(out1, out2),
      class = "tar_condition_deprecate"
    )
  )
  expect_silent(pipeline_validate(out1))
  expect_silent(pipeline_validate(out2))
  expect_silent(pipeline_validate(out3))
})
