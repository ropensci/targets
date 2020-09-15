tar_test("dynamic urls work", {
  tar_script({
    tar_pipeline(
      tar_target(
        abc,
        rep("https://github.com/ropensci/drake/archive/v7.3.0.tar.gz", 2),
        format = "url"
      )
    )
  })
  tar_make(callr_function = NULL)
  exp <- tibble::tibble(name = "abc", progress = "built")
  expect_equal(tar_progress(), exp)
  tar_make(callr_function = NULL)
  exp <- tibble::tibble(name = character(0), progress = character(0))
  expect_equal(tar_progress(), exp)
  meta <- tar_meta(abc)
  expect_equal(nchar(meta$data), 16)
  out <- meta$path[[1]]
  exp <- rep("https://github.com/ropensci/drake/archive/v7.3.0.tar.gz", 2)
  expect_equal(out, exp)
  expect_equal(tar_read(abc), exp)
  expect_false(file.exists(file.path("_targets", "objects", "abc")))
})

tar_test("dynamic urls must return characters", {
  x <- target_init(
    name = "abc",
    expr = quote(list(list("illegal"))),
    format = "url"
  )
  pipeline <- pipeline_init(list(x))
  local <- local_init(pipeline = pipeline)
  expect_error(local$run(), class = "condition_validate")
})
