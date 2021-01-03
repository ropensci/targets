tar_test("active$produce_exports()", {
  active <- active_new()
  envir <- new.env(parent = emptyenv())
  envir$target <- tar_target(x, 1)
  envir$pipeline <- pipeline_init()
  envir$.hidden <- "hidden"
  envir$visible <- "visible"
  expect_true(inherits(envir$target, "tar_target"))
  expect_true(inherits(envir$pipeline, "tar_pipeline"))
  out <- active$produce_exports(envir)
  expect_equal(length(out), 2L)
  expect_equal(sort(names(out)), sort(c(".hidden", "visible")))
  expect_equal(out$.hidden, "hidden")
  expect_equal(out$visible, "visible")
  expect_null(out$target)
  expect_null(out$pipeline)
})
