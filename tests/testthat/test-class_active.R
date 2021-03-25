tar_test("active$produce_exports(copy = TRUE)", {
  active <- active_new()
  envir <- new.env(parent = emptyenv())
  envir$target <- tar_target(x, 1)
  envir$pipeline <- pipeline_init()
  envir$.hidden <- "hidden"
  envir$visible <- "visible"
  expect_true(inherits(envir$target, "tar_target"))
  expect_true(inherits(envir$pipeline, "tar_pipeline"))
  out <- active$produce_exports(envir, copy = TRUE)
  expect_equal(length(out), 3L)
  names <- c(".hidden", "visible", ".tar_envir_5048826d")
  expect_equal(sort(names(out)), sort(names))
  expect_equal(out$.hidden, "hidden")
  expect_equal(out$visible, "visible")
  expect_null(out$target)
  expect_null(out$pipeline)
})

tar_test("active$produce_exports(copy = FALSE)", {
  active <- active_new()
  envir <- new.env(parent = emptyenv())
  envir$target <- tar_target(x, 1)
  envir$pipeline <- pipeline_init()
  envir$.hidden <- "hidden"
  envir$visible <- "visible"
  expect_true(inherits(envir$target, "tar_target"))
  expect_true(inherits(envir$pipeline, "tar_pipeline"))
  out <- active$produce_exports(envir, copy = FALSE)
  expect_equal(length(out), 1L)
  expect_equal(names(out), ".tar_envir_5048826d")
  expect_null(out$.hidden)
  expect_null(out$visible)
  expect_null(out$target)
  expect_null(out$pipeline)
})
