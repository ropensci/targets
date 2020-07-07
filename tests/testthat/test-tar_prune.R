tar_test("tar_prune() works", {
  pipeline <- pipeline_init(
    list(
      target_init("y1", quote(1 + 1)),
      target_init("y2", quote(1 + 1)),
      target_init("z", quote(y1 + y2))
    )
  )
  algorithm_init("local", pipeline = pipeline)$run()
  tar_script(tar_pipeline(tar_target(y1, quote(1))))
  tar_prune(callr_arguments = list(show = FALSE))
  data <- meta_init()$database$read_data()
  expect_equal(data$name, "y1")
  files <- list.files(file.path("_targets", "objects"))
  expect_equal(files, "y1")
})

tar_test("tar_prune() works with patterns", {
  pipeline <- pipeline_init(
    list(
      target_init("x", quote(seq_len(2))),
      target_init("y", quote(x), pattern = quote(map(x))),
      target_init("z", quote(y), pattern = quote(map(y)))
    )
  )
  algorithm_init("local", pipeline = pipeline)$run()
  tar_script(tar_pipeline(tar_target(y, x, map(x))))
  tar_prune(callr_arguments = list(show = FALSE))
  names <- meta_init()$database$read_data()$name
  expect_equal(length(names), 3L)
  expect_equal(length(unique(names)), 3L)
  expect_true(all(grepl("^y", names)))
  names <- list.files(file.path("_targets", "objects"))
  expect_equal(length(names), 2L)
  expect_equal(length(unique(names)), 2L)
  expect_true(all(grepl("^y_", names)))
})

tar_test("tar_prune() does not remove global objects from metadata", {
  envir <- new.env(parent = baseenv())
  envir$a <- 1L
  envir$f <- identity
  x <- target_init("x", quote(f(a)), envir = envir)
  pipeline <- pipeline_init(list(x))
  algorithm_init("local", pipeline = pipeline)$run()
  tar_script(tar_pipeline(tar_target(x, quote(1))))
  tar_prune(callr_arguments = list(show = FALSE))
  names <- meta_init()$database$read_data()$name
  expect_equal(sort(names), sort(c("a", "f", "x")))
})

tar_test("tar_delete() does not delete dynamic files", {
  file.create("x")
  pipeline <- pipeline_init(
    list(target_init("x", quote("x"), format = "file"))
  )
  algorithm_init("local", pipeline = pipeline)$run()
  names <- meta_init()$database$read_data()$name
  expect_equal(names, "x")
  tar_script(tar_pipeline(tar_target(y, quote(1))))
  tar_prune(callr_arguments = list(show = FALSE))
  expect_true(file.exists("x"))
  names <- meta_init()$database$read_data()$name
  expect_equal(length(names), 0L)
})
