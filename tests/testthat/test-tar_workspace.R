tar_test("workspaces are not saved if error = 'stop'", {
  pipeline <- pipeline_init(
    list(
      target_init("y", quote(1)),
      target_init("x", quote(stop(y)), error = "stop")
    )
  )
  local <- local_init(pipeline, reporter = "verbose")
  expect_error(expect_message(local$run()), class = "tar_condition_run")
  expect_false(file.exists(path_workspace("x")))
})

tar_test("workspaces are not saved if error = 'continue'", {
  pipeline <- pipeline_init(
    list(
      target_init("y", quote(12345)),
      target_init("x", quote(stop(y)), error = "continue")
    )
  )
  local <- local_init(pipeline, reporter = "verbose")
  suppressMessages(local$run())
  expect_true(grepl("12345", tar_meta(x)$error[[1]]))
  expect_false(file.exists(path_workspace("x")))
})

tar_test("workspaces are saved if error = 'save'", {
  pipeline <- pipeline_init(
    list(
      target_init("y", quote(1)),
      target_init("x", quote(stop(y)), error = "workspace")
    )
  )
  local <- local_init(pipeline, reporter = "verbose")
  expect_error(expect_message(local$run()), class = "tar_condition_run")
  expect_true(file.exists(path_workspace("x")))
})

tar_test("tar_workspace() works", {
  tmp <- sample(1)
  tar_script({
    tar_option_set(error = "workspace")
    list(
      tar_target(x, "loaded"),
      tar_target(y, stop(x))
    )
  })
  try(tar_make(callr_function = NULL), silent = TRUE)
  seed <- .Random.seed
  envir <- new.env(parent = globalenv())
  tar_workspace(y, envir = envir)
  expect_equal(envir$x, "loaded")
  expect_false(identical(seed, .Random.seed))
})

tar_test("tar_workspace() works with workspace option", {
  tmp <- sample(1)
  tar_script({
    tar_option_set(workspaces = "y")
    list(
      tar_target(x, "loaded"),
      tar_target(y, paste0(x, "nope"))
    )
  })
  tar_make(callr_function = NULL)
  envir <- new.env(parent = globalenv())
  tar_workspace(y, envir = envir)
  expect_equal(envir$x, "loaded")
})

tar_test("tar_workspace() on a branch", {
  tar_script({
    tar_option_set(error = "workspace")
    list(
      tar_target(x, seq_len(4L)),
      tar_target(y, stopifnot(x < 4L), pattern = map(x))
    )
  })
  try(tar_make(callr_function = NULL), silent = TRUE)
  meta <- tar_meta(fields = error)
  failed <- meta[!is.na(unlist(meta$error)), ]$name
  envir <- new.env(parent = globalenv())
  eval(substitute(
    tar_workspace(name = name, envir = envir),
    env = list(name = failed)
  ))
  expect_equal(envir$x, 4L)
})

tar_test("tar_workspace() with an unexportable object", {
  skip_on_cran()
  skip_if_not_installed("torch")
  tar_script({
    tar_option_set(error = "workspace")
    list(
      tar_target(tensor, torch::torch_zeros(10), format = "torch"),
      tar_target(array, stop(tensor))
    )
  })
  try(tar_make(callr_function = NULL), silent = TRUE)
  envir <- new.env(parent = globalenv())
  tar_workspace(array, envir)
  expect_true(inherits(envir$tensor, "torch_tensor"))
  expect_equal(as.numeric(sum(envir$tensor)), 0)
})

tar_test("workspace saved on no error and when target is skipped", {
  path <- path_workspace("z")
  tar_script({
    list(tar_target(z, 0))
  })
  tar_make(callr_function = NULL)
  expect_false(file.exists(path))
  tar_script({
    tar_option_set(workspaces = "z")
    list(tar_target(z, 0))
  })
  tar_make(callr_function = NULL)
  expect_true(file.exists(path))
})
