tar_test("workspaces are not saved if error = 'stop'", {
  pipeline <- pipeline_init(
    list(
      target_init("y", quote(1)),
      target_init("x", quote(stop(y)), error = "stop")
    )
  )
  local <- local_init(pipeline, reporter = "verbose")
  expect_error(expect_message(local$run()), class = "condition_run")
  expect_false(file.exists(path_workspaces("x")))
})

tar_test("workspaces are not saved if error = 'continue'", {
  pipeline <- pipeline_init(
    list(
      target_init("y", quote(1)),
      target_init("x", quote(stop(y)), error = "continue")
    )
  )
  local <- local_init(pipeline, reporter = "verbose")
  expect_message(local$run(), class = "condition_run")
  expect_false(file.exists(path_workspaces("x")))
})

tar_test("workspaces are saved if error = 'save'", {
  pipeline <- pipeline_init(
    list(
      target_init("y", quote(1)),
      target_init("x", quote(stop(y)), error = "save")
    )
  )
  local <- local_init(pipeline, reporter = "verbose")
  expect_error(expect_message(local$run()), class = "condition_run")
  expect_true(file.exists(path_workspaces("x")))
})

tar_test("tar_workspace() works", {
  tmp <- sample(1)
  tar_script({
    tar_option_set(error = "save")
    tar_pipeline(
      tar_target(x, "loaded"),
      tar_target(y, stop(x))
    )
  })
  try(tar_make(callr_function = NULL), silent = TRUE)
  exists("x")
  seed <- .Random.seed
  envir <- new.env(parent = emptyenv())
  tar_workspace(y, envir = envir)
  expect_equal(envir$x, "loaded")
  expect_true(is.integer(envir$.targets$seed))
  expect_true(is.character(envir$.targets$traceback))
  expect_false(identical(seed, .Random.seed))
})

tar_test("tar_workspace() on a branch", {
  tar_script({
    tar_option_set(error = "save")
    tar_pipeline(
      tar_target(x, seq_len(4L)),
      tar_target(y, stopifnot(x < 4L), pattern = map(x))
    )
  })
  try(tar_make(callr_function = NULL), silent = TRUE)
  meta <- tar_meta(fields = error)
  failed <- meta[!is.na(unlist(meta$error)), ]$name
  envir <- new.env(parent = emptyenv())
  eval(substitute(
    tar_workspace(name = name, envir = envir),
    env = list(name = failed)
  ))
  expect_equal(envir$x, 4L)
})
