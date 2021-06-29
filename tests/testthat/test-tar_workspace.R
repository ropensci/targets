tar_test("workspaces are not saved if error = 'stop'", {
  pipeline <- pipeline_init(
    list(
      target_init("y", quote(1)),
      target_init("x", quote(stop(y)), error = "stop")
    )
  )
  local <- local_init(pipeline, reporter = "verbose")
  expect_error(expect_message(local$run()), class = "tar_condition_run")
  expect_false(file.exists(path_workspace(path_store_default(), "x")))
})

tar_test("workspaces are not saved by default", {
  pipeline <- pipeline_init(
    list(
      target_init("y", quote(12345)),
      target_init("x", quote(stop(y)), error = "continue")
    )
  )
  local <- local_init(pipeline, reporter = "verbose")
  suppressMessages(local$run())
  expect_true(grepl("12345", tar_meta(x)$error[[1]]))
  expect_false(file.exists(path_workspace(path_store_default(), "x")))
})

tar_test("workspaces are saved if requested on error", {
  tar_option_set(workspace_on_error = TRUE)
  pipeline <- pipeline_init(
    list(
      target_init("y", quote(1)),
      target_init("x", quote(stop(y)))
    )
  )
  local <- local_init(pipeline, reporter = "verbose")
  expect_error(expect_message(local$run()), class = "tar_condition_run")
  expect_true(file.exists(path_workspace(path_store_default(), "x")))
})

tar_test("deprecated workspace error option works", {
  pipeline <- pipeline_init(
    list(
      target_init("y", quote(1)),
      target_init("x", quote(stop(y)), error = "workspace")
    )
  )
  local <- local_init(pipeline, reporter = "verbose")
  expect_error(expect_message(local$run()), class = "tar_condition_run")
  expect_true(file.exists(path_workspace(path_store_default(), "x")))
})

tar_test("deprecated workspaces option works", {
  suppressWarnings(tar_option_set(workspaces = "x"))
  pipeline <- pipeline_init(
    list(
      target_init("y", quote(1)),
      target_init("x", quote(y))
    )
  )
  local <- local_init(pipeline, reporter = "verbose")
  local$run()
  expect_true(file.exists(path_workspace(path_store_default(), "x")))
})

tar_test("tar_workspace() works", {
  tmp <- sample(1)
  tar_script({
    tar_option_set(workspace_on_error = TRUE)
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

tar_test("tar_workspace() on a branch on error", {
  tar_script({
    tar_option_set(workspace_on_error = TRUE)
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

tar_test("tar_workspace() on a pattern by name", {
  tar_script({
    tar_option_set(workspaces = "y")
    list(
      tar_target(x, seq_len(2L)),
      tar_target(y, x, pattern = map(x))
    )
  })
  try(tar_make(callr_function = NULL), silent = TRUE)
  branches <- tar_branch_names("y", seq_len(2))
  result <- file.exists(path_workspace(path_store_default(), branches))
  expect_true(all(result))
})

tar_test("tar_workspace() on a pattern by name", {
  tar_script({
    tar_option_set(workspaces = "BRANCH")
    list(
      tar_target(x, seq_len(2L)),
      tar_target(y, x, pattern = map(x))
    )
  })
  tar_make(callr_function = NULL)
  expect_equal(tar_workspaces(), character(0))
  lines <- readLines(path_script_default())
  name <- tar_branch_names(y, 1)
  lines <- gsub("BRANCH", name, lines)
  writeLines(lines, path_script_default())
  tar_make(callr_function = NULL)
  expect_equal(tar_workspaces(), name)
})

tar_test("tar_workspace() with an unexportable object", {
  skip_on_cran()
  skip_if_not_installed("torch")
  tar_script({
    tar_option_set(workspace_on_error = TRUE)
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
  path <- path_workspace(path_store_default(), "z")
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

tar_test("custom script and store args", {
  skip_on_cran()
  expect_equal(tar_config_get("script"), path_script_default())
  expect_equal(tar_config_get("store"), path_store_default())
  tar_script({
    tar_option_set(workspace_on_error = TRUE)
    list(tar_target(x, "value"), tar_target(y, stop(x)))
  }, script = "example/script.R")
  try(
    tar_make(
      callr_function = NULL,
      script = "example/script.R",
      store = "example/store"
    ),
    silent = TRUE
  )
  envir <- new.env(parent = globalenv())
  expect_null(envir$x)
  tar_workspace(
    y,
    envir = envir,
    script = "example/script.R",
    store = "example/store"
  )
  expect_equal(envir$x, "value")
  expect_false(file.exists("_targets.yaml"))
  expect_equal(tar_config_get("script"), path_script_default())
  expect_equal(tar_config_get("store"), path_store_default())
  expect_false(file.exists(path_script_default()))
  expect_false(file.exists(path_store_default()))
  expect_true(file.exists("example/script.R"))
  expect_true(file.exists("example/store"))
  expect_true(file.exists("example/store/meta/meta"))
  expect_true(file.exists("example/store/workspaces/y"))
  tar_config_set(script = "x")
  expect_equal(tar_config_get("script"), "x")
  expect_true(file.exists("_targets.yaml"))
})
