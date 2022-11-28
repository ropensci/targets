tar_test("no store", {
  expect_silent(tar_destroy())
})

tar_test("tar_destroy('all')", {
  pipeline <- pipeline_init(list(target_init("x", quote(0))))
  local_init(pipeline)$run()
  expect_true(file.exists("_targets"))
  tar_destroy(destroy = "all")
  expect_false(file.exists("_targets"))
})

tar_test("tar_destroy('local')", {
  pipeline <- pipeline_init(list(target_init("x", quote(0))))
  local_init(pipeline)$run()
  expect_true(file.exists("_targets"))
  tar_destroy(destroy = "local")
  expect_false(file.exists("_targets"))
})

tar_test("tar_destroy('meta')", {
  pipeline <- pipeline_init(list(target_init("x", quote(0))))
  local_init(pipeline)$run()
  expect_true(file.exists("_targets"))
  expect_true(file.exists(file.path("_targets", "meta", "meta")))
  tar_destroy(destroy = "meta")
  expect_true(file.exists("_targets"))
  expect_false(file.exists(file.path("_targets", "meta", "meta")))
})

tar_test("tar_destroy('progress')", {
  pipeline <- pipeline_init(list(target_init("x", quote(0))))
  local_init(pipeline)$run()
  expect_true(file.exists("_targets"))
  expect_true(file.exists(file.path("_targets", "meta", "process")))
  tar_destroy(destroy = "process")
  expect_true(file.exists("_targets"))
  expect_false(file.exists(file.path("_targets", "meta", "process")))
})

tar_test("tar_destroy('progress')", {
  pipeline <- pipeline_init(list(target_init("x", quote(0))))
  local_init(pipeline)$run()
  expect_true(file.exists("_targets"))
  expect_true(file.exists(file.path("_targets", "meta", "progress")))
  tar_destroy(destroy = "progress")
  expect_true(file.exists("_targets"))
  expect_false(file.exists(file.path("_targets", "meta", "progress")))
})

tar_test("tar_destroy('objects')", {
  pipeline <- pipeline_init(list(target_init("x", quote(0))))
  local_init(pipeline)$run()
  expect_true(file.exists("_targets"))
  expect_true(file.exists(file.path("_targets", "objects")))
  tar_destroy(destroy = "objects")
  expect_true(file.exists("_targets"))
  expect_false(file.exists(file.path("_targets", "objects")))
})

tar_test("tar_destroy('scratch')", {
  path <- file.path("_targets", "scratch")
  dir.create(path, recursive = TRUE)
  expect_true(file.exists(path))
  tar_destroy(destroy = "scratch")
  expect_false(file.exists(path))
})

tar_test("tar_destroy('workspaces')", {
  path <- file.path("_targets", "workspaces")
  dir.create(path, recursive = TRUE)
  expect_true(file.exists(path))
  tar_destroy(destroy = "workspaces")
  expect_false(file.exists(path))
})

tar_test("tar_destroy('user')", {
  path <- file.path("_targets", "user")
  dir.create(path, recursive = TRUE)
  expect_true(file.exists(path))
  tar_destroy(destroy = "user")
  expect_false(file.exists(path))
})

tar_test("custom script and store args", {
  skip_cran()
  expect_equal(tar_config_get("script"), path_script_default())
  expect_equal(tar_config_get("store"), path_store_default())
  tar_script({
    tar_option_set(workspace_on_error = TRUE)
    list(tar_target(x, "value"), tar_target(y, stop(x)))
  }, script = "example/script.R")
  expect_false(file.exists("example/store/workspaces/y"))
  try(
    tar_make(
      callr_function = NULL,
      script = "example/script.R",
      store = "example/store"
    ),
    silent = TRUE
  )
  expect_true(file.exists("example/store"))
  tar_destroy(store = "example/store")
  expect_false(file.exists("example/store"))
  expect_false(file.exists("_targets.yaml"))
  expect_equal(tar_config_get("script"), path_script_default())
  expect_equal(tar_config_get("store"), path_store_default())
  expect_false(file.exists(path_script_default()))
  expect_false(file.exists(path_store_default()))
  expect_true(file.exists("example/script.R"))
  tar_config_set(script = "x")
  expect_equal(tar_config_get("script"), "x")
  expect_true(file.exists("_targets.yaml"))
})
