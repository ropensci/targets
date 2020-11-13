tar_test("tar_destroy('all')", {
  pipeline <- pipeline_init(list(target_init("x", quote(0))))
  local_init(pipeline)$run()
  expect_true(file.exists("_targets"))
  tar_destroy(destroy = "all")
  expect_false(file.exists("_targets"))
})

tar_test("tar_destroy('meta')", {
  pipeline <- pipeline_init(list(target_init("x", quote(0))))
  local_init(pipeline)$run()
  expect_true(file.exists("_targets"))
  tar_destroy(destroy = "meta")
  expect_true(file.exists("_targets"))
  expect_false(file.exists(file.path("_targets", "meta", "meta")))
})

tar_test("tar_destroy('progress')", {
  pipeline <- pipeline_init(list(target_init("x", quote(0))))
  local_init(pipeline)$run()
  expect_true(file.exists("_targets"))
  tar_destroy(destroy = "progress")
  expect_true(file.exists("_targets"))
  expect_false(file.exists(file.path("_targets", "meta", "progress")))
})

tar_test("tar_destroy('objects')", {
  pipeline <- pipeline_init(list(target_init("x", quote(0))))
  local_init(pipeline)$run()
  expect_true(file.exists("_targets"))
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
