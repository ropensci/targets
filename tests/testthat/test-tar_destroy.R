tar_test("tar_destroy('all')", {
  pipeline <- pipeline_init(list(target_init("x", quote(0))))
  local_init(pipeline)$run()
  expect_true(file.exists("_targets"))
  tar_destroy(what = "all")
  expect_false(file.exists("_targets"))
})

tar_test("tar_destroy('meta')", {
  pipeline <- pipeline_init(list(target_init("x", quote(0))))
  local_init(pipeline)$run()
  expect_true(file.exists("_targets"))
  tar_destroy(what = "meta")
  expect_true(file.exists("_targets"))
  expect_false(file.exists(file.path("_targets", "meta", "meta")))
})

tar_test("tar_destroy('progress')", {
  pipeline <- pipeline_init(list(target_init("x", quote(0))))
  local_init(pipeline)$run()
  expect_true(file.exists("_targets"))
  tar_destroy(what = "progress")
  expect_true(file.exists("_targets"))
  expect_false(file.exists(file.path("_targets", "meta", "progress")))
})

tar_test("tar_destroy('objects')", {
  pipeline <- pipeline_init(list(target_init("x", quote(0))))
  local_init(pipeline)$run()
  expect_true(file.exists("_targets"))
  tar_destroy(what = "objects")
  expect_true(file.exists("_targets"))
  expect_false(file.exists(file.path("_targets", "objects")))
})
