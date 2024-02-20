tar_test("tar_config_projects()", {
  yaml <- tempfile()
  tar_config_set(store = "my_store_a", config = yaml, project = "project_a")
  tar_config_set(store = "my_store_b", config = yaml, project = "project_b")
  out <- tar_config_yaml(config = yaml)
  exp <- list(
    project_a = list(store = "my_store_a"),
    project_b = list(store = "my_store_b")
  )
  expect_equal(out, exp)
})
