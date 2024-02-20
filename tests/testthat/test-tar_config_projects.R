tar_test("tar_config_projects()", {
  yaml <- tempfile()
  tar_config_set(store = "my_store_a", config = yaml, project = "project_a")
  tar_config_set(store = "my_store_b", config = yaml, project = "project_b")
  out <- tar_config_projects(config = yaml)
  expect_equal(sort(out), sort(c("project_a", "project_b")))
})
