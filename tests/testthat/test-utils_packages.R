tar_test("load packages", {
  expect_silent(load_packages(packages = "digest", library = NULL))
  expect_error(
    suppressWarnings(
      load_packages(packages = "does;not;exist", library = NULL)
    ),
    class = "tar_condition_validate"
  )
})
