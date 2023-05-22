tar_test("load packages", {
  expect_silent(load_packages(packages = "digest", library = NULL))
  expect_error(
    suppressWarnings(
      load_packages(packages = "does;not;exist", library = NULL)
    ),
    class = "tar_condition_validate"
  )
})

tar_test("package_version_check", {
  expect_silent(
    package_version_check(
      package = "crew",
      version = "0.0.0",
      repo = "https://wlandau.r-universe.dev"
    )
  )
  expect_message(
    package_version_check(
      package = "none",
      version = "9999.9999.9999",
      repo = "https://wlandau.r-universe.dev"
    )
  )
})
