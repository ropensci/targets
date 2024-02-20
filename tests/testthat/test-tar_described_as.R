tar_test("tar_described_as() return character vector", {
  tar_script(
    list(
      tar_target(b2, TRUE, description = "blue two"),
      tar_target(b3, TRUE, description = "blue three"),
      list(
        tar_target(g2, TRUE, description = "green two"),
        tar_target(g3, TRUE, description = "green three"),
        tar_target(g4, TRUE, description = "green three"),
        tar_target(none, TRUE),
        NULL
      )
    )
  )
  out <- tar_described_as(
    tidyselect::starts_with("green"),
    tidyselect = FALSE,
    callr_function = NULL
  )
  expect_equal(sort(out), sort(c("g2", "g3", "g4")))
  out <- tar_described_as(
    tidyselect::ends_with("two"),
    tidyselect = FALSE,
    callr_function = NULL
  )
  expect_equal(sort(out), sort(c("b2", "g2")))
})

tar_test("tar_described_as() to select target names in another function", {
  skip_cran()
  tar_script(
    list(
      tar_target(b2, TRUE, description = "blue two"),
      tar_target(b3, TRUE, description = "blue three"),
      list(
        tar_target(g2, TRUE, description = "green two"),
        tar_target(g3, TRUE, description = "green three"),
        tar_target(g4, TRUE, description = "green three"),
        tar_target(none, TRUE),
        NULL
      )
    )
  )
  out <- tar_manifest(
    names = tar_described_as(
      tidyselect::starts_with("green"),
      callr_function = NULL
    ),
    callr_function = NULL
  )
  expect_equal(sort(out$name), sort(c("g2", "g3", "g4")))
  out <- tar_manifest(
    names = tar_described_as(tidyselect::ends_with("two")),
    callr_function = NULL
  )
  expect_equal(sort(out$name), sort(c("b2", "g2")))
  expect_error(
    tar_described_as(
      tidyselect::starts_with("green"),
      callr_function = NULL
    )
  )
})
