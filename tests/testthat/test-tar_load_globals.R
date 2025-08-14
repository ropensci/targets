tar_test("tar_load_globals", {
  tar_script(
    {
      tar_option_set(packages = "callr")
      analyze_data <- function(data) {
        summary(data)
      }
      list(
        tar_target(x, 1 + 1),
        tar_target(y, 1 + 1)
      )
    },
    ask = FALSE
  )
  envir <- new.env(parent = globalenv())
  tar_load_globals(envir = envir)
  expect_true(is.function(envir$analyze_data))
  expect_true("callr" %in% (.packages()))
})
