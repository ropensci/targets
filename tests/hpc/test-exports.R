tar_test("options get exported to workers", {
  skip_if_not_installed("clustermq")
  tar_script({
    options(clustermq.scheduler = "multiprocess")
    tar_option_set(
      error = "continue",
      envir = globalenv()
    )
    list(
      tar_target(x, writeLines(targets::tar_option_get("error"), "error.txt"))
    )
  })
  tar_make_clustermq()
  expect_equal(readLines("error.txt"), "continue")
})

tar_test("same with the future backend and non-global environment", {
  skip_if_not_installed("future")
  tar_script({
    future::plan(future::multisession)
    envir <- new.env(parent = globalenv())
    tar_option_set(
      error = "continue",
      envir = globalenv()
    )
    list(
      tar_target(x, writeLines(targets::tar_option_get("error"), "error.txt"))
    )
  })
  tar_make_future()
  expect_equal(readLines("error.txt"), "continue")
})

tar_test("runtime settings get exported to workers", {
  skip_if_not_installed("clustermq")
  tar_script({
    options(clustermq.scheduler = "multiprocess")
    Sys.setenv(TAR_WARN = "success")
    list(
      tar_target(x, targets::tar_store()),
      tar_target(y, targets::tar_call()),
      tar_target(z, Sys.getenv("TAR_WARN"))
    )
  })
  tar_make_clustermq(store = "local_store")
  expect_equal(tar_read(x, store = "local_store"), "local_store")
  expect_equal(tar_read(y, store = "local_store"), "tar_make_clustermq")
  expect_equal(tar_read(z, store = "local_store"), "success")
  expect_null(tar_call())
  expect_false(tar_store() == "local_store")
})

tar_test("same with the future backend", {
  skip_if_not_installed("future")
  tar_script({
    future::plan(future::multisession)
    Sys.setenv(TAR_WARN = "success")
    list(
      tar_target(x, targets::tar_store()),
      tar_target(y, targets::tar_call()),
      tar_target(z, Sys.getenv("TAR_WARN"))
    )
  })
  tar_make_future(store = "local_store")
  expect_equal(tar_read(x, store = "local_store"), "local_store")
  expect_equal(tar_read(y, store = "local_store"), "tar_make_future")
  expect_equal(tar_read(z, store = "local_store"), "success")
  expect_null(tar_call())
  expect_false(tar_store() == "local_store")
})
