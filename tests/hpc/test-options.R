tar_test("options get exported to workers", {
  skip_if_not_installed("clustermq")
  tar_script({
    options(clustermq.scheduler = "multiprocess")
    tar_option_set(error = "continue", envir = globalenv())
    tar_target(x, writeLines(targets::tar_option_get("error"), "error.txt"))
  })
  tar_make_clustermq()
  expect_equal(readLines("error.txt"), "continue")
})

tar_test("same with the future backend and non-global environment", {
  skip_if_not_installed("future")
  tar_script({
    future::plan(future::multisession)
    envir <- new.env(parent = globalenv())
    tar_option_set(error = "continue", envir = envir)
    tar_target(x, writeLines(targets::tar_option_get("error"), "error.txt"))
  })
  tar_make_future()
  expect_equal(readLines("error.txt"), "continue")
})

tar_test("runtime settings get exported to workers", {
  skip_if_not_installed("clustermq")
  tar_script({
    options(clustermq.scheduler = "multiprocess")
    list(
      tar_target(x, targets::tar_store()),
      tar_target(y, targets::tar_function())
    )
  })
  tar_make_clustermq(store = "local_store")
  expect_equal(tar_read(x, store = "local_store"), "local_store")
  expect_equal(tar_read(y, store = "local_store"), "tar_make_clustermq")
  expect_null(tar_function())
  expect_false(tar_store() == "local_store")
})

tar_test("same with the future backend", {
  skip_if_not_installed("future")
  tar_script({
    future::plan(future::multisession)
    list(
      tar_target(x, targets::tar_store()),
      tar_target(y, targets::tar_function())
    )
  })
  tar_make_future(store = "local_store")
  expect_equal(tar_read(x, store = "local_store"), "local_store")
  expect_equal(tar_read(y, store = "local_store"), "tar_make_future")
  expect_null(tar_function())
  expect_false(tar_store() == "local_store")
})
