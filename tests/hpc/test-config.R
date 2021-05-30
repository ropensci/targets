tar_test("config gets exported to workers", {
  skip_if_not_installed("clustermq")
  tar_config_set(store = "tmp")
  tar_script({
    options(clustermq.scheduler = "multiprocess")
    tar_target(x, writeLines(targets::tar_config_get("store"), "store.txt"))
  })
  tar_make_clustermq()
  expect_equal(readLines("store.txt"), "tmp")
})

tar_test("same with the future backend", {
  skip_if_not_installed("future")
  tar_config_set(store = "tmp")
  tar_script({
    future::plan(future::multisession)
    envir <- new.env(parent = globalenv())
    tar_target(x, writeLines(targets::tar_config_get("store"), "store.txt"))
  })
  tar_make_future()
  expect_equal(readLines("store.txt"), "tmp")
})
