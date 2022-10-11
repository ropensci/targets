tar_test("torch format", {
  skip_cran()
  skip_on_os("solaris")
  skip_torch()
  tar_script({
    list(tar_target(a, torch::torch_tensor(c(1, 2)), format = "torch"))
  })
  tar_make(callr_function = NULL)
  out <- tar_read(a)
  expect_true(inherits(out, "torch_tensor"))
  expect_equal(torch::as_array(out), c(1, 2))
})

tar_test("torch format with in-memory serialization", {
  skip_cran()
  skip_on_os("solaris")
  skip_if_not_installed("future")
  skip_torch()
  future::plan(future::sequential)
  tar_script({
    list(tar_target(a, torch::torch_tensor(c(1, 2)), format = "torch"))
  })
  tar_make_future(callr_function = NULL)
  out <- tar_read(a)
  expect_true(inherits(out, "torch_tensor"))
  expect_equal(torch::as_array(out), c(1, 2))
})

tar_test("torch in-memory serialization of deps", {
  skip_cran()
  skip_on_os("solaris")
  skip_on_os("windows")
  require_clustermq()
  skip_on_covr()
  skip_torch()
  tar_script({
    tar_option_set(packages = "torch", retrieval = "main")
    options(clustermq.scheduler = "multiprocess")
    list(
      tar_target(tensor, torch_zeros(10), format = "torch"),
      tar_target(array, as.array(tensor))
    )
  })
  # https://github.com/mschubert/clustermq/issues/269
  suppressWarnings(
    capture.output(
      tar_make_clustermq(reporter = "silent", callr_function = NULL)
    )
  )
  tar_load(tensor)
  expect_true(inherits(tensor, "torch_tensor"))
  expect_equal(length(tensor), 10)
  tar_load(array)
  expect_equal(array, rep(0, 10))
})

tar_test("validate torch format", {
  skip_cran()
  skip_on_os("solaris")
  skip_torch()
  x <- target_init(name = "a", expr = quote(f()), format = "torch")
  expect_silent(target_validate(x))
})

tar_test("torch packages", {
  x <- tar_target(x, 1, format = "torch")
  out <- store_get_packages(x$store)
  expect_equal(out, "torch")
})

tar_test("does not inherit from tar_external", {
  store <- tar_target(x, "x_value", format = "torch")$store
  expect_false(inherits(store, "tar_external"))
})

tar_test("store_row_path()", {
  store <- tar_target(x, "x_value", format = "torch")$store
  store$file$path <- "path"
  expect_equal(store_row_path(store), NA_character_)
})

tar_test("store_path_from_record()", {
  store <- tar_target(x, "x_value", format = "torch")$store
  record <- record_init(name = "x", path = "path", format = "torch")
  expect_equal(
    store_path_from_record(store, record, path_store_default()),
    path_objects(path_store_default(), "x")
  )
})
