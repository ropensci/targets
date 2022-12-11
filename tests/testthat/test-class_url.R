tar_test("dynamic urls work", {
  skip_cran()
  skip_if_not_installed("curl")
  skip_if_offline()
  url <- "https://httpbin.org/etag/test"
  skip_if(!url_exists(url))
  tar_script({
    list(
      tar_target(
        abc,
        rep("https://httpbin.org/etag/test", 2),
        format = "url"
      )
    )
  })
  tar_make(callr_function = NULL)
  exp <- tibble::tibble(
    name = "abc",
    type = "stem",
    parent = "abc",
    branches = 0L,
    progress = "built"
  )
  expect_equal(tar_progress(fields = NULL), exp)
  tar_make(callr_function = NULL)
  expect_equal(tar_progress()$progress, "skipped")
  meta <- tar_meta(abc)
  expect_equal(nchar(meta$data), 16)
  out <- meta$path[[1]]
  exp <- rep(url, 2)
  expect_equal(out, exp)
  expect_equal(tar_read(abc), exp)
  expect_false(file.exists(file.path("_targets", "objects", "abc")))
  expect_true(inherits(tar_timestamp(abc), "POSIXct"))
  expect_gt(tar_timestamp(abc), tar_timestamp(nope))
})

tar_test("dynamic urls in dynamic branches work", {
  skip_cran()
  skip_if_not_installed("curl")
  skip_if_offline()
  url <- "https://httpbin.org/etag/test"
  skip_if(!url_exists(url))
  tar_script({
    list(
      tar_target(x, 1),
      tar_target(
        abc,
        "https://httpbin.org/etag/test",
        format = "url",
        pattern = map(x)
      )
    )
  })
  tar_make(callr_function = NULL)
  branch <- tar_branch_names(abc, 1)
  expect_equal(tar_progress(fields = NULL)$progress, rep("built", 3))
  tar_make(callr_function = NULL)
  expect_equal(tar_progress(fields = NULL)$progress, rep("skipped", 3))
  meta <- tar_meta()
  meta <- meta[meta$name == branch, ]
  expect_equal(nchar(meta$data), 16)
  out <- meta$path[[1]]
  expect_equal(out, url)
  expect_equal(tar_read_raw(branch), url)
  expect_false(file.exists(file.path("_targets", "objects", "abc")))
  expect_false(file.exists(file.path("_targets", "objects", branch)))
  expect_true(inherits(tar_timestamp_raw(branch), "POSIXct"))
  expect_gt(tar_timestamp_raw(branch), tar_timestamp(nope))
})

tar_test("dynamic urls work from a custom data store", {
  skip_cran()
  skip_if_not_installed("curl")
  skip_if_offline()
  url <- "https://httpbin.org/etag/test"
  skip_if(!url_exists(url))
  tar_script({
    list(
      tar_target(
        abc,
        rep("https://httpbin.org/etag/test", 2),
        format = "url"
      )
    )
  })
  path <- tempfile()
  writeLines(paste("main:\n  store:", path), "_targets.yaml")
  tar_make(callr_function = NULL)
  expect_true(dir.exists(path))
  expect_false(file.exists(path_store_default()))
  exp <- tibble::tibble(
    name = "abc",
    type = "stem",
    parent = "abc",
    branches = 0L,
    progress = "built"
  )
  expect_equal(tar_progress(fields = NULL), exp)
  tar_make(callr_function = NULL)
  expect_equal(tar_progress()$progress, "skipped")
  meta <- tar_meta(abc)
  expect_equal(nchar(meta$data), 16)
  out <- meta$path[[1]]
  exp <- rep(url, 2)
  expect_equal(out, exp)
  expect_equal(tar_read(abc), exp)
  expect_false(file.exists(file.path(path, "objects", "abc")))
  expect_false(file.exists(file.path(path_store_default(), "objects", "abc")))
  expect_true(dir.exists(path))
  expect_false(file.exists(path_store_default()))
  # Move the data store and verify that the targets are still up to date.
  unlink("_targets.yaml")
  file.rename(path, path_store_default())
  expect_false(dir.exists(path))
  expect_true(file.exists(path_store_default()))
  expect_equal(tar_outdated(callr_function = NULL), character(0))
  tar_make(callr_function = NULL)
  expect_equal(unique(tar_progress()$progress), "skipped")
})

tar_test("tar_condition_run error on bad URL", {
  skip_cran()
  skip_if_not_installed("curl")
  skip_if_offline()
  tar_script(tar_target(abc, "https://httpbin.org/status/404", format = "url"))
  expect_error(tar_make(callr_function = NULL), class = "tar_condition_run")
})

tar_test("custom handle without error (unstructured resources)", {
  skip_cran()
  skip_if_not_installed("curl")
  skip_if_offline()
  url <- "https://httpbin.org/etag/test"
  skip_if(!url_exists(url))
  tar_script({
    list(
      tar_target(
        abc,
        rep("https://httpbin.org/etag/test", 2),
        format = "url",
        resources = list(handle = curl::new_handle())
      )
    )
  })
  expect_warning(
    tar_target(
      abc,
      rep("https://httpbin.org/etag/test", 2),
      format = "url",
      resources = list(handle = curl::new_handle())
    ),
    class = "tar_condition_deprecate"
  )
  suppressWarnings(tar_make(callr_function = NULL))
  expect_equal(tar_read(abc), rep("https://httpbin.org/etag/test", 2))
  expect_false(file.exists(file.path("_targets", "objects", "abc")))
})

tar_test("dynamic urls must return characters", {
  skip_cran()
  skip_if_not_installed("curl")
  skip_if_offline()
  x <- target_init(
    name = "abc",
    expr = quote(list(list("illegal"))),
    format = "url"
  )
  pipeline <- pipeline_init(list(x))
  local <- local_init(pipeline = pipeline)
  expect_error(local$run(), class = "tar_condition_run")
})

tar_test("url target gets custom curl handle (structured resources)", {
  skip_cran()
  skip_if_not_installed("curl")
  x <- tar_target_raw(
    name = "abc",
    command = quote(list(list("illegal"))),
    format = "url",
    resources = tar_resources(
      url = tar_resources_url(handle = curl::new_handle())
    )
  )
  handle <- x$store$resources$url$handle
  expect_true(inherits(handle, "curl_handle"))
})

tar_test("url target gets custom curl handle (unstructured resources)", {
  skip_cran()
  skip_if_not_installed("curl")
  expect_warning(
    x <- tar_target_raw(
      name = "abc",
      command = quote(list(list("illegal"))),
      format = "url",
      resources = list(handle = curl::new_handle())
    ),
    class = "tar_condition_deprecate"
  )
  handle <- x$store$resources$handle
  expect_true(inherits(handle, "curl_handle"))
})

tar_test("bad curl handle throws an error (structrued resources)", {
  skip_cran()
  skip_if_not_installed("curl")
  skip_if_offline()
  url <- "https://httpbin.org/etag/test"
  skip_if(!url_exists(url))
  tar_script({
    list(
      tar_target(
        abc,
        rep("https://httpbin.org/etag/test", 2),
        format = "url",
        resources = tar_resources(
          url = tar_resources_url(handle = "invalid")
        )
      )
    )
  })
  expect_error(
    tar_make(callr_function = NULL),
    class = "tar_condition_run"
  )
})

tar_test("bad curl handle throws an error (unstructrued resources)", {
  skip_cran()
  skip_if_not_installed("curl")
  skip_if_offline()
  url <- "https://httpbin.org/etag/test"
  skip_if(!url_exists(url))
  tar_script({
    list(
      tar_target(
        abc,
        rep("https://httpbin.org/etag/test", 2),
        format = "url",
        resources = list(handle = "invalid")
      )
    )
  })
  expect_warning(
    tar_target(
      abc,
      rep("https://httpbin.org/etag/test", 2),
      format = "url",
      resources = list(handle = "invalid")
    ),
    class = "tar_condition_deprecate"
  )
  suppressWarnings(
    expect_error(
      tar_make(callr_function = NULL),
      class = "tar_condition_run"
    )
  )
})

tar_test("inherits from tar_external", {
  store <- tar_target(x, "x_value", format = "url")$store
  expect_true(inherits(store, "tar_external"))
})

tar_test("store_row_path()", {
  store <- tar_target(x, "x_value", format = "url")$store
  store$file$path <- "path"
  expect_equal(store_row_path(store), "path")
})

tar_test("store_path_from_record()", {
  store <- tar_target(x, "x_value", format = "url")$store
  record <- record_init(path = "path", format = "url")
  expect_equal(
    store_path_from_record(store, record, path_store_default()),
    "path"
  )
})

tar_test("url packages", {
  x <- tar_target(x, 1, format = "url")
  out <- store_get_packages(x$store)
  expect_equal(out, "curl")
})

tar_test("validate url format", {
  skip_if_not_installed("curl")
  tar_script(list(tar_target(x, "x_value", format = "url")))
  expect_silent(tar_validate(callr_function = NULL))
})

tar_test("url and NULL", {
  skip_cran()
  tar_script(tar_target(x, NULL, format = "url", memory = "persistent"))
  tar_make(callr_function = NULL)
  expect_equal(tar_read(x), character(0))
})
