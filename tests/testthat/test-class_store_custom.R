tar_test("class_store_custom rds", {
  tar_script({
    format <- tar_format(
      read = function(path) {
        readRDS(path)
      },
      write = function(object, path) {
        saveRDS(object = object, file = path, version = 3L)
      },
      marshal = function(object) {
        identity(object)
      },
      unmarshal = function(object) {
        identity(object)
      }
    )
    tar_target(x, "value", format = format)
  })
  tar_make(callr_function = NULL)
  format <- tar_meta(x, format)$format
  store <- store_init(format)
  expect_silent(store_validate(store))
  expect_true(nzchar(store$read))
  expect_true(any(grepl("readRDS", store$read)))
  expect_true(nzchar(store$write))
  expect_true(any(grepl("saveRDS", store$write)))
  expect_true(nzchar(store$marshal))
  expect_true(any(grepl("identity", store$marshal)))
  expect_true(nzchar(store$unmarshal))
  expect_true(any(grepl("identity", store$unmarshal)))
  expect_equal(tar_read(x), "value")
  tar_make(callr_function = NULL)
  expect_equal(tar_progress(x)$progress, "skipped")
  tar_script({
    format <- tar_format(
      read = function(path) {
        readRDS(c(path))
      },
      write = function(object, path) {
        saveRDS(object = object, file = path, version = 3L)
      },
      marshal = function(object) {
        identity(object)
      },
      unmarshal = function(object) {
        identity
      }
    )
    tar_target(x, "value", format = format)
  })
  tar_make(callr_function = NULL)
  expect_equal(tar_progress(x)$progress, "built")
  expect_equal(tar_read(x), "value")
})

tar_test("torch as custom format", {
  skip_on_cran()
  skip_on_os("solaris")
  skip_if_not_installed("future")
  skip_if_not_installed("future.callr")
  skip_if_not_installed("torch")
  tar_script({
    future::plan(future.callr::callr)
    format <- tar_format(
      read = function(path) {
        torch::torch_load(path)
      },
      write = function(object, path) {
        torch::torch_save(obj = object, path = path)
      },
      marshal = function(object) {
        con <- rawConnection(raw(), open = "wr")
        on.exit(close(con))
        torch::torch_save(object, con)
        rawConnectionValue(con)
      },
      unmarshal = function(object) {
        con <- rawConnection(object, open = "r")
        on.exit(close(con))
        torch::torch_load(con)
      }
    )
    tar_target(
      a,
      torch::torch_tensor(c(1, 2)),
      format = format,
      storage = "main",
      retrieval = "main"
    )
  })
  tar_make_future(callr_function = NULL)
  expect_equal(tar_outdated(callr_function = NULL), character(0))
  out <- tar_read(a)
  expect_true(inherits(out, "torch_tensor"))
  expect_equal(as.integer(sum(out)), 3L)
  format <- tar_meta(a, format)$format
  store <- store_init(format)
  expect_silent(store_validate(store))
  expect_true(nzchar(store$read))
  expect_true(any(grepl("torch_load", store$read)))
  expect_true(nzchar(store$write))
  expect_true(any(grepl("torch_save", store$write)))
  expect_true(nzchar(store$marshal))
  expect_true(any(grepl("torch_save", store$marshal)))
  expect_true(any(grepl("rawConnectionValue", store$marshal)))
  expect_true(nzchar(store$unmarshal))
  expect_true(any(grepl("torch_load", store$unmarshal)))
  expect_true(any(grepl("rawConnection", store$unmarshal)))
})

tar_test("deprecated: aws custom store is valid", {
  skip_if_not_installed("paws")
  expect_warning(
    format <- tar_format(
      read = function(path) {
        readRDS(c(path))
      },
      write = function(object, path) {
        saveRDS(object = object, file = path, version = 3L)
      },
      marshal = function(object) {
        identity(object)
      },
      unmarshal = function(object) {
        identity
      },
      repository = "aws"
    ),
    class = "tar_condition_deprecate"
  )
  x <- tar_target(x, "value", format = format)
  expect_silent(target_validate(x))
  expect_silent(store_validate(x$store))
  expect_true(inherits(x$store, "tar_aws"))
})
