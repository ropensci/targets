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
        identity
      },
      repository = "default"
    )
    tar_target(x, "value", format = format)
  })
  tar_make(callr_function = NULL)
  format <- tar_meta(x, format)$format
  format <- format_init(format)
  expect_silent(format_validate(format))
  expect_equal(format$class, "custom")
  expect_true(nzchar(format$read))
  expect_true(any(grepl("readRDS", format$read)))
  expect_true(nzchar(format$write))
  expect_true(any(grepl("saveRDS", format$write)))
  expect_true(nzchar(format$marshal))
  expect_true(any(grepl("identity", format$marshal)))
  expect_true(nzchar(format$unmarshal))
  expect_true(any(grepl("identity", format$unmarshal)))
  expect_equal(format$repository, "default")
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
      },
      repository = "default"
    )
    tar_target(x, "value", format = format)
  })
  tar_make(callr_function = NULL)
  expect_equal(tar_progress(x)$progress, "built")
  expect_equal(tar_read(x), "value")
})
