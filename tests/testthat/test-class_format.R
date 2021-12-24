tar_test("uncomplicated format", {
  out <- format_init("rds")
  expect_silent(format_validate(out))
})

tar_test("custom format", {
  format <- tar_format(
    read = function(path) {
      readRDS(path) # nocov
    },
    write = function(object, path) {
      saveRDS(object = object, file = path, version = 3L) # nocov
    },
    marshal = function(object) {
      identity(object) # nocov
    },
    unmarshal = function(object) {
      identity # nocov
    },
    repository = "default"
  )
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
})
