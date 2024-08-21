tar_test("tar_repository_cas() generates a format string", {
  out <- tar_repository_cas(
    upload = function(path, key) {
      file.move(path, file.path("cas", key))
    },
    download = function(path, key) {
      file.copy(file.path("cas", key), path)
    },
    exists = function(key) {
      file.exists(file.path("cas", key))
    }
  )
  expect_equal(length(out), 1)
  out <- unlist(strsplit(out, split = "&", fixed = TRUE))
  expect_equal(out[1], "repository_cas")
  expect_true(any(grepl("^upload=+.", out)))
  expect_true(any(grepl("^download=+.", out)))
  expect_true(any(grepl("^exists=+.", out)))
})
