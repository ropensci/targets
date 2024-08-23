tar_test("tar_repository_cas() generates an encoded string", {
  out <- tar_repository_cas(
    upload = function(path, key) {
      file.rename(path, file.path("cas", key))
    },
    download = function(path, key) {
      file.copy(file.path("cas", key), path)
    },
    exists = function(key) {
      file.exists(file.path("cas", key))
    },
    consistent = TRUE
  )
  expect_equal(length(out), 1)
  out <- unlist(strsplit(out, split = "&", fixed = TRUE))
  expect_equal(out[1], "repository_cas")
  expect_true(any(grepl("^upload=+.", out)))
  expect_true(any(grepl("^download=+.", out)))
  expect_true(any(grepl("^exists=+.", out)))
  expect_true(any(grepl("^consistent=+.", out)))
})

tar_test("CAS repository works", {
  skip_if_not_installed("qs")
  tar_script({
    repository <- tar_repository_cas(
      upload = function(path, key) {
        fs::dir_create("cas", recurse = TRUE)
        file.rename(path, file.path("cas", key))
      },
      download = function(path, key) {
        file.copy(file.path("cas", key), path)
      },
      exists = function(key) {
        file.exists(file.path("cas", key))
      },
      consistent = TRUE
    )
    write_file <- function(object) {
      writeLines(as.character(object), "file.txt")
      "file.txt"
    }
    list(
      tar_target(x, c(2L, 4L), repository = repository),
      tar_target(
        y,
        x,
        pattern = map(x),
        format = "qs",
        repository = repository
      ),
      tar_target(z, write_file(y), format = "file", repository = repository)
    )
  })
  tar_make(callr_function = NULL)
  expect_equal(tar_read(x), c(2L, 4L))
  expect_equal(unname(tar_read(y)), c(2L, 4L))
  expect_equal(unname(tar_read(y, branches = 2L)), 4L)
  expect_equal(readLines(tar_read(z)), c("2", "4"))
  expect_equal(tar_outdated(callr_function = NULL), character(0L))
  unlink(file.path("cas", tar_meta(z)$data))
  expect_equal(tar_outdated(callr_function = NULL), "z")
  tar_destroy()
})

tar_test("CAS repository works", {
  tar_script({
    repository <- tar_repository_cas(
      upload = function(path, key) {
        fs::dir_create("cas", recurse = TRUE)
        file.rename(path, file.path("cas", key))
      },
      download = function(path, key) {
        file.copy(file.path("cas", key), path)
      },
      exists = function(key) {
        file.exists(file.path("cas", key))
      },
      consistent = TRUE
    )
    list(
      tar_target(x, TRUE, repository = repository),
      tar_target(y, x, repository = repository)
    )
  })
  tar_make(callr_function = NULL)
  tar_invalidate(y)
  tar_make(callr_function = NULL)
  
  
})

tar_test("custom format + CAS repository", {
  skip_cran()
  tar_script({
    format <- tar_format(
      read = function(path) {
        readLines(path)
      },
      write = function(object, path) {
        writeLines(as.character(object), path)
      }
    )
    repository <- tar_repository_cas(
      upload = function(path, key) {
        fs::dir_create("cas", recurse = TRUE)
        file.rename(path, file.path("cas", key))
      },
      download = function(path, key) {
        file.copy(file.path("cas", key), path)
      },
      exists = function(key) {
        file.exists(file.path("cas", key))
      },
      consistent = TRUE
    )
    write_file <- function(object) {
      writeLines(as.character(object), "file.txt")
      "file.txt"
    }
    list(
      tar_target(x, 1L + 1L, format = format, repository = repository)
    )
  })
  tar_make(callr_function = NULL)
  expect_equal(tar_read(x), "2")
})

tar_test("revert and appear up to date", {
  skip_cran()
  tar_script({
    repository <- tar_repository_cas(
      upload = function(path, key) {
        fs::dir_create("cas", recurse = TRUE)
        file.rename(path, file.path("cas", key))
      },
      download = function(path, key) {
        file.copy(file.path("cas", key), path)
      },
      exists = function(key) {
        file.exists(file.path("cas", key))
      },
      consistent = TRUE
    )
    write_file <- function(object) {
      writeLines(as.character(object), "file.txt")
      "file.txt"
    }
    list(
      tar_target(x, "contents", repository = repository)
    )
  })
  tar_make(callr_function = NULL)
  file.copy("_targets/meta/meta", "first_meta")
  tar_script({
    repository <- tar_repository_cas(
      upload = function(path, key) {
        fs::dir_create("cas", recurse = TRUE)
        file.rename(path, file.path("cas", key))
      },
      download = function(path, key) {
        file.copy(file.path("cas", key), path)
      },
      exists = function(key) {
        file.exists(file.path("cas", key))
      },
      consistent = TRUE
    )
    write_file <- function(object) {
      writeLines(as.character(object), "file.txt")
      "file.txt"
    }
    list(
      tar_target(x, "contents2", repository = repository)
    )
  })
  tar_make(callr_function = NULL)
  tar_script({
    repository <- tar_repository_cas(
      upload = function(path, key) {
        fs::dir_create("cas", recurse = TRUE)
        file.rename(path, file.path("cas", key))
      },
      download = function(path, key) {
        file.copy(file.path("cas", key), path)
      },
      exists = function(key) {
        file.exists(file.path("cas", key))
      },
      consistent = TRUE
    )
    write_file <- function(object) {
      writeLines(as.character(object), "file.txt")
      "file.txt"
    }
    list(
      tar_target(x, "contents", repository = repository)
    )
  })
  expect_equal(tar_outdated(callr_function = NULL), "x")
  file.copy("first_meta", "_targets/meta/meta", overwrite = TRUE)
  expect_equal(tar_outdated(callr_function = NULL), character(0L))
})
