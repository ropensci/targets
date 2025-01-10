tar_test("tar_repository_cas() generates an encoded string", {
  out <- tar_repository_cas(
    upload = function(key, path) {
      file.copy(path, file.path("cas", key))
    },
    download = function(key, path) {
      file.copy(file.path("cas", key), path)
    },
    exists = function(key) {
      file.exists(file.path("cas", key))
    },
    list = function(keys) {
      keys[file.exists(file.path("cas", keys))]
    },
    consistent = TRUE
  )
  expect_equal(length(out), 1)
  out <- unlist(strsplit(out, split = "&", fixed = TRUE))
  expect_equal(out[1], "repository_cas")
  expect_true(any(grepl("^upload=+.", out)))
  expect_true(any(grepl("^download=+.", out)))
  expect_true(any(grepl("^exists=*.", out)))
  expect_true(any(grepl("^list=+.", out)))
  expect_true(any(grepl("^consistent=+.", out)))
})

tar_test("tar_repository_cas() keeps 'exists' at the right times", {
  out <- tar_repository_cas(
    upload = function(key, path) {
      file.copy(path, file.path("cas", key))
    },
    download = function(key, path) {
      file.copy(file.path("cas", key), path)
    },
    exists = function(key) {
      file.exists(file.path("cas", key))
    },
    list = function(keys) {
      keys[file.exists(file.path("cas", keys))]
    },
    consistent = TRUE
  )
  out <- unlist(strsplit(out, split = "&", fixed = TRUE))
  expect_true(any(out == "exists="))
  out <- tar_repository_cas(
    upload = function(key, path) {
      file.copy(path, file.path("cas", key))
    },
    download = function(key, path) {
      file.copy(file.path("cas", key), path)
    },
    exists = function(key) {
      file.exists(file.path("cas", key))
    },
    list = function(keys) {
      keys[file.exists(file.path("cas", keys))]
    },
    consistent = FALSE
  )
  out <- unlist(strsplit(out, split = "&", fixed = TRUE))
  exists <- base64url::base64_urldecode(
    gsub("^exists=", "", out[grepl("^exists=+.", out)])
  )
  expect_true(nzchar(exists))
  expect_false(any(out == "exists="))
  for (consistent in c(TRUE, FALSE)) {
    out <- tar_repository_cas(
      upload = function(key, path) {
        file.copy(path, file.path("cas", key))
      },
      download = function(key, path) {
        file.copy(file.path("cas", key), path)
      },
      exists = function(key) {
        file.exists(file.path("cas", key))
      },
      consistent = consistent
    )
    out <- unlist(strsplit(out, split = "&", fixed = TRUE))
    exists <- base64url::base64_urldecode(
      gsub("^exists=", "", out[grepl("^exists=+.", out)])
    )
    expect_true(nzchar(exists))
    expect_false(any(out == "exists="))
  }
})

tar_test("validate CAS repository class", {
  repository <- tar_repository_cas(
    upload = function(key, path) {
      file.copy(path, file.path("cas", key))
    },
    download = function(key, path) {
      file.copy(file.path("cas", key), path)
    },
    exists = function(key) {
      file.exists(file.path("cas", key))
    },
    consistent = TRUE
  )
  target <- tar_target(x, 1, repository = repository)
  expect_silent(store_validate(target$store))
})

tar_test("CAS repository works", {
  skip_if_not_installed("qs2")
  tar_script({
    repository <- tar_repository_cas(
      upload = function(key, path) {
        if (!file.exists("cas")) {
          dir.create("cas", recursive = TRUE)
        }
        if (dir.exists(path)) {
          stop("This CAS repository does not support directory outputs.")
        }
        file.copy(path, file.path("cas", key))
      },
      download = function(key, path) {
        file.copy(file.path("cas", key), path)
      },
      exists = function(key) {
        file.exists(file.path("cas", key))
      },
      list = function(keys) {
        keys[file.exists(file.path("cas", keys))]
      },
      consistent = FALSE
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

tar_test("CAS repository works with parallel workers", {
  skip_cran()
  skip_if_not_installed("crew")
  skip_if_not_installed("qs2")
  tar_script({
    repository <- tar_repository_cas(
      upload = function(key, path) {
        if (!file.exists("cas")) {
          dir.create("cas", recursive = TRUE)
        }
        if (dir.exists(path)) {
          stop("This CAS repository does not support directory outputs.")
        }
        file.copy(path, file.path("cas", key))
      },
      download = function(key, path) {
        file.copy(file.path("cas", key), path)
      },
      exists = function(key) {
        file.exists(file.path("cas", key))
      },
      list = function(keys) {
        keys[file.exists(file.path("cas", keys))]
      },
      consistent = FALSE
    )
    write_file <- function(object) {
      writeLines(as.character(object), "file.txt")
      "file.txt"
    }
    tar_option_set(
      controller = crew::crew_controller_local(),
      storage = "worker",
      retrieval = "worker"
    )
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

tar_test("CAS repository works without list method", {
  skip_cran()
  skip_if_not_installed("qs2")
  tar_script({
    repository <- tar_repository_cas(
      upload = function(key, path) {
        if (!file.exists("cas")) {
          dir.create("cas", recursive = TRUE)
        }
        if (dir.exists(path)) {
          stop("This CAS repository does not support directory outputs.")
        }
        file.copy(path, file.path("cas", key))
      },
      download = function(key, path) {
        file.copy(file.path("cas", key), path)
      },
      exists = function(key) {
        file.exists(file.path("cas", key))
      },
      consistent = FALSE
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

tar_test("CAS repository works with transient memory and files", {
  tar_script({
    tar_option_set(memory = "transient")
    repository <- tar_repository_cas(
      upload = function(key, path) {
        if (!file.exists("cas")) {
          dir.create("cas", recursive = TRUE)
        }
        if (dir.exists(path)) {
          stop("This CAS repository does not support directory outputs.")
        }
        file.rename(path, file.path("cas", key))
      },
      download = function(key, path) {
        file.copy(file.path("cas", key), path)
      },
      exists = function(key) {
        file.exists(file.path("cas", key))
      },
      list = function(keys) {
        keys[file.exists(file.path("cas", keys))]
      },
      consistent = FALSE
    )
    list(
      tar_target(x, 1L, repository = repository),
      tar_target(y, x + 1L, repository = repository),
      tar_target(z, y + 1L, repository = repository),
      tar_target(
        a, {
          saveRDS(z, "file_a.rds")
          "file_a.rds"
        },
        repository = repository,
        format = "file"
      ),
      tar_target(
        b, {
          saveRDS(readRDS(a), "file_b.rds")
          "file_b.rds"
        },
        repository = repository,
        format = "file"
      ),
      tar_target(
        c,
        readRDS(b),
        repository = repository
      )
    )
  })
  tar_make(callr_function = NULL)
  expect_equal(tar_read(z), 3L)
  expect_equal(readRDS(tar_read(a)), 3L)
  expect_equal(readRDS(tar_read(b)), 3L)
  expect_equal(tar_read(c), 3L)
  expect_equal(tar_outdated(callr_function = NULL), character(0L))
  tar_invalidate(y)
  expect_equal(
    sort(tar_outdated(callr_function = NULL)),
    sort(c("a", "b", "c", "y", "z"))
  )
})

tar_test("CAS repository works with custom envvars", {
  tar_script({
    tar_option_set(memory = "transient")
    repository <- tar_repository_cas(
      upload = function(key, path) {
        if (!file.exists("cas")) {
          dir.create("cas", recursive = TRUE)
        }
        if (dir.exists(path)) {
          stop("This CAS repository does not support directory outputs.")
        }
        writeLines(Sys.getenv("TARGETS_TEST_CUSTOM_ENVVAR"), "envvar.txt")
        file.copy(path, file.path("cas", key))
      },
      download = function(key, path) {
        file.copy(file.path("cas", key), path)
      },
      exists = function(key) {
        file.exists(file.path("cas", key))
      },
      list = function(keys) {
        keys[file.exists(file.path("cas", keys))]
      },
      consistent = FALSE
    )
    resources <- tar_resources(
      repository_cas = tar_resources_repository_cas(
        envvars = c(TARGETS_TEST_CUSTOM_ENVVAR = "abcdefg")
      )
    )
    list(
      tar_target(x, 1L, repository = repository, resources = resources)
    )
  })
  Sys.unsetenv("TARGETS_TEST_CUSTOM_ENVVAR")
  out <- tar_make(callr_function = NULL, reporter = "silent")
  expect_equal(readLines("envvar.txt"), "abcdefg")
  Sys.unsetenv("TARGETS_TEST_CUSTOM_ENVVAR")
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
      upload = function(key, path) {
        if (!file.exists("cas")) {
          dir.create("cas", recursive = TRUE)
        }
        if (dir.exists(path)) {
          stop("This CAS repository does not support directory outputs.")
        }
        file.copy(path, file.path("cas", key))
      },
      download = function(key, path) {
        file.copy(file.path("cas", key), path)
      },
      exists = function(key) {
        file.exists(file.path("cas", key))
      },
      list = function(keys) {
        keys[file.exists(file.path("cas", keys))]
      },
      consistent = FALSE
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
      upload = function(key, path) {
        if (!file.exists("cas")) {
          dir.create("cas", recursive = TRUE)
        }
        if (dir.exists(path)) {
          stop("This CAS repository does not support directory outputs.")
        }
        file.copy(path, file.path("cas", key))
      },
      download = function(key, path) {
        file.copy(file.path("cas", key), path)
      },
      exists = function(key) {
        file.exists(file.path("cas", key))
      },
      list = function(keys) {
        keys[file.exists(file.path("cas", keys))]
      },
      consistent = FALSE
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
      upload = function(key, path) {
        if (!file.exists("cas")) {
          dir.create("cas", recursive = TRUE)
        }
        if (dir.exists(path)) {
          stop("This CAS repository does not support directory outputs.")
        }
        file.copy(path, file.path("cas", key))
      },
      download = function(key, path) {
        file.copy(file.path("cas", key), path)
      },
      exists = function(key) {
        file.exists(file.path("cas", key))
      },
      list = function(keys) {
        keys[file.exists(file.path("cas", keys))]
      },
      consistent = FALSE
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
      upload = function(key, path) {
        if (!file.exists("cas")) {
          dir.create("cas", recursive = TRUE)
        }
        if (dir.exists(path)) {
          stop("This CAS repository does not support directory outputs.")
        }
        file.copy(path, file.path("cas", key))
      },
      download = function(key, path) {
        file.copy(file.path("cas", key), path)
      },
      exists = function(key) {
        file.exists(file.path("cas", key))
      },
      list = function(keys) {
        keys[file.exists(file.path("cas", keys))]
      },
      consistent = FALSE
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
