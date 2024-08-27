tar_test("tar_cas_u() (upload)", {
  for (index in seq_len(2L)) {
    file.create("x")
    tar_cas_u("cas", "key", "x")
    expect_true(file.exists(file.path("cas", "key")))
  }
})

tar_test("tar_cas_d() (download)", {
  file.create("x")
  tar_cas_u("cas", "key", "x")
  tar_cas_d("cas", "key", "file")
  expect_true(file.exists("file"))
})

tar_test("tar_cas_e() (exists)", {
  tar_repository_cas_local_cache[["cas"]] <- NULL
  on.exit(tar_repository_cas_local_cache[["cas"]] <- NULL)
  expect_false(tar_cas_e("cas", "key"))
  file.create("x")
  tar_cas_u("cas", "key", "x")
  expect_true(tar_cas_e("cas", "key"))
  expect_true(tar_cas_e("cas", "key"))
  expect_false(tar_cas_e("cas", "key2"))
  file.create("x")
  tar_cas_u("cas", "key2", "x")
  expect_true(tar_cas_e("cas", "key2"))
})

tar_test("local CAS repository works on default directory", {
  skip_cran()
  tar_script({
    repository <- tar_repository_cas_local()
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
        repository = repository
      ),
      tar_target(z, write_file(y), format = "file", repository = repository)
    )
  })
  on.exit(tar_option_reset())
  for (storage in c("main", "worker")) {
    tar_option_set(storage = storage, retrieval = storage)
    tar_make(callr_function = NULL, reporter = "silent")
    expect_equal(tar_read(x), c(2L, 4L))
    expect_equal(unname(tar_read(y)), c(2L, 4L))
    expect_equal(unname(tar_read(y, branches = 2L)), 4L)
    expect_equal(readLines(tar_read(z)), c("2", "4"))
    expect_equal(tar_outdated(), character(0L))
    unlink(file.path(tar_config_get("store"), "cas", tar_meta(z)$data))
    expect_equal(tar_outdated(), "z")
    tar_destroy()
  }
})

tar_test("local CAS repository works on custom directory", {
  skip_cran()
  tar_script({
    repository <- tar_repository_cas_local(path = "cas")
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
        repository = repository
      ),
      tar_target(z, write_file(y), format = "file", repository = repository)
    )
  })
  tar_make(reporter = "silent")
  expect_equal(tar_read(x), c(2L, 4L))
  expect_equal(unname(tar_read(y)), c(2L, 4L))
  expect_equal(unname(tar_read(y, branches = 2L)), 4L)
  expect_equal(readLines(tar_read(z)), c("2", "4"))
  expect_equal(tar_outdated(), character(0L))
  unlink(file.path("cas", tar_meta(z)$data))
  expect_equal(tar_outdated(), "z")
  tar_destroy()
})
