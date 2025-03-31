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

tar_test("tar_cas_l() (list)", {
  dir.create("cas")
  on.exit(unlink("cas", recursive = TRUE))
  expect_equal(tar_cas_l("cas", letters), character(0L))
  file.create(file.path("cas", "a"))
  file.create(file.path("cas", "b"))
  file.create(file.path("cas", "123"))
  expect_equal(sort(tar_cas_l("cas", letters)), sort(c("a", "b")))
})

tar_test("local CAS repository works on default directory", {
  skip_cran()
  for (consistent in c(TRUE, FALSE)) {
    tar_script({
      write_file <- function(object) {
        writeLines(as.character(object), "file.txt")
        "file.txt"
      }
      list(
        tar_target(x, c(2L, 4L)),
        tar_target(
          y,
          x,
          pattern = map(x)
        ),
        tar_target(z, write_file(y), format = "file")
      )
    })
    on.exit(tar_option_reset())
    for (consistent in c(TRUE, FALSE)) {
      repository <- tar_repository_cas_local(consistent = consistent)
      tar_option_set(repository = repository)
      for (storage in c("main", "worker")) {
        tar_option_set(storage = storage, retrieval = storage)
        tar_make(callr_function = NULL, reporter = "silent")
        expect_equal(tar_read(x), c(2L, 4L))
        expect_equal(unname(tar_read(y)), c(2L, 4L))
        expect_equal(unname(tar_read(y, branches = 2L)), 4L)
        expect_equal(readLines(tar_read(z)), c("2", "4"))
        expect_equal(
          tar_outdated(callr_function = NULL, reporter = "silent"),
          character(0L)
        )
        tar_make(callr_function = NULL, reporter = "silent")
        expect_equal(unique(tar_progress()$progress), "skipped")
        unlink(file.path(tar_config_get("store"), "cas", tar_meta(z)$data))
        expect_equal(
          tar_outdated(callr_function = NULL, reporter = "silent"),
          "z"
        )
        tar_make(callr_function = NULL, reporter = "silent")
        progress <- tar_progress()
        expect_equal(
          progress$progress,
          ifelse(progress$name == "z", "completed", "skipped")
        )
        tar_destroy()
      }
    }
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
  expect_equal(tar_outdated(reporter = "silent"), character(0L))
  unlink(file.path("cas", tar_meta(z)$data))
  expect_equal(tar_outdated(reporter = "silent"), "z")
  tar_destroy()
})

tar_test("local CAS repository with some invalidated branches", {
  skip_cran()
  tar_script({
    tar_option_set(repository = tar_repository_cas_local(path = "cas"))
    tar_option_set(memory = "transient")
    list(
      tar_target(x, seq_len(3)),
      tar_target(y, x, pattern = map(x)),
      tar_target(z, y, pattern = map(y)),
      tar_target(w, sum(y))
    )
  })
  tar_make(callr_function = NULL, reporter = "silent")
  tar_script({
    tar_option_set(repository = tar_repository_cas_local(path = "cas"))
    tar_option_set(memory = "transient")
    list(
      tar_target(x, c(1L, 5L, 3L)),
      tar_target(y, x, pattern = map(x)),
      tar_target(z, y, pattern = map(y)),
      tar_target(w, sum(y))
    )
  })
  tar_make(callr_function = NULL, reporter = "silent")
  expect_equal(tar_read(w), 9L)
})

tar_test("local CAS repository while depending on all branches", {
  skip_cran()
  tar_script({
    tar_option_set(repository = tar_repository_cas_local(path = "cas"))
    tar_option_set(memory = "transient")
    list(
      tar_target(x, seq_len(3)),
      tar_target(y, x, pattern = map(x)),
      tar_target(z, y)
    )
  })
  tar_make(callr_function = NULL, reporter = "silent")
  tar_script({
    tar_option_set(repository = tar_repository_cas_local(path = "cas"))
    tar_option_set(memory = "transient")
    list(
      tar_target(x, c(1L, 5L, 3L)),
      tar_target(y, x, pattern = map(x)),
      tar_target(z, y)
    )
  })
  tar_make(callr_function = NULL, reporter = "silent")
  expect_equal(as.integer(tar_read(z)), c(1L, 5L, 3L))
})
