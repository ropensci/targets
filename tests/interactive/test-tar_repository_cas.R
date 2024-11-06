tar_test("consistent CAS repository invokes methods at the right times", {
  tar_script({
    library(targets)
    repository <- tar_repository_cas(
      upload = function(key, path) {
        message(sprintf("upload('%s')", key))
        if (dir.exists(path)) {
          stop("This CAS repository does not support directory outputs.")
        }
        if (!file.exists("cas")) {
          dir.create("cas", recursive = TRUE)
        }
        file.rename(path, file.path("cas", key))
      },
      download = function(key, path) {
        message(sprintf("download('%s')", key))
        file.copy(file.path("cas", key), path)
      },
      exists = function(key) {
        message(sprintf("exists('%s')", key))
        Sys.sleep(2)
        file.exists(file.path("cas", key))
      },
      list = function(keys) {
        message("list()")
        Sys.sleep(2)
        keys[file.exists(file.path("cas", keys))]
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
  # Should be slow to list at first
  # but the rest should be fast.
  # list() should run once,
  # and upload() should run on every target.
  tar_make()
  # Only list() should be calledonce , and everything should be skipped.
  tar_make()
  # A target should rebuild, but exists() should not be called.
  # download() should be called.
  unlink(file.path("cas", list.files("cas")[1L]))
  tar_make()
  tar_destroy()
  unlink("cas", recursive = TRUE)
})

tar_test("non-consistent CAS repository invokes methods at the right times", {
  tar_script({
    library(targets)
    repository <- tar_repository_cas(
      upload = function(key, path) {
        message(sprintf("upload('%s')", key))
        if (dir.exists(path)) {
          stop("This CAS repository does not support directory outputs.")
        }
        if (!file.exists("cas")) {
          dir.create("cas", recursive = TRUE)
        }
        file.rename(path, file.path("cas", key))
      },
      download = function(key, path) {
        message(sprintf("download('%s')", key))
        file.copy(file.path("cas", key), path)
      },
      exists = function(key) {
        message(sprintf("exists('%s')", key))
        Sys.sleep(2)
        file.exists(file.path("cas", key))
      },
      list = function(keys) {
        message("list()")
        Sys.sleep(2)
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
  # Should be slow on every target.
  # list() should be called once, and
  # upload() and exists() should be called on each target.
  tar_make()
  # Only list() should be called once, and everything should be skipped.
  tar_make()
  # A target should rebuild, and exists() should be called on
  # that target just after upload.
  # download() should be called beforehand to get the dependencies.
  unlink(file.path("cas", list.files("cas")[1L]))
  tar_make()
  tar_destroy()
  unlink("cas", recursive = TRUE)
})
