tar_test("tar_make_future() works", {
  skip_if_not_installed("future")
  tar_script(list(tar_target(x, "x")))
  tar_make_future(
    callr_arguments = list(show = FALSE),
    reporter = "silent"
  )
  expect_equal(tar_read(x), "x")
})

tar_test("tar_make_future() can use tidyselect", {
  skip_if_not_installed("future")
  tar_script(
    list(
      tar_target(y1, 1 + 1),
      tar_target(y2, 1 + 1),
      tar_target(z, y1 + y2)
    )
  )
  tar_make_future(
    names = starts_with("y"),
    reporter = "silent",
    callr_arguments = list(show = FALSE)
  )
  out <- sort(list.files(file.path("_targets", "objects")))
  expect_equal(out, sort(c("y1", "y2")))
})

tar_test("nontrivial globals with global environment", {
  skip_on_cran()
  skip_if_not_installed("future")
  skip_if_not_installed("future.callr")
  tar_script({
    future::plan(future.callr::callr)
    f <- function(x) {
      g(x) + 1L
    }
    g <- function(x) {
      x + 1L
    }
    list(
      tar_target(x, 1),
      tar_target(y, f(x))
    )
  })
  tar_make_future(
    reporter = "silent",
    callr_arguments = list(spinner = FALSE)
  )
  expect_equal(tar_read(y), 3L)
})

tar_test("nontrivial globals with non-global environment", {
  skip_on_cran()
  skip_if_not_installed("future")
  skip_if_not_installed("future.callr")
  tar_script({
    future::plan(future.callr::callr)
    envir <- new.env(parent = globalenv())
    evalq({
      f <- function(x) {
        g(x) + 1L
      }
      g <- function(x) {
        x + 1L
      }
    }, envir = envir)
    tar_option_set(envir = envir)
    list(
      tar_target(x, 1),
      tar_target(y, f(x))
    )
  })
  tar_make_future(
    reporter = "silent",
    callr_arguments = list(spinner = FALSE)
  )
  expect_equal(tar_read(y), 3L)
})
