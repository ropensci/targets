tar_test("tar_seed_set()", {
  out <- list()
  tar_seed_set(0L)
  out[[1L]] <- runif(n = 1L)
  tar_seed_set(NA)
  out[[2L]] <- runif(n = 1L)
  tar_seed_set(NULL)
  out[[3L]] <- runif(n = 1L)
  tar_seed_set(2L)
  out[[4L]] <- runif(n = 1L)
  tar_seed_set(2L)
  out[[5L]] <- runif(n = 1L)
  tar_seed_set(3L)
  out[[6L]] <- runif(n = 1L)
  expect_equal(out[[4L]], out[[5L]])
  out[[5L]] <- NULL
  for (i in seq_len(5L)) {
    for (j in seq_len(5L)) {
      if (i != j) {
        expect_false(out[[i]] == out[[j]])
      }
    }
  }
})
