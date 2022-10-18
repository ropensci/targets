tar_test("digest functions", {
  expect_silent(tar_assert_chr(digest_chr64("x")))
  expect_silent(tar_assert_scalar(digest_chr64("x")))
  expect_silent(tar_assert_chr(digest_obj64(0L)))
  expect_silent(tar_assert_scalar(digest_obj64(0L)))
  expect_error(digest_chr64(0L))
})

tar_test("produce_seed()", {
  on.exit(tar_option_reset())
  out <- list()
  out[[1]] <- produce_seed("x")
  out[[2]] <- produce_seed("x")
  out[[3]] <- produce_seed("y")
  tar_option_set(seed = tar_option_get("seed") + 1L)
  out[[4]] <- produce_seed("x")
  out[[5]] <- produce_seed("x")
  out[[6]] <- produce_seed("y")
  tar_option_set(seed = NA_integer_)
  out[[7]] <- produce_seed("x")
  out[[8]] <- produce_seed("x")
  out[[9]] <- produce_seed("y")
  expect_equal(out[[1]], out[[2]])
  expect_equal(out[[4]], out[[5]])
  expect_false(out[[1]] == out[[3]])
  expect_false(out[[1]] == out[[4]])
  for (index in seq_len(6)) {
    expect_true(is.integer(out[[index]]))
    expect_equal(length(out[[index]]), 1L)
    expect_equal(attributes(out[[index]]), NULL)
  }
  for (index in c(7, 8, 9)) {
    expect_true(anyNA(out[[index]]))
    expect_equal(length(out[[index]]), 1L)
    expect_equal(attributes(out[[index]]), NULL)
  }
})
