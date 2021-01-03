tar_test("tar_visnetwork() does not create a data store", {
  tar_script({
    f <- identity
    tar_option_set()
    list(tar_target(x, f(1L)))
  })
  out <- tar_visnetwork(callr_function = NULL)
  expect_false(file.exists("_targets"))
})

tar_test("tar_visnetwork()", {
  tar_script({
    f <- identity
    tar_option_set()
    list(
      tar_target(y1, f(1)),
      tar_target(y2, 1 + 1),
      tar_target(z, y1 + y2)
    )
  })
  out <- tar_visnetwork(
    callr_function = NULL,
    callr_arguments = list(show = FALSE)
  )
  expect_true(inherits(out, "visNetwork"))
})

tar_test("tar_visnetwork() does not deduplicate metadata", {
  tar_script({
    tar_option_set(envir = new.env(parent = baseenv()))
    list(tar_target(x, 1L, cue = tar_cue(mode = "always")))
  })
  for (index in seq_len(2L)) {
    tar_make(callr_function = NULL)
  }
  out <- meta_init()$database$read_data()
  expect_equal(nrow(out), 2L)
  out <- tar_visnetwork(callr_arguments = list(show = FALSE))
  out <- meta_init()$database$read_data()
  expect_equal(nrow(out), 2L)
})
