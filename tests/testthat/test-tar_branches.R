tar_test("tar_branches() with a nontrivial pattern", {
  skip_cran()
  tar_script(
    list(
      tar_target(x, seq_len(2)),
      tar_target(y, head(letters, 2)),
      tar_target(z, head(LETTERS, 2)),
      tar_target(dynamic, c(x, y, z), pattern = cross(z, map(x, y)))
    )
  )
  tar_make(callr_function = NULL)
  out <- tar_branches(dynamic, pattern = cross(z, map(x, y)))
  expect_equal(dim(out), c(4L, 4L))
  expect_true(all(c("dynamic", "z", "x", "y") %in% colnames(out)))
  exp <- tar_meta(dynamic, children)$children[[1]]
  expect_equal(out$dynamic, exp)
  children <- tar_meta(x, children)$children[[1]]
  exp <- rep(children, times = 2)
  expect_equal(out$x, exp)
  children <- tar_meta(y, children)$children[[1]]
  exp <- rep(children, times = 2)
  expect_equal(out$y, exp)
  children <- tar_meta(z, children)$children[[1]]
  exp <- rep(children, each = 2)
  expect_equal(out$z, exp)
})

tar_test("tar_branches() with a random pattern", {
  skip_cran()
  tar_script(
    list(
      tar_target(w, letters),
      tar_target(x, w, pattern = map(w)),
      tar_target(dynamic, x, pattern = sample(x, 10))
    )
  )
  tar_make(callr_function = NULL)
  out <- tar_branches(dynamic, pattern = sample(x, 10))
  expect_equal(dim(out), c(10L, 2L))
  expect_true(all(c("dynamic", "x") %in% colnames(out)))
  exp <- tar_meta(dynamic, children)$children[[1]]
  expect_equal(out$dynamic, exp)
  for (i in seq_len(nrow(out))) {
    dynamic <- tar_read_raw(out$dynamic[i])
    x <- tar_read_raw(out$x[i])
    expect_equal(dynamic, x)
  }
})

tar_test("custom script and store args", {
  skip_cran()
  expect_equal(tar_config_get("script"), path_script_default())
  expect_equal(tar_config_get("store"), path_store_default())
  tar_script({
    list(
      tar_target(w, letters),
      tar_target(x, w, pattern = map(w)),
      tar_target(dynamic, x, pattern = sample(x, 10))
    )
  }, script = "example/script.R")
  try(
    tar_make(
      callr_function = NULL,
      script = "example/script.R",
      store = "example/store"
    ),
    silent = TRUE
  )
  out <- tar_branches(
    dynamic,
    pattern = sample(x, 10),
    store = "example/store"
  )
  expect_true(is.data.frame(out))
  expect_false(file.exists("_targets.yaml"))
  expect_equal(tar_config_get("script"), path_script_default())
  expect_equal(tar_config_get("store"), path_store_default())
  expect_false(file.exists(path_script_default()))
  expect_false(file.exists(path_store_default()))
  expect_true(file.exists("example/script.R"))
  expect_true(file.exists("example/store"))
  expect_true(file.exists("example/store/meta/meta"))
  expect_true(file.exists("example/store/objects/w"))
  tar_config_set(script = "x")
  expect_equal(tar_config_get("script"), "x")
  expect_true(file.exists("_targets.yaml"))
})
