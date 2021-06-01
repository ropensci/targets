tar_test("tar_network() works", {
  tar_script(
    list(
      tar_target(y1, 1 + 1),
      tar_target(y2, 1 + 1),
      tar_target(z, y1 + y2)
    )
  )
  out <- tar_network(
    callr_function = NULL,
    callr_arguments = list(show = FALSE),
    targets_only = TRUE
  )
  out$vertices <- out$vertices[order(out$vertices$name), ]
  rownames(out$vertices) <- NULL
  exp <- data_frame(
    name = c("z", "y1", "y2"),
    type = "stem",
    status = "outdated",
    seconds = NA_real_,
    bytes = NA_real_,
    children = NA_real_
  )
  exp <- exp[order(exp$name), ]
  rownames(exp) <- NULL
  expect_equiv(out$vertices, exp)
  out$edges <- out$edges[order(out$edges$from), ]
  rownames(out$edges) <- NULL
  exp <- data_frame(from = c("y1", "y2"), to = "z")
  exp <- exp[order(exp$from), ]
  rownames(exp) <- NULL
  expect_equiv(out$edges, exp)
})

tar_test("targets_only = FALSE", {
  tar_script({
    x <- 1L
    envir <- environment()
    tar_option_set(envir = envir)
    list(
      tar_target(y1, 1 + 1),
      tar_target(y2, 1 + 1),
      tar_target(z, y1 + y2)
    )
  })
  out <- tar_network(
    callr_function = NULL,
    callr_arguments = list(show = FALSE),
    targets_only = FALSE
  )
  expect_true("x" %in% out$vertices$name)
})

tar_test("custom script and store args", {
  skip_on_cran()
  expect_equal(tar_config_get("script"), path_script_default())
  expect_equal(tar_config_get("store"), path_store_default())
  tar_script(tar_target(x, "y"), script = "example/script.R")
  out <- tar_network(
    script = "example/script.R",
    store = "example/store",
    callr_function = NULL
  )
  expect_true(is.list(out))
  expect_false(file.exists("_targets.yaml"))
  expect_equal(tar_config_get("script"), path_script_default())
  expect_equal(tar_config_get("store"), path_store_default())
  expect_equal(path_script(), path_script_default())
  expect_equal(path_store(), path_store_default())
  expect_false(file.exists(path_script_default()))
  expect_false(file.exists(path_store_default()))
  expect_true(file.exists("example/script.R"))
  expect_false(file.exists("example/store"))
  tar_config_set(script = "x")
  expect_equal(tar_config_get("script"), "x")
  expect_true(file.exists("_targets.yaml"))
})

tar_test("custom script and store args with callr function", {
  skip_on_cran()
  expect_equal(tar_config_get("script"), path_script_default())
  expect_equal(tar_config_get("store"), path_store_default())
  tar_script(tar_target(x, "y"), script = "example/script.R")
  out <- tar_network(
    script = "example/script.R",
    store = "example/store"
  )
  expect_true(is.list(out))
  expect_false(file.exists("_targets.yaml"))
  expect_equal(tar_config_get("script"), path_script_default())
  expect_equal(tar_config_get("store"), path_store_default())
  expect_equal(path_script(), path_script_default())
  expect_equal(path_store(), path_store_default())
  expect_false(file.exists(path_script_default()))
  expect_false(file.exists(path_store_default()))
  expect_true(file.exists("example/script.R"))
  expect_false(file.exists("example/store"))
  tar_config_set(script = "x")
  expect_equal(tar_config_get("script"), "x")
  expect_true(file.exists("_targets.yaml"))
})
