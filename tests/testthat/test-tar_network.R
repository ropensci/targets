tar_test("tar_network() works", {
  tar_script(
    list(
      tar_target(y1, 1 + 1),
      tar_target(y2, 1 + 1, description = "y2 info"),
      tar_target(z, y1 + y2, description = "z info")
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
    description = c("z info", NA_character_, "y2 info"),
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

tar_test("allow", {
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
    allow = "z"
  )
  expect_equal(out$vertices$name, "z")
})

tar_test("exclude", {
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
    targets_only = TRUE,
    exclude = c("y1", "z")
  )
  expect_equal(out$vertices$name, "y2")
})

tar_test("names", {
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
    names = "y1",
    targets_only = TRUE
  )
  expect_equal(out$vertices$name, "y1")
})

tar_test("names and shortcut", {
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
  tar_make(callr_function = NULL)
  out <- tar_network(
    callr_function = NULL,
    callr_arguments = list(show = FALSE),
    names = "z",
    targets_only = TRUE,
    shortcut = TRUE
  )
  expect_equal(out$vertices$name, "z")
})

tar_test("custom script and store args", {
  skip_cran()
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
  expect_false(file.exists(path_script_default()))
  expect_false(file.exists(path_store_default()))
  expect_true(file.exists("example/script.R"))
  expect_false(file.exists("example/store"))
  tar_config_set(script = "x")
  expect_equal(tar_config_get("script"), "x")
  expect_true(file.exists("_targets.yaml"))
})

tar_test("custom script and store args with callr function", {
  skip_cran()
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
  expect_false(file.exists(path_script_default()))
  expect_false(file.exists(path_store_default()))
  expect_true(file.exists("example/script.R"))
  expect_false(file.exists("example/store"))
  tar_config_set(script = "x")
  expect_equal(tar_config_get("script"), "x")
  expect_true(file.exists("_targets.yaml"))
})
