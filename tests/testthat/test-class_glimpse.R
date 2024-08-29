tar_test("glimpse$targets_only", {
  skip_if_not_installed("visNetwork")
  net <- glimpse_init(pipeline_init(), targets_only = FALSE)
  expect_false(net$targets_only)
  net <- glimpse_init(pipeline_init(), targets_only = TRUE)
  expect_true(net$targets_only)
})

tar_test("glimpse$allow", {
  skip_if_not_installed("visNetwork")
  net <- glimpse_init(pipeline_init(), allow = "x")
  expect_equal(net$allow, "x")
})

tar_test("glimpse$exclude", {
  skip_if_not_installed("visNetwork")
  net <- glimpse_init(pipeline_init(), exclude = "x")
  expect_equal(net$exclude, "x")
})

tar_test("glimpse$pipeline", {
  expect_equal(
    class(glimpse_init(pipeline_init())$pipeline)[1],
    "tar_pipeline"
  )
})

tar_test("vertices and edges", {
  envir <- new.env(parent = baseenv())
  evalq({
    f <- function(x) g(x) + h
    g <- function(x) i
    h <- 1
    i <- 1
  }, envir = envir)
  tar_option_set(envir = envir)
  pipeline <- pipeline_init(
    list(
      target_init("x", quote(f(1))),
      target_init("y", quote(g(2))),
      target_init("z", quote(x + y), description = "info")
    )
  )
  vis <- glimpse_init(pipeline, targets_only = FALSE)
  vis$update()
  vertices <- vis$vertices
  vertices <- vertices[order(vertices$name), ]
  exp_imports <- data_frame(
    name = c("g", "f", "h", "i"),
    type = c("function", "function", "object", "object"),
    description = NA_character_,
    status = rep("none", 4L),
    seconds = NA_real_,
    bytes = NA_real_,
    branches = NA_real_
  )
  exp_targets <- data_frame(
    name = c("x", "y", "z"),
    type = rep("stem", 3L),
    description = c(rep(NA_character_, 2L), "info"),
    status = rep("none", 3L),
    seconds = NA_real_,
    bytes = NA_real_,
    branches = NA_real_
  )
  exp <- rbind(exp_imports, exp_targets)
  exp <- exp[order(exp$name), ]
  rownames(vertices) <- NULL
  rownames(exp) <- NULL
  expect_equal(vertices, exp)
  edges <- vis$edges
  edges <- edges[order(edges$from), ]
  exp_imports <- data_frame(from = c("g", "h", "i"), to = c("f", "f", "g"))
  exp_targets <- data_frame(
    from = c("f", "g", "x", "y"),
    to = c("x", "y", "z", "z")
  )
  exp <- rbind(exp_imports, exp_targets)
  exp <- exp[order(exp$from), ]
  rownames(edges) <- NULL
  rownames(exp) <- NULL
  expect_equal(edges, exp)
})

tar_test("glimpse$update() with allow", {
  skip_if_not_installed("visNetwork")
  x <- target_init("x", quote(1))
  y <- target_init("y", quote(x))
  pipeline <- pipeline_init(list(x, y))
  net <- glimpse_init(pipeline, allow = "x")
  net$update()
  vertices <- net$vertices
  exp <- data_frame(
    name = "x",
    type = "stem",
    status = "none",
    seconds = NA_real_,
    bytes = NA_real_,
    branches = NA_integer_
  )
  rownames(vertices) <- NULL
  rownames(exp) <- NULL
  expect_equal(vertices[, colnames(exp)], exp)
  edges <- net$edges
  exp <- data_frame(from = character(0), to = character(0))
  expect_equal(edges, exp)
})

tar_test("glimpse$update() with exclude", {
  skip_if_not_installed("visNetwork")
  x <- target_init("x", quote(1))
  y <- target_init("y", quote(x))
  pipeline <- pipeline_init(list(x, y))
  net <- glimpse_init(pipeline, exclude = "x")
  net$update()
  vertices <- net$vertices
  exp <- data_frame(
    name = "y",
    type = "stem",
    status = "none"
  )
  rownames(vertices) <- NULL
  rownames(exp) <- NULL
  expect_equal(vertices[, colnames(exp)], exp)
  edges <- net$edges
  exp <- data_frame(from = character(0), to = character(0))
  expect_equal(edges, exp)
})

tar_test("glimpse$update() with names", {
  skip_if_not_installed("visNetwork")
  x <- target_init("x", quote(1))
  y <- target_init("y", quote(x))
  z <- target_init("z", quote(y))
  pipeline <- pipeline_init(list(x, y, z))
  net <- glimpse_init(pipeline, names = "y", targets_only = TRUE)
  net$update()
  expect_equal(sort(net$vertices$name), sort(c("x", "y")))
  expect_equal(net$edges$from, "x")
  expect_equal(net$edges$to, "y")
})

tar_test("glimpse$update() with names and shortcut", {
  skip_if_not_installed("visNetwork")
  x <- target_init("x", quote(1))
  y <- target_init("y", quote(x))
  z <- target_init("z", quote(y))
  pipeline <- pipeline_init(list(x, y, z))
  local_init(pipeline)$run()
  net <- glimpse_init(
    pipeline,
    names = "y",
    targets_only = TRUE,
    shortcut = TRUE
  )
  net$update()
  expect_equal(net$vertices$name, "y")
  expect_equal(nrow(net$edges), 0L)
})

tar_test("glimpse$validate()", {
  expect_silent(glimpse_init(pipeline_init())$validate())
})

tar_test("glimpse$validate() with names, allow, and exclude", {
  x <- glimpse_init(
    pipeline_init(),
    names = "x",
    allow = "x",
    exclude = "y"
  )
  expect_silent(x$validate())
})
