tar_test("inspection$targets_only", {
  skip_if_not_installed("visNetwork")
  net <- inspection_init(pipeline_init(), targets_only = FALSE)
  expect_equal(net$targets_only, FALSE)
  net <- inspection_init(pipeline_init(), targets_only = TRUE)
  expect_equal(net$targets_only, TRUE)
})

tar_test("inspection$allow", {
  skip_if_not_installed("visNetwork")
  net <- inspection_init(pipeline_init(), allow = "x")
  expect_equal(net$allow, "x")
})

tar_test("inspection$exclude", {
  skip_if_not_installed("visNetwork")
  net <- inspection_init(pipeline_init(), exclude = "x")
  expect_equal(net$exclude, "x")
})

tar_test("inspection$pipeline", {
  expect_equal(
    class(inspection_init(pipeline_init())$pipeline)[1],
    "tar_pipeline"
  )
})

tar_test("inspection$meta", {
  inspection <- inspection_init(pipeline_init(), meta_init())
  expect_silent(inspection$meta$validate())
})

tar_test("inspection$progress", {
  inspection <- inspection_init(pipeline_init(), progress_init())
  expect_silent(inspection$progress$validate())
})

tar_test("inspection$outdated", {
  out <- inspection_init(pipeline_init(), outdated = FALSE)
  expect_equal(out$outdated, FALSE)
})

tar_test("vertices and edges of empty imports", {
  envir <- new.env(parent = baseenv())
  tar_option_set(envir = envir)
  pipeline <- pipeline_init(
    list(
      target_init("x", quote(f(1))),
      target_init("y", quote(g(2))),
      target_init("z", quote(x + y))
    )
  )
  vis <- inspection_init(pipeline)
  vis$update()
  vertices <- vis$vertices_imports
  exp <- data_frame(
    name = character(0L),
    type = character(0L),
    description = character(0L),
    status = character(0L),
    seconds = numeric(0L),
    bytes = numeric(0L),
    branches = integer(0L)
  )
  expect_equal(vertices, exp)
  edges <- vis$edges_imports
  exp <- data_frame(from = character(0L), to = character(0L))
  expect_equal(edges, exp)
})

tar_test("vertices and edges of nonempty imports", {
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
      target_init("z", quote(x + y))
    )
  )
  local_init(pipeline)$run()
  evalq(g <- function(x) i + 1L, envir = envir)
  tar_option_set(envir = envir)
  pipeline <- pipeline_init(
    list(
      target_init("x", quote(f(1))),
      target_init("y", quote(g(2))),
      target_init("z", quote(x + y))
    )
  )
  vis <- inspection_init(pipeline)
  vis$update()
  vertices <- vis$vertices_imports
  vertices <- vertices[order(vertices$name), ]
  exp <- data_frame(
    name = c("g", "f", "h", "i"),
    type = c("function", "function", "object", "object"),
    status = c("outdated", "outdated", "uptodate", "uptodate")
  )
  exp <- exp[order(exp$name), ]
  rownames(vertices) <- NULL
  rownames(exp) <- NULL
  expect_equal(vertices[, colnames(exp)], exp)
  edges <- vis$edges_imports
  exp <- data_frame(from = c("g", "h", "i"), to = c("f", "f", "g"))
  exp <- exp[order(exp$from), ]
  rownames(edges) <- NULL
  rownames(exp) <- NULL
  expect_equal(edges, exp)
})

tar_test("same for targets", {
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
      target_init("w", quote(1)),
      target_init("x", quote(f(1))),
      target_init("y", quote(g(2))),
      target_init("z", quote(x + y))
    )
  )
  local_init(pipeline)$run()
  evalq(g <- function(x) i + 1L, envir = envir)
  pipeline <- pipeline_init(
    list(
      target_init("w", quote(1)),
      target_init("x", quote(f(1))),
      target_init("y", quote(g(2))),
      target_init("z", quote(x + y))
    )
  )
  vis <- inspection_init(pipeline)
  vis$update()
  vertices <- vis$vertices_targets
  vertices <- vertices[order(vertices$name), ]
  exp <- data_frame(
    name = c("w", "x", "y", "z"),
    type = rep("stem", 4L),
    status = c("uptodate", "outdated", "outdated", "outdated")
  )
  exp <- exp[order(exp$name), ]
  rownames(vertices) <- NULL
  rownames(exp) <- NULL
  expect_equal(vertices[, colnames(exp)], exp)
  expect_false(any(is.na(vertices$seconds)))
  expect_false(any(is.na(vertices$bytes)))
  expect_true(is.numeric(vertices$seconds))
  expect_true(is.numeric(vertices$bytes))
  expect_true(all(vertices$seconds >= 0))
  expect_true(all(vertices$bytes > 0))
  expect_true(all(is.na(vertices$children)))
  edges <- vis$edges_targets
  exp <- data_frame(from = c("f", "g", "x", "y"), to = c("x", "y", "z", "z"))
  exp <- exp[order(exp$from), ]
  rownames(edges) <- NULL
  rownames(exp) <- NULL
  expect_equal(edges, exp)
})

tar_test("branches get counted in network data", {
  envir <- new.env(parent = baseenv())
  tar_option_set(envir = envir)
  pipeline <- pipeline_init(
    list(
      target_init("w", quote(seq_len(3))),
      target_init("x", quote(w), pattern = quote(map(w)))
    )
  )
  local_init(pipeline)$run()
  evalq(g <- function(x) i + 1L, envir = envir)
  pipeline <- pipeline_init(
    list(
      target_init("w", quote(seq_len(3))),
      target_init("x", quote(w), pattern = quote(map(w)))
    )
  )
  vis <- inspection_init(pipeline)
  vis$update()
  vertices <- vis$vertices_targets
  expect_false(anyNA(vertices$branches[vertices$name == "x"]))
  expect_equal(vertices$branches[vertices$name == "x"], 3L)
})

tar_test("targets and imports bound together", {
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
      target_init("z", quote(x + y))
    )
  )
  local_init(pipeline)$run()
  evalq(g <- function(x) i + 1L, envir = envir)
  pipeline <- pipeline_init(
    list(
      target_init("x", quote(f(1))),
      target_init("y", quote(g(2))),
      target_init("z", quote(x + y))
    )
  )
  vis <- inspection_init(pipeline)
  vis$update()
  vertices <- vis$vertices
  vertices <- vertices[order(vertices$name), ]
  exp_imports <- data_frame(
    name = c("g", "f", "h", "i"),
    type = c("function", "function", "object", "object"),
    status = c("outdated", "outdated", "uptodate", "uptodate")
  )
  exp_targets <- data_frame(
    name = c("x", "y", "z"),
    type = rep("stem", 3L),
    status = rep("outdated", 3L)
  )
  exp <- rbind(exp_imports, exp_targets)
  exp <- exp[order(exp$name), ]
  rownames(vertices) <- NULL
  rownames(exp) <- NULL
  expect_equal(vertices[, colnames(exp)], exp)
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

tar_test("target vertices get progress", {
  envir <- new.env(parent = baseenv())
  tar_option_set(envir = envir)
  pipeline <- pipeline_init(
    list(
      target_init("w", quote(1)),
      target_init("x", quote(1)),
      target_init("y", quote(1)),
      target_init("z", quote(x + y))
    )
  )
  lines <- c(
    "name|progress",
    "w|completed",
    "x|dispatched",
    "y|canceled",
    "z|errored"
  )
  dir_create(dirname(path_progress(path_store_default())))
  writeLines(lines, path_progress(path_store_default()))
  vis <- inspection_init(pipeline)
  vis$update()
  vertices <- vis$vertices_targets
  vertices <- vertices[order(vertices$name), ]
  exp <- data_frame(
    name = c("w", "x", "y", "z"),
    type = rep("stem", 4L),
    status = c("outdated", "dispatched", "canceled", "errored")
  )
  exp <- exp[order(exp$name), ]
  rownames(vertices) <- NULL
  rownames(exp) <- NULL
  expect_equal(vertices[, colnames(exp)], exp)
})

tar_test("turn outdated off", {
  envir <- new.env(parent = baseenv())
  tar_option_set(envir = envir)
  pipeline <- pipeline_init(
    list(
      target_init("w", quote(1)),
      target_init("x", quote(1)),
      target_init("y", quote(1)),
      target_init("z", quote(x + y))
    )
  )
  lines <- c(
    "name|progress",
    "w|completed",
    "x|dispatched",
    "y|canceled",
    "z|errored"
  )
  dir_create(dirname(path_progress(path_store_default())))
  writeLines(lines, path_progress(path_store_default()))
  vis <- inspection_init(pipeline, outdated = FALSE)
  vis$update()
  vertices <- vis$vertices_targets
  vertices <- vertices[order(vertices$name), ]
  exp <- data_frame(
    name = c("w", "x", "y", "z"),
    type = rep("stem", 4L),
    status = c("completed", "dispatched", "canceled", "errored")
  )
  exp <- exp[order(exp$name), ]
  rownames(vertices) <- NULL
  rownames(exp) <- NULL
  expect_equal(vertices[, colnames(exp)], exp)
})

tar_test("inspection$update() with allow", {
  skip_if_not_installed("visNetwork")
  x <- target_init("x", quote(1))
  y <- target_init("y", quote(x))
  pipeline <- pipeline_init(list(x, y))
  net <- inspection_init(pipeline, allow = "x")
  net$update()
  vertices <- net$vertices
  exp <- data_frame(
    name = "x",
    type = "stem",
    status = "outdated",
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

tar_test("inspection$update() with names", {
  skip_if_not_installed("visNetwork")
  x <- target_init("x", quote(1))
  y <- target_init("y", quote(x))
  z <- target_init("z", quote(y))
  pipeline <- pipeline_init(list(x, y, z))
  net <- inspection_init(pipeline, names = "y", targets_only = TRUE)
  net$update()
  expect_equal(sort(net$vertices$name), sort(c("x", "y")))
  expect_equal(net$edges$from, "x")
  expect_equal(net$edges$to, "y")
})

tar_test("inspection$update() descriptions", {
  skip_if_not_installed("visNetwork")
  x <- target_init("x", quote(1), description = "x info")
  y <- target_init("y", quote(x), description = "y info")
  z <- target_init("z", quote(y))
  pipeline <- pipeline_init(list(x, y, z))
  net <- inspection_init(pipeline, targets_only = TRUE)
  net$update()
  expect_equal(net$vertices$description[net$vertices$name == "x"], "x info")
  expect_equal(net$vertices$description[net$vertices$name == "y"], "y info")
})

tar_test("inspection$update() with names and shortcut", {
  skip_if_not_installed("visNetwork")
  x <- target_init("x", quote(1))
  y <- target_init("y", quote(x))
  z <- target_init("z", quote(y))
  pipeline <- pipeline_init(list(x, y, z))
  local_init(pipeline)$run()
  net <- inspection_init(
    pipeline,
    names = "y",
    targets_only = TRUE,
    shortcut = TRUE
  )
  net$update()
  expect_equal(net$vertices$name, "y")
  expect_equal(nrow(net$edges), 0L)
})

tar_test("inspection$update() with exclude", {
  skip_if_not_installed("visNetwork")
  x <- target_init("x", quote(1))
  y <- target_init("y", quote(x))
  pipeline <- pipeline_init(list(x, y))
  net <- inspection_init(pipeline, exclude = "x")
  net$update()
  vertices <- net$vertices
  exp <- data_frame(
    name = "y",
    type = "stem",
    status = "outdated"
  )
  rownames(vertices) <- NULL
  rownames(exp) <- NULL
  expect_equal(vertices[, colnames(exp)], exp)
  edges <- net$edges
  exp <- data_frame(from = character(0), to = character(0))
  expect_equal(edges, exp)
})

tar_test("inspection$validate()", {
  expect_silent(inspection_init(pipeline_init())$validate())
})
