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
    name = character(0),
    type = character(0),
    status = character(0),
    seconds = numeric(0),
    bytes = numeric(0),
    branches = integer(0)
  )
  expect_equal(vertices, exp)
  edges <- vis$edges_imports
  exp <- data_frame(from = character(0), to = character(0))
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
    "w|built",
    "x|started",
    "y|canceled",
    "z|errored"
  )
  dir_create(dirname(path_progress()))
  writeLines(lines, path_progress())
  vis <- inspection_init(pipeline)
  vis$update()
  vertices <- vis$vertices_targets
  vertices <- vertices[order(vertices$name), ]
  exp <- data_frame(
    name = c("w", "x", "y", "z"),
    type = rep("stem", 4L),
    status = c("outdated", "started", "canceled", "errored")
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
    "w|built",
    "x|started",
    "y|canceled",
    "z|errored"
  )
  dir_create(dirname(path_progress()))
  writeLines(lines, path_progress())
  vis <- inspection_init(pipeline, outdated = FALSE)
  vis$update()
  vertices <- vis$vertices_targets
  vertices <- vertices[order(vertices$name), ]
  exp <- data_frame(
    name = c("w", "x", "y", "z"),
    type = rep("stem", 4L),
    status = c("built", "started", "canceled", "errored")
  )
  exp <- exp[order(exp$name), ]
  rownames(vertices) <- NULL
  rownames(exp) <- NULL
  expect_equal(vertices[, colnames(exp)], exp)
})

tar_test("inspection$validate()", {
  expect_silent(inspection_init(pipeline_init())$validate())
})
