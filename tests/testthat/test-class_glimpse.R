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
  pipeline <- pipeline_init(
    list(
      target_init("x", quote(f(1)), envir = envir),
      target_init("y", quote(g(2)), envir = envir),
      target_init("z", quote(x + y), envir = envir)
    )
  )
  vis <- glimpse_init(pipeline)
  vis$update()
  vertices <- vis$vertices
  vertices <- vertices[order(vertices$name), ]
  exp_imports <- data_frame(
    name = c("g", "f", "h", "i"),
    type = c("function", "function", "object", "object"),
    status = rep("none", 4L),
    seconds = NA_real_,
    bytes = NA_real_,
    branches = NA_real_
  )
  exp_targets <- data_frame(
    name = c("x", "y", "z"),
    type = rep("stem", 3L),
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

tar_test("glimpse$validate()", {
  expect_silent(glimpse_init(pipeline_init())$validate())
})
