tar_test("mermaid$update_network()", {
  envir <- new.env(parent = baseenv())
  envir$a <- 1L
  tar_option_set(envir = envir)
  x <- target_init("x", quote(a))
  pipeline <- pipeline_init(list(x))
  local_init(pipeline)$run()
  x <- target_init("x", quote(a))
  pipeline <- pipeline_init(list(x))
  net <- inspection_init(pipeline)
  vis <- mermaid_init(network = net)
  vis$update_network()
  vertices <- vis$network$vertices
  vertices <- vertices[order(vertices$name), ]
  rownames(vertices) <- NULL
  exp <- data_frame(
    name = c("a", "x"),
    type = c("object", "stem"),
    status = rep("uptodate", 2L)
  )
  exp <- exp[order(exp$name), ]
  rownames(exp) <- NULL
  expect_equal(vertices[, colnames(exp)], exp)
  edges <- vis$network$edges
  exp <- data_frame(from = "a", to = "x")
  rownames(edges) <- NULL
  rownames(exp) <- NULL
  expect_equal(edges, exp)
})

tar_test("mermaid$update_labels()", {
  net <- glimpse_init(pipeline_order())
  vis <- mermaid_init(network = net)
  vis$update_network()
  vis$update_labels()
  vertices <- vis$network$vertices
  expect_true(is.character(vertices$name))
  expect_true(is.character(vertices$label))
  expect_equal(vertices$name, vertices$label)
})

tar_test("mermaid$update_colors()", {
  net <- glimpse_init(pipeline_order())
  vis <- mermaid_init(network = net)
  vis$update_network()
  vis$update_colors()
  vertices <- vis$network$vertices
  expect_true("color" %in% colnames(vertices))
})

tar_test("mermaid$update_colors() on cross plan", {
  net <- glimpse_init(pipeline_cross())
  vis <- mermaid_init(network = net)
  vis$update()
  vertices <- vis$network$vertices
  expect_true("color" %in% colnames(vertices))
})

tar_test("mermaid$update_legend() on cross plan", {
  net <- glimpse_init(pipeline_cross())
  vis <- mermaid_init(network = net)
  vis$update()
  expect_silent(vis$validate())
  legend <- vis$legend
  exp <- data_frame(
    name = c("pattern", "stem"),
    open = c("[", "(["),
    close = c("]", "])"),
    status = rep("none", 2),
    label = c("\"Pattern\"", "\"Stem\"")
  )
  cols <- colnames(legend)
  legend <- legend[order(legend$label), cols]
  exp <- exp[order(exp$label), cols]
  expect_equiv(legend, exp)
})

tar_test("mermaid$update() on cross pipeline", {
  net <- glimpse_init(pipeline_cross())
  vis <- mermaid_init(network = net)
  vis$update()
  expect_silent(vis$validate())
  mermaid <- vis$visual
  expect_true(is.character(mermaid))
  expect_true(all(nzchar(mermaid)))
})

tar_test("mermaid$update() on empty pipeline", {
  net <- glimpse_init(pipeline_init())
  vis <- mermaid_init(network = net)
  vis$update()
  expect_silent(vis$validate())
  mermaid <- vis$visual
  expect_true(is.character(mermaid))
  expect_equal(mermaid, "")
})

tar_test("mermaid$update() on edgeless pipeline", {
  net <- glimpse_init(pipeline_init(list(target_init("x", quote(1)))))
  vis <- mermaid_init(network = net)
  vis$update()
  expect_silent(vis$validate())
  mermaid <- vis$visual
  expect_true(is.character(mermaid))
  expect_true(all(nzchar(mermaid)))
})

tar_test("mermaid$validate() with no allow or exclude", {
  net <- glimpse_init(pipeline_init())
  vis <- mermaid_init(network = net)
  expect_silent(vis$validate())
})

tar_test("mermaid$validate() with allow and exclude", {
  net <- glimpse_init(pipeline_init(), allow = "x", exclude = "y")
  vis <- mermaid_init(network = net)
  expect_silent(vis$validate())
})

tar_test("mermaid$validate() with label", {
  pipeline <- pipeline_map()
  local_init(pipeline = pipeline, reporter = "silent")$run()
  net <- inspection_init(pipeline_map())
  vis <- mermaid_init(network = net, label = c("time", "size", "branches"))
  vis$update()
  expect_true(is.character(vis$visual))
})
