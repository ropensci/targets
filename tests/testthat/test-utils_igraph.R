tar_test("upstream_vertices() goes far upstream", {
  igraph <- pipeline_produce_igraph(pipeline_order(), targets_only = TRUE)
  out <- upstream_vertices(igraph, "mins")
  exp <- c("mins", "min1", "min2", "data1", "data2")
  expect_equal(sort(out), sort(exp))
})

tar_test("upstream_vertices() can handle multiple vertices", {
  igraph <- pipeline_produce_igraph(pipeline_order(), targets_only = TRUE)
  out <- upstream_vertices(igraph, c("min1", "max2"))
  exp <- c("min1", "max2", "data1", "data2")
  expect_equal(sort(out), sort(exp))
})

tar_test("igraph_leaves()", {
  igraph <- pipeline_produce_igraph(pipeline_order(), targets_only = TRUE)
  out <- igraph_leaves(igraph)
  expect_equal(sort(out), sort(c("data1", "data2")))
})

tar_test("topo_sort_by_priority()", {
  pipeline <- pipeline_init(
    list(
      target_init("x", quote(1), priority = 0.5),
      target_init("y1", quote(x), priority = 0.2),
      target_init("y2", quote(x), priority = 0.1),
      target_init("y3", quote(x), priority = 0.3),
      target_init("z", quote(c(y1, y2, y3)), priority = 1)
    )
  )
  igraph <- pipeline_produce_igraph(pipeline)
  priorities <- pipeline_get_priorities(pipeline)
  out <- topo_sort_by_priority(igraph, priorities)
  exp <- c("x", "y3", "y1", "y2", "z")
  expect_equal(out, exp)
})

tar_test("topo_sort_by_priority() on a different test case", {
  pipeline <- pipeline_init(
    list(
      target_init("x", quote(1), priority = 1),
      target_init("y1", quote(x), priority = 0.1),
      target_init("y2", quote(x), priority = 0.2),
      target_init("y3", quote(x), priority = 0.3),
      target_init("z", quote(c(y1, y2, y3)), priority = 0)
    )
  )
  igraph <- pipeline_produce_igraph(pipeline)
  priorities <- pipeline_get_priorities(pipeline)
  out <- topo_sort_by_priority(igraph, priorities)
  exp <- c("x", "y3", "y2", "y1", "z")
  expect_equal(out, exp)
})
