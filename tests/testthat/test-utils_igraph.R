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

tar_test("leaves()", {
  igraph <- pipeline_produce_igraph(pipeline_order(), targets_only = TRUE)
  out <- leaves(igraph)
  expect_equal(sort(out), sort(c("data1", "data2")))
})
