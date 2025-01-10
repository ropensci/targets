tar_test("graph$produce_upstream()", {
  edges <- pipeline_upstream_edges(pipeline_order(), targets_only = TRUE)
  graph <- graph_init(remove_loops(edges))
  expect_equal(graph$produce_upstream("data1"), character(0))
  expect_equal(graph$produce_upstream("data2"), character(0))
  expect_equal(graph$produce_upstream("min1"), "data1")
  expect_equal(graph$produce_upstream("min2"), "data2")
  expect_equal(graph$produce_upstream("max1"), "data1")
  expect_equal(graph$produce_upstream("max2"), "data2")
  expect_equal(sort(graph$produce_upstream("mins")), sort(c("min1", "min2")))
  expect_equal(sort(graph$produce_upstream("maxes")), sort(c("max1", "max2")))
  expect_equal(sort(graph$produce_upstream("all")), sort(c("mins", "maxes")))
})

tar_test("graph$produce_downstream()", {
  edges <- pipeline_upstream_edges(pipeline_order(), targets_only = TRUE)
  graph <- graph_init(remove_loops(edges))
  expect_equal(
    sort(graph$produce_downstream("data1")),
    sort(c("min1", "max1"))
  )
  expect_equal(
    sort(graph$produce_downstream("data2")),
    sort(c("min2", "max2"))
  )
  expect_equal(graph$produce_downstream("min1"), "mins")
  expect_equal(graph$produce_downstream("min2"), "mins")
  expect_equal(graph$produce_downstream("max1"), "maxes")
  expect_equal(graph$produce_downstream("max2"), "maxes")
  expect_equal(graph$produce_downstream("mins"), "all")
  expect_equal(graph$produce_downstream("maxes"), "all")
  expect_equal(graph$produce_downstream("all"), character(0))
})

tar_test("graph$produce_degrees_upstream()", {
  edges <- pipeline_upstream_edges(pipeline_order(), targets_only = TRUE)
  graph <- graph_init(remove_loops(edges))
  expect_equal(graph$produce_degrees_upstream("data1"), 0L)
  expect_equal(graph$produce_degrees_upstream("data2"), 0L)
  expect_equal(graph$produce_degrees_upstream("min1"), 1L)
  expect_equal(graph$produce_degrees_upstream("min2"), 1L)
  expect_equal(graph$produce_degrees_upstream("max1"), 1L)
  expect_equal(graph$produce_degrees_upstream("max2"), 1L)
  expect_equal(graph$produce_degrees_upstream("mins"), 2L)
  expect_equal(graph$produce_degrees_upstream("maxes"), 2L)
  expect_equal(graph$produce_degrees_upstream("all"), 2L)
  expect_equal(
    graph$produce_degrees_upstream(c("all", "data1")),
    c(2L, 0L)
  )
})

tar_test("graph$produce_degrees_downstream()", {
  edges <- pipeline_upstream_edges(pipeline_order(), targets_only = TRUE)
  graph <- graph_init(remove_loops(edges))
  expect_equal(graph$produce_degrees_downstream("data1"), 2L)
  expect_equal(graph$produce_degrees_downstream("data2"), 2L)
  expect_equal(graph$produce_degrees_downstream("min1"), 1L)
  expect_equal(graph$produce_degrees_downstream("min2"), 1L)
  expect_equal(graph$produce_degrees_downstream("max1"), 1L)
  expect_equal(graph$produce_degrees_downstream("max2"), 1L)
  expect_equal(graph$produce_degrees_downstream("mins"), 1L)
  expect_equal(graph$produce_degrees_downstream("maxes"), 1L)
  expect_equal(graph$produce_degrees_downstream("all"), 0L)
  expect_equal(
    graph$produce_degrees_downstream(c("all", "data1")),
    c(0L, 2L)
  )
})

tar_test("graph$insert_edges() upstream checks", {
  edges <- pipeline_upstream_edges(pipeline_order(), targets_only = TRUE)
  graph <- graph_init(remove_loops(edges))
  new_edgelist <- data_frame(
    from = c("abc", "xyz", "min1", "other1", "one_more"),
    to = c("data1", "data2", "123", "other2", "all")
  )
  graph$insert_edges(new_edgelist)
  upstream <- graph$upstream
  expect_null(upstream[["abc"]])
  expect_null(upstream[["xyz"]])
  expect_null(upstream[["other1"]])
  expect_equal(upstream[["123"]], "min1")
  expect_equal(upstream[["other2"]], "other1")
  expect_equal(upstream[["data1"]], "abc")
  expect_equal(upstream[["data2"]], "xyz")
  expect_equal(upstream[["min1"]], "data1")
  expect_equal(upstream[["min2"]], "data2")
  expect_equal(upstream[["max1"]], "data1")
  expect_equal(upstream[["max2"]], "data2")
  expect_equal(sort(upstream[["mins"]]), sort(c("min1", "min2")))
  expect_equal(sort(upstream[["maxes"]]), sort(c("max1", "max2")))
  expect_equal(sort(upstream[["all"]]), sort(c("mins", "maxes", "one_more")))
})

tar_test("graph$insert_edges() downstream checks", {
  edges <- pipeline_upstream_edges(pipeline_order(), targets_only = TRUE)
  graph <- graph_init(remove_loops(edges))
  new_edgelist <- data_frame(
    from = c("abc", "xyz", "min1", "other1"),
    to = c("data1", "data2", "123", "other2")
  )
  graph$insert_edges(new_edgelist)
  downstream <- graph$downstream
  expect_equal(downstream[["other1"]], "other2")
  expect_null(downstream[["other2"]])
  expect_null(downstream[["123"]])
  expect_equal(downstream[["abc"]], "data1")
  expect_equal(downstream[["xyz"]], "data2")
  expect_equal(sort(downstream[["min1"]]), sort(c("mins", "123")))
  expect_equal(sort(downstream[["data1"]]), sort(c("min1", "max1")))
  expect_equal(sort(downstream[["data2"]]), sort(c("min2", "max2")))
  expect_equal(sort(downstream[["min1"]]), sort(c("123", "mins")))
  expect_equal(downstream[["min2"]], "mins")
  expect_equal(downstream[["max1"]], "maxes")
  expect_equal(downstream[["max2"]], "maxes")
  expect_equal(downstream[["mins"]], "all")
  expect_equal(downstream[["maxes"]], "all")
  expect_null(downstream[["all"]])
})

tar_test("graph$replace_upstream()", {
  graph <- graph_init()
  new_edgelist <- data_frame(
    from = c("abc", "xyz", "123"),
    to = c("data1", "data1", "data2")
  )
  graph$insert_edges(new_edgelist)
  expect_equal(sort(graph$upstream[["data1"]]), sort(c("abc", "xyz")))
  graph$replace_upstream("data1", "xyz", "789")
  expect_equal(sort(graph$upstream[["data1"]]), sort(c("abc", "789")))
})

tar_test("graph$validate()", {
  edges <- pipeline_upstream_edges(pipeline_order(), targets_only = TRUE)
  graph <- graph_init(remove_loops(edges))
  expect_silent(graph$validate())
})
