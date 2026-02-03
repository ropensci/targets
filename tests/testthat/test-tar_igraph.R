tar_test("tar_igraph() with targets_only = FALSE", {
  skip_cran()
  tar_script(
    {
      a <- 1
      list(
        tar_target(x, a),
        tar_target(y, 2),
        tar_target(z, x + y)
      )
    },
    ask = FALSE
  )
  graph <- tar_igraph(targets_only = FALSE)
  out <- igraph::as_data_frame(graph)
  out <- out[order(out$from, out$to), ]
  out <- out[out$from != "+", ]
  rownames(out) <- NULL
  expected <- data.frame(
    from = c("a", "x", "y"),
    to = c("x", "z", "z"),
    stringsAsFactors = FALSE
  )
  expect_equal(out, expected)
})

tar_test("tar_igraph() with targets_only = TRUE", {
  skip_cran()
  tar_script(
    {
      a <- 1
      list(
        tar_target(x, a),
        tar_target(y, 2),
        tar_target(z, x + y)
      )
    },
    ask = FALSE
  )
  graph <- tar_igraph(targets_only = TRUE)
  out <- igraph::as_data_frame(graph)
  out <- out[order(out$from, out$to), ]
  out <- out[out$from != "+", ]
  rownames(out) <- NULL
  expected <- data.frame(
    from = c("x", "y"),
    to = c("z", "z"),
    stringsAsFactors = FALSE
  )
  expect_equal(out, expected)
})
