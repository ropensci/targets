tar_test("cross pattern initializes correctly", {
  x <- target_init("x", expr = quote(1 + 1), pattern = quote(cross(a, b)))
  expect_true(inherits(x, "tar_cross"))
})

tar_test("target_get_parent(cross)", {
  x <- target_init("x", expr = quote(1 + 1), pattern = quote(cross(a, b)))
  expect_equal(target_get_parent(x), "x")
})

tar_test("cross$sitrep", {
  x <- target_init("x", expr = quote(1 + 1), pattern = quote(cross(a, b)))
  expect_silent(sitrep_validate(x$sitrep))
})

tar_test("crosses produce correct junctions and bud niblings", {
  pipeline <- pipeline_cross()
  local <- algorithm_init("local", pipeline)
  scheduler <- local$scheduler
  local$ensure_meta()
  local$process_target("data1")
  local$process_target("data2")
  cross2 <- pipeline_get_target(pipeline, "cross2")
  splits <- target_produce_junction(cross2, pipeline)$splits
  expect_true(all(grepl("cross2_", splits)))
  target <- pipeline_get_target(pipeline, "cross2")
  out <- target_produce_junction(target, pipeline)$deps
  expect_equal(dim(out), c(9L, 2L))
  expect_true(all(grepl("data1_", out$data1)))
  expect_true(all(grepl("data2_", out$data2)))
  buds <- map_int(seq_len(9), function(index) {
    bud <- pipeline_get_target(pipeline, out$data1[index])
    target_load_value(bud, pipeline)
    bud$value$object
  })
  expect_equal(buds, rep(seq_len(3), each = 3))
  buds <- map_int(seq_len(9), function(index) {
    bud <- pipeline_get_target(pipeline, out$data2[index])
    target_load_value(bud, pipeline)
    pipeline_get_target(pipeline, out$data2[index])$value$object
  })
  expect_equal(buds, rep(rev(seq_len(3)), times = 3))
})

tar_test("correct junction of non-crossed stems", {
  pipeline <- pipeline_cross()
  local <- algorithm_init("local", pipeline)
  scheduler <- local$scheduler
  local$ensure_meta()
  local$process_target("data1")
  local$process_target("data2")
  cross2 <- pipeline_get_target(pipeline, "cross2")
  splits <- target_produce_junction(cross2, pipeline)$splits
  expect_true(all(grepl("cross2_", splits)))
  target <- pipeline_get_target(pipeline, "cross2")
  out <- target_produce_junction(target, pipeline)$deps
  expect_equal(dim(out), c(9L, 2L))
  expect_true(all(grepl("data1_", out$data1)))
  expect_true(all(grepl("data2_", out$data2)))
  exp <- expand_grid(data1 = unique(out$data1), data2 = unique(out$data2))
  expect_equal(out, exp)
})

tar_test("cross pipeline gives correct values", {
  pipeline <- pipeline_cross()
  local <- algorithm_init("local", pipeline)
  scheduler <- local$scheduler
  local$run()
  value <- function(name) {
    target_read_value(pipeline_get_target(pipeline, name))$object
  }
  expect_equal(value("data1"), seq_len(3L))
  expect_equal(value("data2"), rev(seq_len(3L)))
  branches <- target_get_children(pipeline_get_target(pipeline, "map1"))
  children <- target_get_children(pipeline_get_target(pipeline, "map1"))
  out <- map_int(children, value)
  expect_equivalent(out, seq(11, 13))
  children <- target_get_children(pipeline_get_target(pipeline, "cross1"))
  out1 <- map_int(children, value)
  exp1 <- seq(2, 4)
  expect_equivalent(out1, exp1)
  children <- target_get_children(pipeline_get_target(pipeline, "cross2"))
  out2 <- map_int(children, value)
  exp2 <- rowSums(expand_grid(x = seq_len(3L), y = rev(seq_len(3L))))
  expect_equivalent(out2, exp2)
  children <- target_get_children(pipeline_get_target(pipeline, "cross3"))
  out3 <- map_int(children, value)
  exp3 <- rowSums(expand_grid(x = seq_len(3L), y = seq(11, 13)))
  expect_equivalent(out3, exp3)
  expect_equal(value("out1"), sum(exp1))
  expect_equal(value("out2"), sum(exp2))
  expect_equal(value("out3"), sum(exp3))
  expect_equal(value("out4"), sum(exp1) + sum(exp2))
  expect_equal(value("out5"), sum(exp2) + sum(exp3))
  expect_equivalent(value("out6"), c(12, 25))
  children <- target_get_children(pipeline_get_target(pipeline, "map2"))
  out <- map_int(children, value)
  expect_equivalent(out, c(144, 625))
})

tar_test("empty mapping variable", {
  pipeline <- pipeline_init(
    list(
      target_init("x", quote(NULL)),
      target_init("y", quote(x), pattern = quote(cross(x)))
    )
  )
  expect_error(
    algorithm_init("local", pipeline)$run(),
    class = "condition_pattern"
  )
})

tar_test("cross print", {
  x <- tar_target(x, 1, cross(x, y, z))
  out <- utils::capture.output(print(x))
  expect_true(any(grepl("cross", out)))
})

tar_test("validate", {
  x <- target_init("x", expr = quote(1 + 1), pattern = quote(cross(a, b)))
  expect_silent(target_validate(x))
})
