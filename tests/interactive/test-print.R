tar_test("print stem", {
  x <- tar_target(x, {
    a <- 1
    b
  }, resources = list(cpu = 1, mem = 2))
  print(x)
  expect_equal(1, 1)
})

tar_test("print map", {
  print(tar_target(x, 1, pattern = map(y, z)))
  expect_equal(1, 1)
})

tar_test("print cross", {
  print(tar_target(x, 1, cross(y, z)))
  expect_equal(1, 1)
})

tar_test("print empty pipeline", {
  print(pipeline_init())
  expect_equal(1, 1)
})

tar_test("print pipeline with 1 target", {
  print(suppressWarnings(pipeline_init(list(tar_target(x, 1, cross(y, z))))))
  expect_equal(1, 1)
})

tar_test("print pipeline with 2 targets", {
  print(
    pipeline_init(
      list(
        tar_target(x, 1),
        tar_target(y, 1)
      )
    )
  )
  expect_equal(1, 1)
})

tar_test("print cue", {
  print(tar_cue())
  expect_equal(1, 1)
})
