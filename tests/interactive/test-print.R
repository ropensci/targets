tar_test("print stem", {
  x <- tar_target(x, {
    a <- 1
    b
  }, resources = list(cpu = 1, mem = 2))
  print(x)
})

tar_test("print map", {
  print(tar_target(x, 1, map(x, y, z)))
})

tar_test("print cross", {
  print(tar_target(x, 1, cross(x, y, z)))
})

tar_test("print empty pipeline", {
  print(tar_pipeline())
})

tar_test("print pipeline with 1 target", {
  print(tar_pipeline(tar_target(x, 1, cross(x, y, z))))
})

tar_test("print pipeline with 2 targets", {
  print(
    tar_pipeline(
      tar_target(x, 1),
      tar_target(y, 1)
    )
  )
})
