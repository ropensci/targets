tar_test("print stem", {
  expect_warning(
    x <- tar_target(x, {
      a <- 1
      b
    }, template = list(cpu = 1, mem = 2)),
    class = "condition_validate"
  )
  print(x)
})

tar_test("print map", {
  print(tar_target(x, 1, map(x, y, z)))
})

tar_test("print cross", {
  print(tar_target(x, 1, cross(x, y, z)))
})
