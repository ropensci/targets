tar_test("pedigree_validate() with a good pedigree", {
  pedigree <- pedigree_init("x", "x_f4acd87c52d4e62b", 2L)
  expect_silent(pedigree_validate(pedigree))
})

tar_test("pedigree_validate() with an extra field", {
  pedigree <- pedigree_init("x", "x_f4acd87c52d4e62b", 2L)
  pedigree$wrong <- 1
  expect_error(pedigree_validate(pedigree), class = "condition_validate")
})

tar_test("pedigree_validate() with a bad parent", {
  pedigree <- pedigree_new("_illegal", "x_f4acd87c52d4e62b", 2L)
  expect_error(pedigree_validate(pedigree), class = "condition_validate")
})

tar_test("pedigree_validate() with an incorrect child", {
  pedigree <- pedigree_new("x", "mismatch", 2L)
  expect_error(pedigree_validate(pedigree), class = "condition_validate")
})

tar_test("pedigree_validate() with a bad index", {
  pedigree <- pedigree_new("x", "x_f4acd87c52d4e62b", -3L)
  expect_error(pedigree_validate(pedigree), class = "condition_validate")
})
