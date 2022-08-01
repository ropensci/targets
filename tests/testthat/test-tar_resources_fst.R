tar_test("tar_resources_fst()", {
  out <- tar_resources_fst()
  expect_silent(resources_validate(out))
})

tar_test("tar_resources_fst() default compress", {
  tar_option_set(
    resources = tar_resources(
      fst = tar_resources_fst(
        compress = 80L
      )
    )
  )
  out <- tar_resources_fst()
  expect_equal(out$compress, 80L)
})
