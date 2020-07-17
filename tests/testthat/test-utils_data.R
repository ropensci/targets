tar_test("mask_pointers()", {
  str <- "function (x, y, z) \n.Call(<pointer: 0x116937930>, x, y, z)"
  x <- mask_pointers(str)
  expect_true(grepl("function", x))
  expect_true(grepl("Call", x))
  expect_false(grepl("pointer: 0x", x))
  expect_true(grepl("pointer: 0x", str))
})
