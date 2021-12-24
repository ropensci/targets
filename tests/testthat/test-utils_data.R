tar_test("mask_pointers()", {
  str <- "function (x, y, z) \n.Call(<pointer: 0x116937930>, x, y, z)"
  x <- mask_pointers(str)
  expect_true(grepl("function", x))
  expect_true(grepl("Call", x))
  expect_false(grepl("pointer: 0x", x))
  expect_true(grepl("pointer: 0x", str))
})

tar_test("keyvalue_field()", {
  x <- c("bucket=bu", "region=reg", "key=sdfasdf")
  expect_equal(
    keyvalue_field(x = x, pattern = "^bucket="),
    "bu"
  )
  expect_equal(
    keyvalue_field(x = x, pattern = "^region="),
    "reg"
  )
  expect_equal(
    keyvalue_field(x = x, pattern = "^key="),
    "sdfasdf"
  )
})
