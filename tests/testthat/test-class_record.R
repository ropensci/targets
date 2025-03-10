tar_test("record$produce_row() on an internal target", {
  record <- record_init(
    name = "x_name",
    parent = "x_parent",
    type = "x_target",
    command = "x_command",
    depend = "x_depend",
    path = c("x_path", "y_path"),
    data = "x_data",
    time = "f12345",
    size = "f123456",
    bytes = 123,
    format = "rds",
    iteration = "x_iteration",
    children = c("x_branch", "y_branch"),
    error = c("stop", "msg")
  )
  row <- record_produce_row(record)
  expect_true(is.list(row))
  expect_equal(sort(names(row)), sort(header_meta()))
  expect_equal(row$name, "x_name")
  expect_equal(row$parent, "x_parent")
  expect_equal(row$type, "x_target")
  expect_equal(row$command, "x_command")
  expect_equal(row$depend, "x_depend")
  expect_equal(row$path, list(NA_character_))
  expect_equal(row$data, "x_data")
  expect_equal(row$time, "f12345")
  expect_equal(row$size, "f123456")
  expect_equal(row$bytes, 123)
  expect_equal(row$format, "rds")
  expect_equal(row$iteration, "x_iteration")
  expect_equal(row$children, list(c("x_branch", "y_branch")))
  expect_equal(row$error, record_encode_field(c("stop", "msg")))
})

tar_test("record$produce_row() on an external target", {
  record <- record_init(
    name = "x_name",
    parent = "x_parent",
    type = "x_target",
    command = "x_command",
    depend = "x_depend",
    path = c("x_path", "y_path"),
    data = "x_data",
    time = "f12345",
    size = "f123456",
    bytes = 123,
    format = "file",
    iteration = "x_iteration",
    children = c("x_branch", "y_branch"),
    error = c("stop", "msg")
  )
  row <- record_produce_row(record)
  expect_true(is.list(row))
  expect_equal(sort(names(row)), sort(header_meta()))
  expect_equal(row$name, "x_name")
  expect_equal(row$parent, "x_parent")
  expect_equal(row$type, "x_target")
  expect_equal(row$command, "x_command")
  expect_equal(row$depend, "x_depend")
  expect_equal(row$path, list(c("x_path", "y_path")))
  expect_equal(row$data, "x_data")
  expect_equal(row$time, "f12345")
  expect_equal(row$size, "f123456")
  expect_equal(row$bytes, 123)
  expect_equal(row$format, "file")
  expect_equal(row$iteration, "x_iteration")
  expect_equal(row$children, list(c("x_branch", "y_branch")))
  expect_equal(row$error, record_encode_field(c("stop", "msg")))
})

tar_test("record_encode_field() empty", {
  expect_equal(record_encode_field(NULL), NA_character_)
})

tar_test("record_encode_field() nonempty", {
  out <- record_encode_field(c("stop", "msg"))
  expect_equal(out, "stop msg")
  expect_true(nzchar(out))
})

tar_test("record_encode_field() nonempty", {
  out <- record_encode_field(c("stop", "msg"))
  expect_equal(out, "stop msg")
  expect_true(nzchar(out))
})

tar_test("record_encode_field() with bad characters", {
  out <- record_encode_field(c("a\nb*|c\r", "b\t"))
  expect_equal(out, "a bc  b ")
  expect_true(nzchar(out))
  expect_length(out, 1L)
})

tar_test("record_encode_field() with more bad characters", {
  str <- "123abcXYZ90878734 123a*}*}*}|||*|*|*|*|bcXYZ90878734 ...,,,_-="
  out <- record_encode_field(str)
  exp <- "123abcXYZ90878734 123a}}}bcXYZ90878734 ...,,,_-="
  expect_equal(out, exp)
})

tar_test("record_validate(record) empty", {
  expect_silent(record_validate(record_init()))
})

tar_test("record_is_target()", {
  expect_true(record_is_target(record_init(type = "stem")))
  expect_true(record_is_target(record_init(type = "branch")))
  expect_true(record_is_target(record_init(type = "map")))
  expect_true(record_is_target(record_init(type = "cross")))
  expect_false(record_is_target(record_init(type = "object")))
  expect_false(record_is_target(record_init(type = "function")))
})

tar_test("record_validate() custom", {
  out <- record_init(
    name = "x_name",
    parent = "x_parent",
    type = "x_target",
    command = "x_command",
    depend = "x_depend",
    path = c("x_path", "y_path"),
    data = "x_data",
    time = "x123",
    size = "x456",
    bytes = 123,
    format = "x_format",
    iteration = "x_iteration",
    children = c("x_branch", "y_branch")
  )
  expect_silent(record_validate(out))
})
