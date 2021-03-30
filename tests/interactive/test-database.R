tar_test("database$append_line() loops when it cannot append to the file", {
  path <- file.path(tempfile(), "x", "y")
  database <- database_init(path = path)
  database$append_line("line")
})
