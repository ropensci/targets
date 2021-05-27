tar_test("tar_make_interactive", {
  code <- c("list(", "tar_target(x, \"a\"),", "tar_target(y, \"b\")", ")")
  tar_make_interactive(code)
  
})
