tar_test("tar_debug_instructions()", {
  on.exit(tar_runtime$unset_target())
  targets <- list(
    tar_target(a, g(a)),
    tar_target(a, b),
    tar_target(a, NULL)
  )
  for (target in targets) {
    tar_runtime$set_target(target)
    target$command$expr <- as.expression(
      list(
        instructions = quote(targets::tar_debug_instructions()),
        browser = quote(browser()),
        expr = target$command$expr
      )
    )
    suppressMessages(expect_message(tar_debug_instructions()))
  }
})
