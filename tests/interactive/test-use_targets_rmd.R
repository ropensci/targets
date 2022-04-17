use_targets_rmd(path = "_targets.Rmd", open = FALSE)
expect_true(file.exists("_targets.Rmd"))
unlink("_targets.Rmd")
