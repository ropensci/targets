use_targets_rmd(path = "_targets.Rmd", open = FALSE) # Should not open.
use_targets_rmd(path = "_targets.Rmd", open = TRUE) # Should open.
expect_true(file.exists("_targets.Rmd"))
unlink("_targets.Rmd")
