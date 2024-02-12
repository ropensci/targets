use_targets(open = FALSE) # Should not open the file.
use_targets(open = TRUE) # Should open the file for editing.
expect_true(file.exists("_targets.R"))
unlink("_targets.R")
