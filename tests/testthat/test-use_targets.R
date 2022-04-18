tar_test("use_targets() overwrite", {
  script <- tar_config_get("script")
  use_targets(overwrite = FALSE, open = FALSE)
  expect_true(file.exists(script))
  expect_true(file.exists("run.sh"))
  expect_true(file.exists("run.R"))
  writeLines("abc", script)
  expect_identical(readLines(script), "abc")
  use_targets(overwrite = FALSE, open = FALSE)
  expect_identical(readLines(script), "abc")
  use_targets(overwrite = TRUE, open = FALSE)
  expect_false(identical(readLines(script), "abc"))
})

tar_test("use_targets() multicore", {
  for (package in c("future", "future.callr", "future.batchtools")) {
    skip_if_not_installed(package)
  }
  script <- tar_config_get("script")
  use_targets(scheduler = "multicore", open = FALSE)
  expect_false(file.exists("clustermq.tmpl"))
  expect_false(file.exists("future.tmpl"))
  line <- grep("clustermq\\.scheduler", readLines(script), value = TRUE)
  expect_true(grepl("multicore", line))
  line <- grep("future::plan", readLines(script), value = TRUE)
  expect_equal(line, "future::plan(future.callr::callr)")
})

tar_test("use_targets() multiprocess", {
  for (package in c("future", "future.callr", "future.batchtools")) {
    skip_if_not_installed(package)
  }
  script <- tar_config_get("script")
  use_targets(scheduler = "multiprocess", open = FALSE)
  expect_false(file.exists("clustermq.tmpl"))
  expect_false(file.exists("future.tmpl"))
  line <- grep("clustermq\\.scheduler", readLines(script), value = TRUE)
  expect_true(grepl("multiprocess", line))
  line <- grep("future::plan", readLines(script), value = TRUE)
  expect_equal(line, "future::plan(future.callr::callr)")
})

tar_test("use_targets() schedulers", {
  for (package in c("future", "future.callr", "future.batchtools")) {
    skip_if_not_installed(package)
  }
  script <- tar_config_get("script")
  for (scheduler in c("slurm", "sge", "lsf", "pbs", "torque")) {
    unlink("*.tmpl")
    use_targets(scheduler = scheduler, open = FALSE, overwrite = TRUE)
    expect_true(file.exists("clustermq.tmpl"))
    expect_true(file.exists("future.tmpl") || scheduler == "pbs")
    line <- grep("clustermq\\.scheduler", readLines(script), value = TRUE)
    expect_true(grepl(scheduler, line))
    line <- grep("future::plan", readLines(script), value = TRUE)
    expect_true(grepl(scheduler, line) || scheduler == "pbs")
  }
})
