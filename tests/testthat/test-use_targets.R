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

tar_test("use_targets() local schedulers", {
  for (scheduler in c("multicore", "multiprocess")) {
    expect_false(file.exists("job.sh"))
    use_targets(scheduler = scheduler, open = FALSE, overwrite = TRUE)
    shell <- file.path("run", "run.sh")
    shell <- system.file(shell, package = "targets", mustWork = TRUE)
    exp <- readLines(shell)
    out <- readLines("run.sh")
    expect_true(identical(out, exp))
  }
})

tar_test("use_targets() hpc schedulers", {
  for (package in c("future", "future.callr", "future.batchtools")) {
    skip_if_not_installed(package)
  }
  script <- tar_config_get("script")
  for (scheduler in c("slurm", "sge", "lsf", "pbs", "torque")) {
    unlink("*.tmpl")
    use_targets(
      scheduler = scheduler,
      open = FALSE,
      overwrite = TRUE,
      job_name = "my_job_name"
    )
    expect_true(file.exists("clustermq.tmpl"))
    expect_true(file.exists("future.tmpl") || scheduler == "pbs")
    line <- grep("clustermq\\.scheduler", readLines(script), value = TRUE)
    expect_true(grepl(scheduler, line))
    lines <- readLines("clustermq.tmpl")
    expect_false(any(grepl("JOB_NAME", lines)))
    expect_true(any(grepl("my_job_name", lines)))
    tmpl <- file.path("templates", "clustermq", paste0(scheduler, ".tmpl"))
    path <- system.file(tmpl, package = "targets", mustWork = TRUE)
    exp <- readLines(path)
    exp <- gsub(pattern = "JOB_NAME", replacement = "my_job_name", x = exp)
    expect_true(identical(lines, exp))
    line <- grep("future::plan", readLines(script), value = TRUE)
    expect_true(grepl(scheduler, line) || scheduler == "pbs")
    shell <- file.path("run", "job", paste0(scheduler, ".sh"))
    shell <- system.file(shell, package = "targets", mustWork = TRUE)
    exp <- readLines(shell)
    exp <- gsub(pattern = "JOB_NAME", replacement = "my_job_name", x = exp)
    out <- readLines("job.sh")
    expect_true(identical(out, exp))
    expect_false(any(grepl("JOB_NAME", out)))
    expect_true(any(grepl("my_job_name", out)))
    shell <- file.path("run", "run.sh")
    shell <- system.file(shell, package = "targets", mustWork = TRUE)
    exp <- readLines(shell)
    out <- readLines("run.sh")
    expect_true(identical(out, exp))
  }
})
