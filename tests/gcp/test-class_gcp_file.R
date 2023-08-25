# Use sparingly to minimize GCP costs.
# Verify all `targets` buckets are deleted afterwards.
tar_test("gcp_file format file gets stored", {
  skip_if_no_gcp()
  bucket_name <- random_bucket_name()
  # needs to be a GCP project the tester auth has access to
  gcp_gcs_auth(max_tries = 5L)
  project <- Sys.getenv("GCE_DEFAULT_PROJECT_ID")
  googleCloudStorageR::gcs_create_bucket(bucket_name, projectId = project)
  on.exit(gcp_gcs_delete_bucket(bucket_name))
  expr <- quote({
    tar_option_set(resources = tar_resources(
      gcp = tar_resources_gcp(bucket = !!bucket_name, prefix = "_targets")
    ))
    evalq(
      write_local_file <- function(lines) {
        writeLines(lines, "example_gcp_file.txt")
        "example_gcp_file.txt"
      }, envir = tar_option_get("envir")
    )
    list(
      tar_target(
        x,
        write_local_file("x_lines"),
        format = "file",
        repository = "gcp"
      ),
      tar_target(y, readLines(x))
    )
  })
  expr <- tar_tidy_eval(expr, environment(), TRUE)
  eval(as.call(list(`tar_script`, expr, ask = FALSE)))
  tar_make(callr_function = NULL, envir = tar_option_get("envir"))
  expect_true(
    gcp_gcs_exists(
      bucket = bucket_name,
      key = "_targets/objects/x",
      max_tries = 5
    )
  )
  unlink("_targets/scratch", recursive = TRUE)
  expect_equal(tar_read(y), "x_lines")
  expect_equal(length(list.files("_targets/scratch/")), 0L)
  expect_false(file.exists("_targets/scratch/x"))
  expect_false(file.exists("example_gcp_file.txt"))
  path <- tar_read(x)
  expect_equal(length(list.files("_targets/scratch/")), 0L)
  expect_false(file.exists("_targets/scratch/x"))
  expect_true(file.exists("example_gcp_file.txt"))
  expect_equal(readLines("example_gcp_file.txt"), "x_lines")
  tmp <- tempfile()
  gcp_gcs_download(
    key = "_targets/objects/x",
    bucket = bucket_name,
    file = tmp,
    max_tries = 5
  )
  expect_equal(readLines(tmp), "x_lines")
})

# Run once with debug(store_unload.tar_gcp_file) # nolint
# to make sure scratch file gets unloaded according to
# the `memory` setting. Should run more twice for persistent
# and then four times for transient.
tar_test("gcp_file format invalidation", {
  skip_if_no_gcp()
  gcp_gcs_auth(max_tries = 5L)
  project <- Sys.getenv("GCE_DEFAULT_PROJECT_ID")
  for (memory in c("persistent", "transient")) {
    # print(memory) # Uncomment for debug() test. # nolint
    bucket_name <- random_bucket_name()
    # needs to be a GCP project the tester auth has access to
    googleCloudStorageR::gcs_create_bucket(bucket_name, projectId = project)
    expr <- quote({
      tar_option_set(
        resources = tar_resources(
          gcp = tar_resources_gcp(bucket = !!bucket_name, prefix = "_targets")
        ),
        memory = !!memory
      )
      evalq(
        write_local_file <- function(lines) {
          writeLines(lines, "example_gcp_file.txt")
          "example_gcp_file.txt"
        }, envir = tar_option_get("envir")
      )
      list(
        tar_target(
          x,
          write_local_file("x_lines"),
          format = "file",
          repository = "gcp"
        ),
        tar_target(y, readLines(x))
      )
    })
    expr <- tar_tidy_eval(expr, environment(), TRUE)
    eval(as.call(list(`tar_script`, expr, ask = FALSE)))
    tar_make(callr_function = NULL, envir = tar_option_get("envir"))
    expect_equal(tar_progress(x)$progress, "built")
    expect_equal(tar_progress(y)$progress, "built")
    tar_make(callr_function = NULL)
    progress <- tar_progress()
    progress <- progress[progress$progress != "skipped", ]
    expect_equal(nrow(progress), 0L)
    expr <- quote({
      tar_option_set(
        resources = tar_resources(
          gcp = tar_resources_gcp(bucket = !!bucket_name)
        ),
        memory = !!memory
      )
      evalq(
        write_local_file <- function(lines) {
          writeLines(lines, "example_gcp_file.txt")
          "example_gcp_file.txt"
        }, envir = tar_option_get("envir")
      )
      list(
        tar_target(
          x,
          write_local_file("x_lines2"),
          format = "file",
          repository = "gcp"
        ),
        tar_target(y, readLines(x))
      )
    })
    expr <- tar_tidy_eval(expr, environment(), TRUE)
    eval(as.call(list(`tar_script`, expr, ask = FALSE)))
    tar_make(callr_function = NULL, envir = tar_option_get("envir"))
    expect_equal(tar_progress(x)$progress, "built")
    expect_equal(tar_progress(y)$progress, "built")
    expect_equal(tar_read(y), "x_lines2")
    unlink("example_gcp_file.txt")
    gcp_gcs_delete_bucket(bucket_name)
  }
})

tar_test("gcp_file format with a custom data store", {
  skip_if_no_gcp()
  tar_config_set(store = "custom_targets_store")
  bucket_name <- random_bucket_name()
  # needs to be a GCP project the tester auth has access to
  gcp_gcs_auth(max_tries = 5L)
  project <- Sys.getenv("GCE_DEFAULT_PROJECT_ID")
  googleCloudStorageR::gcs_create_bucket(bucket_name, projectId = project)
  on.exit(gcp_gcs_delete_bucket(bucket_name))
  expr <- quote({
    tar_option_set(resources = tar_resources(
      gcp = tar_resources_gcp(bucket = !!bucket_name, prefix = "_targets")
    ))
    evalq(
      write_local_file <- function(lines) {
        writeLines(lines, "example_gcp_file.txt")
        "example_gcp_file.txt"
      }, envir = tar_option_get("envir")
    )
    list(
      tar_target(
        x,
        write_local_file("x_lines"),
        format = "file",
        repository = "gcp"
      ),
      tar_target(y, readLines(x))
    )
  })
  expr <- tar_tidy_eval(expr, environment(), TRUE)
  eval(as.call(list(`tar_script`, expr, ask = FALSE)))
  tar_make(callr_function = NULL, envir = tar_option_get("envir"))
  expect_true(file.exists("custom_targets_store"))
  expect_false(file.exists(path_store_default()))
  expect_true(
    gcp_gcs_exists(
      bucket = bucket_name,
      key = "_targets/objects/x",
      max_tries = 5
    )
  )
  expect_equal(tar_read(y), "x_lines")
  expect_equal(length(list.files("custom_targets_store/scratch/")), 0L)
  expect_false(file.exists("example_gcp_file.txt"))
  path <- tar_read(x)
  expect_equal(length(list.files("custom_targets_store/scratch/")), 0L)
  expect_true(file.exists("example_gcp_file.txt"))
  expect_equal(readLines("example_gcp_file.txt"), "x_lines")
  tmp <- tempfile()
  gcp_gcs_download(
    key = "_targets/objects/x",
    bucket = bucket_name,
    file = tmp,
    max_tries = 5
  )
  expect_equal(readLines(tmp), "x_lines")
})
