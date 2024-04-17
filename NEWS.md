# targets 1.7.0

## Invalidating changes

* Use `secretbase::siphash13()` instead of `digest(algo = "xxhash64", serializationVersion = 3)` so hashes of in-memory objects no longer depend on serialization version 3 headers (#1244, @shikokuchuo). Unfortunately, pipelines built with earlier versions of `targets` will need to rerun.

## Other improvements

* Ensure patterns marshal properly (#1266, #1264, https://github.com/njtierney/geotargets/issues/52, @Aariq, @njtierney).
* Inform and prompt the user when the pipeline was built with an old version of `targets` and changes to the package will cause the current work to rerun (#1244). For the `tar_make*()` functions, `utils::menu()` prompts the user to give people a chance to downgrade if necessary.
* For type safety in the internal database class, read all columns as character vectors in `data.table::fread()`, then convert them to the correct types afterwards.
* Add a new `tar_resources_custom_format()` function which can pass environment variables to customize the behavior of custom `tar_format()` storage formats (#1263, #1232, @Aariq, @noamross).
* Only marshal dependencies if actually sending the target to a parallel worker.

# targets 1.6.0

* Modernize `extras` in `tar_renv()`.
* `tar_target()` gains a `description` argument for free-form text describing what the target is about (#1230, #1235, #1236, @tjmahr).
* `tar_visnetwork()`, `tar_glimpse()`, `tar_network()`, `tar_mermaid()`, and `tar_manifest()` now optionally show target descriptions (#1230, #1235, #1236, @tjmahr).
* `tar_described_as()` is a new wrapper around `tidyselect::any_of()` to select specific subsets of targets based on the description rather than the name (#1136, #1196, @noamross, @mattmoo).
* Fix the documentation of the `names` argument (nudge users toward `tidyselect` expressions).
* Make assertions on the pipeline process more robust (to check if two processes are trying to access the same data store).

# targets 1.5.1

* Avoid `arrow`-related CRAN check NOTE.
* `use_targets()` only writes the `_targets.R` script. The `run.sh` and `run.R` scripts are superseded by the `as_job` argument of `tar_make()`. Users not using the RStudio IDE can call `tar_make()` with `callr_function = callr::r_bg` to run the pipeline as a background process. `tar_make_clustermq()` and `tar_make_future()` are superseded in favor `tar_make(use_crew = TRUE)`, so template files are no longer written for the former automatically.

# targets 1.5.0

## Invalidating changes

Because of the changes below, upgrading to this version of `targets` will unavoidably invalidate previously built targets in existing pipelines. Your pipeline code should still work, but any targets you ran before will most likely need to rerun after the upgrade.

* In `tar_seed_create()`, use `secretbase::sha3(x = TARGET_NAME, bits = 32L, convert = NA)` to generate target seeds that are more resistant to overlapping RNG streams (#1139, @shikokuchuo). The previous approach used a less rigorous combination of `digest::digest(algo = "sha512")` and `digets::digest2int()`.

## Other improvements

* Update the documentation of the `deployment` argument of `tar_target()` to reflect the advent of `crew` (#1208, @psychelzh).
* Unset `cli.num_colors` on exit in `tar_error()` and `tar_warning()` (#1210, @dipterix).
* Do not try to access `seconds_timeout` if the `crew` controller is actually a controller group (#1207, https://github.com/wlandau/crew.cluster/discussions/35, @stemangiola, @drejom).
* `tar_make()` gains an `as_job` argument to optionally run a `targets` pipeline as an RStudio job.
* Bump required `igraph` version to 2.0.0 because `igraph::get.edgelist()` was deprecated in favor of `igraph::as_edgelist()`.
* Do not dispatch targets to backlogged `crew` controllers (or controller groups) (#1220). Use the new `push_backlog()` and `pop_backlog()` `crew` methods to make this smooth.
* Make the debugger message more generic (#1223, @eliocamp).
* Throw an early and informative error from `tar_make()` if there is already a `targets` pipeline running on a local process on the same local data store. The local process is detected using the process ID and time stamp from `tar_process()` (with a 1.01-second tolerance for the time stamp).
* Remove `pkgload::load_all()` warning (#1218). Tried using `.__DEVTOOLS__` but it interferes with reverse dependencies.
* Add documentation and an assertion in `tar_target_raw()` to let users know that `iteration = "group"` is invalid for dynamic targets (ones with `pattern = map(...)` etc.; #1226, @bmfazio).

# targets 1.4.1

* Print "errored pipeline" when at least one target errors.
* Bump minimum `clustermq` version to 0.9.2.
* Repair the `tar_debug_instructions()` tips for when commands are long.
* Do not look for dependencies of primitive functions (#1200, @smwindecker, @joelnitta).

# targets 1.4.0

## Invalidating changes

Because of the changes below, upgrading to this version of `targets` will unavoidably invalidate previously built targets in existing pipelines. Your pipeline code should still work, but any targets you ran before will most likely need to rerun after the upgrade.

* Use SHA512 during the creation of target-specific pseudo-random number generator seeds (#1139). This change decreases the risk of overlapping/correlated random number generator streams. See the "RNG overlap" section of the `tar_seed_create()` help file for details and justification. Unfortunately, this change will invalidate all currently built targets because the seeds will be different. To avoid rerunning your whole pipeline, set `cue = tar_cue(seed = FALSE)` in `tar_target()`.
* For cloud storage: instead of the hash of the local file, use the ETag for AWS S3 targets and the MD5 hash for GCP GCS targets (#1172). Sanitize with `targets:::digest_chr64()` in both cases before storing the result in the metadata.
* For a cloud target to be truly up to date, the hash in the metadata now needs to match the *current* object in the bucket, not the version recorded in the metadata (#1172). In other words, `targets` now tries to ensure that the up-to-date data objects in the cloud are in their newest versions. So if you roll back the metadata to an older version, you will still be able to access historical data versions with e.g. `tar_read()`, but the pipeline will no longer be up to date.

## Other changes to seeds

* Add a new exported function `tar_seed_create()` which creates target-specific pseudo-random number generator seeds.
* Add an "RNG overlap" section in the `tar_seed_create()` help file to justify and defend how `targets` and `tarchetypes` approach pseudo-random numbers.
* Add function `tar_seed_set()` which sets a seed and sets all the RNG algorithms to their defaults in the R installation of the user. Each target now uses `tar_seed_set()` function to set its seed before running its R command (#1139).
* Deprecate `tar_seed()` in favor of the new `tar_seed_get()` function.

## Other cloud storage improvements

* For all cloud targets, check hashes in batched LIST requests instead of individual HEAD requests (#1172). Dramatically speeds up the process of checking if cloud targets are up to date.
* For AWS S3 targets, `tar_delete()`, `tar_destroy()`, and `tar_prune()` now use efficient batched calls to `delete_objects()` instead of costly individual calls to `delete_object()` (#1171).
* Add a new `verbose` argument to `tar_delete()`, `tar_destroy()`, and `tar_prune()`.
* Add a new `batch_size` argument to `tar_delete()`, `tar_destroy()`, and `tar_prune()`.
* Add new arguments `page_size` and `verbose` to `tar_resources_aws()` (#1172).
* Add a new `tar_unversion()` function to remove version IDs from the metadata of cloud targets. This makes it easier to interact with just the current version of each target, as opposed to the version ID recorded in the local metadata.

## Other improvements

* Migrate to the changes in `clustermq` 0.9.0 (@mschubert).
* In progress statuses, change "started" to "dispatched" and change "built" to "completed" (#1192).
* Deprecate `tar_started()` in favor of `tar_dispatched()` (#1192).
* Deprecate `tar_built()` in favor of `tar_completed()` (#1192).
* Console messages from reporters say "dispatched" and "completed" instead of "started" and "built" (#1192).
* The `crew` scheduling algorithm no longer waits on saturated controllers, and targets that are ready are greedily dispatched to `crew` even if all workers are busy (#1182, #1192). To appropriately set expectations for users, reporters print "dispatched (pending)" instead of "dispatched" if the task load is backlogged at the moment.
* In the `crew` scheduling algorithm, waiting for tasks is now a truly event-driven process and consumes 5-10x less CPU resources (#1183). Only the auto-scaling of workers uses polling (with an inexpensive default polling interval of 0.5 seconds, configurable through `seconds_interval` in the controller).
* Simplify stored target tracebacks.
* Print the traceback on error.

# targets 1.3.2

* Try to fix function help files for CRAN.

# targets 1.3.1

* Add `tar_config_projects()` and `tar_config_yaml()` (#1153, @psychelzh).
* Apply error modes to `builder_wait_correct_hash()` in `target_conclude.tar_builder()` (#1154, @gadenbuie).
* Remove duplicated error message from `builder_error_null()`.
* Allow `tar_meta_upload()` and `tar_meta_download()` to avoid errors if one or more metadata files do not exist. Add a new argument `strict` to control error behavior.
* Add new arguments `meta`, `progress`, `process`, and `crew` to control individual metadata files in `tar_meta_upload()`, `tar_meta_download()`, `tar_meta_sync()`, and `tar_meta_delete()`.
* Avoid newly deprecated arguments and functions in `crew` 0.5.0.9003 (https://github.com/wlnadau/crew/issues/131).
* Allow `tar_read()` etc. inside a pipeline whenever it uses a different data store (#1158, @MilesMcBain).
* Set `seed = FALSE` in `future::future()` (#1166, @svraka).
* Add a new `physics` argument to `tar_visnetwork()` and `tar_glimpse()` (#925, @Bdblodgett-usgs).

# targets 1.3.0

## Invalidating changes

Because of these changes, upgrading to this version of `targets` will unavoidably invalidate previously built targets in existing pipelines. Your pipeline code should still work, but any targets you ran before will most likely need to rerun after the upgrade.

* In the `hash_deps()` method of the metadata class, exclude symbols which are not actually dependencies, rather than just giving them empty strings. This change decouples the dependency hash from the hash of the target's command (#1108).

## Cloud metadata

* Continuously upload metadata files to the cloud during `tar_make()`, `tar_make_clustermq()`, and `tar_make_future()` (#1109). Upload them to the repository specified in the `repository_meta` `tar_option_set()` option, and use the bucket and prefix set in the `resources` `tar_option_set()` option. `repository_meta` defaults to the existing `repository` `tar_option_set()` option.
* Add new functions `tar_meta_download()`, `tar_meta_upload()`, `tar_meta_sync()`, and `tar_meta_delete()` to directly manage cloud metadata outside the pipeline (#1109).

## Other changes

* Fix solution of #1103 so the copy fallback actually runs (@jds485, #1102, #1103).
* Switch back to `tempdir()` for #1103.
* Move `path_scratch_dir_network()` to `file.path(tempdir(), "targets")` and make sure `tar_destroy("all")` and `tar_destroy("cloud")` delete it.
* Display `tar_mermaid()` subgraphs with transparent fills and black borders.
* Allow `database$get_data()` to work with list columns.
* Disallow functions that access the local data store (including metadata) from inside a target while the pipeline is running (#1055, #1063). The only exception to this is local file targets such as `tarchetypes` literate programming target factories like `tar_render()` and `tar_quarto()`.
* In the `hash_deps()` method of the metadata class, use a new custom `sort_chr()` function which temporarily sets the `LC_COLLATE` locale to `"C"` for sorting. This ensures lexicographic comparisons  are consistent across platforms (#1108).
* In `tar_source()`, use the `file` argument and `keep.source = TRUE` to help with interactive debugging (#1120).
* Deprecated `seconds_interval` in `tar_config_get()`, `tar_make()`, `tar_make_clustermq()` and `tar_make_future()`. Replace it with `seconds_meta` (to control how often metadata gets saved) and `seconds_reporter` (to control how often to print messages to the R console) (#1119).
* Respect `seconds_meta` and `seconds_reporter` for writing metadata and console messages even for currently building targets (#1055).
* Retry all cloud REST API calls with HTTP error codes (429, 500-599) with the exponential backoff algorithm from `googleAuthR` (#1112).
* For `format = "url"`, only retry on the HTTP error codes above.
* Make cloud temp file instances unique in order to avoid file conflicts with the same target.
* Un-deprecate `seconds_interval` and `seconds_timeout` from `tar_resources_url()`, and implement `max_tries` arguments in `tar_resources_aws()` and `tar_resources_gcp()` (#1127).
* Use `file` and `keep.source` in `parse()` in `callr` utils and target Markdown.
* Automatically convert `"file_fast"` format to `"file"` format for cloud targets.
* In `tar_prune()` and `tar_delete()`, do not try to delete pattern targets which have no cloud storage.
* Add new arguments `seconds_timeout`, `close_connection`, `s3_force_path_style` to `tar_resources_aws()` to support the analogous arguments in `paws.storage::s3()` (#1134, @snowpong).

# targets 1.2.2

* Fix a documentation issue for CRAN.

# targets 1.2.1

* Add `tar_prune_list()` (#1090, @mglev1n).
* Wrap `file.rename()` in `tryCatch()` and fall back on a copy-then-remove workaround (@jds485, #1102, #1103).
* Stage temporary cloud upload/download files in `tools::R_user_dir(package = "targets", which = "cache")` instead of `tempdir()`. `tar_destroy(destroy = "cloud")` and `tar_destroy(destroy = "all")` remove any leftover files from failed uploads/downloads (@jds485, #1102, #1103).
* Use `paws.storage` instead of all of `paws`.

# targets 1.2.0

## `crew` integration

* Do not assume S3 classes when validating `crew` controllers.
* Suggest a crew controller in the `_targets.R` file from `use_targets()`.
* Make `tar_crew()` compatible with `crew` >= 0.3.0.
* Rename argument `terminate` to `terminate_controller` in `tar_make()`.
* Add argument `use_crew` in `tar_make()` and add an option in `tar_config_set()` to make it configurable.
* Write progress data and metadata in `target_prepare()`.

## Other improvements

* Allow users to set the default `label` and `level_separation` arguments through `tar_config_set()` (#1085, @Moohan).

# targets 1.1.3

* Decide on `nanonext` usage in `time_seconds_local()` at runtime and not installation time. That way, if `nanonext` is removed after `targets` is installed, functions in `targets` still work. Fixes the CRAN issues seen in `tarchetypes`, `jagstargets`, and `gittargets`.

# targets 1.1.2

* Remove `crew`-related startup messages.

# targets 1.1.1

* Pre-compute `cli` colors and bullets to improve performance in RStudio.
* Use `packageStartupMessage()` for package startup messages.

# targets 1.1.0

## Bug fixes

* Send targets to the appropriate controller in a controller group when `crew` is used.

## General improvements

* Call `gc()` more appropriately when `garbage_collection` is `TRUE` in `tar_target()`.
* Add `garbage_collection` arguments to `tar_make()`, `tar_make_clustermq()`, and `tar_make_future()` to add optional garbage collection before targets are sent to workers. This is different and independent from the `garbage_collection` argument of `tar_target()`. In high-performance computing scenarios, the former controls what happens on the main controlling process, whereas the latter controls what happens on the worker.
* Add `garbage_collection` and `seconds_interval` arguments to `tar_make()`, `tar_make_clustermq()`, `tar_make_future()`, and `tar_config_set()`.
* Downsize the `tar_runtime` object.
* Remove the 100 Kb file size cutoff for determining whether to trust the file timestamp or recompute the hash when checking if a file is up to date (#1062). Instate the `"file_fast"` format and the `trust_object_timestamps` option in `tar_option_set()` as safer alternatives.
* Consolidate store constructors.
* Allow `crew` controller groups (#1065, @mglev1n).
* Expose more exponential backoff configuration parameters through `tar_backoff()`. The `backoff` argument of `tar_option_set()` now accepts output from `tar_backoff()`, and supplying a numeric is deprecated.
* Fix the exponential backoff rules in the `crew` scheduling algorithm.
* Implement `tar_resources_network()` to configure retries and timeouts for internal HTTP/HTTPS requests in specialized targets with `format = "url"`, `repository = "aws"`, and `repository = "gcp"`. Also applies to syncing target files across network file systems in the case of `storage = "worker"` or `format = "file"`, which previously had a hard-coded `seconds_interval = 0.1` and `seconds_timeout = 60`.
* Deprecate `seconds_interval` and `seconds_timeout` in `tar_resources_url()` in favor of the new equivalent arguments of `tar_resources_network()`
* Safely withhold a target from its `crew` controller when the controller is saturated (#1074, @mglev1n).
* Use exponential backoff when appending a target back to the queue in the case of a saturated `crew` controller.
* Use native retries in `paws.common` (@DyfanJones).

## Speedups

* Cache info about all of `_targets/objects/` in `tar_callr_inner_try()` and update the cache as targets are saved to `_targets/objects/` to avoid the overhead of repeated calls to `file.exists()` and `file.info()` (#1056).
* Trust the timestamps by default when checking whether files in `_targets/objects/` are up to date (#1062). `tar_option_set(trust_object_timestamps = FALSE)` ignores the timestamps and recomputes the hashes.
* Write to `_targets/meta/meta` and `_targets/meta/progress` in timed batches instead of line by line (#1055).
* Reporters now print progress messages in timed batches instead of line by line (#1055).
* The summary and forecast reporters are much faster because they avoid going through data frames.
* Avoid `tempfile()` when working with the scratch directory.
* Use `nanonext::mclock()` instead of `proc.time()` when there is no risk of forked processes.
* Replace `withr` with slightly faster/leaner base R alternatives.
* Efficiently catch changes to the working directory instead of overburdening the pipeline with calls to `setwd()` (#1057).
* Invoke `tar_options` methods in the internals instead of `tar_option_get()`.
* Avoid `gsub()` in `store_init()`.
* Avoid repeated calls to `meta$get_record()` in `builder_should_run()`.
* Mock the store object when creating a record from a metadata row.
* Avoid `cli::col_none()` to reduce the number of ANSI characters printed to the R console.

# targets 1.0.0

`targets` is moving to version 1.0.0 because it is significantly more mature than previous versions. Specifically,

1. `tar_make()` now integrates with `crew`, which will significantly improve the way `targets` does high-performance computing going forward.
2. All other functionality in `targets` has stabilized. There is still room for smaller new features, but none as large as `crew` integration, none that will fundamentally change how the package operates.

## Major improvements

* Support distributed computing through the `crew` package in `tar_make()` (#753). `crew` itself is still in its early stages and currently lacks the launcher plugins to match the `clustermq` and `future` backends, but long-term, `crew` will be the predominant high-performance computing backend.

## Minor improvements

* Add a new `store_copy_object()` to the store class to enable `"fst_dt"` and other formats to make deep copies when needed (#1041, @MilesMcBain).
* Add a new `copy` argument to allow `tar_format()` formats to set the `store_copy_object()` method (#1041, @MilesMcBain).
* Shorten the output string returned by `tar_format()` when default methods are used.
* Add a `change_directory` argument to `tar_source()` (#1040, @dipterix).
* In `format = "url"` targets, implement retries and timeouts when connecting to URLs. The default timeout is 10 seconds, and the default retry interval is 1 second. Both are configurable via `tar_resources_url()` (#1048).
* Use `parallelly::freePort()` in `tar_random_port()`.
* Rename a target and a function in the `tar_script()` example pipeline (#1033, @b-rodrigues).
* Edit the description.

# targets 0.14.3

* Handle encoding errors while trying to process error and warning messages (#1019, @adrian-quintario).
* Fix S3 generic/method consistency.

# targets 0.14.2

* Forward user-level custom error conditions to the top of the pipeline (#997, @alexverse).
* Link to the help page of the manual.

# targets 0.14.1

* Fix the command inserted for debug mode (#975).
* Set empty chunk options to ensure Target Markdown compatibility with the special "setup" chunk (#973, @KaiAragaki).
* Only store the first 50 warnings in the metadata, and cap the text of the warning messages at 2048 characters (#983, @thejokenott).
* Enhance the `tar_destroy()` help file (#988, @Sage0614).
* Implement `destroy = "user"` in `tar_destroy()`.

# targets 0.14.0

* Move `#!/bin/sh` line to the top of SLURM `clustermq` template file (#944, #955, @GiuseppeTT).
* Add new function `tar_path_script()`.
* Rename `tar_store()` to `tar_path_store()` with deprecation.
* Rename `tar_path()` to `tar_path_target()` with deprecation.
* Add new function `tar_path_script_support()`.
* Make Target Markdown target scripts dynamically locate their support scripts so the appropriate scripts can be found even when they are generated from one directory and sourced from another (#953, #957, @TylerGrantSmith).
* Allow user-side control of the seeds at the pipeline level. `tar_option_set()` now supports a `seed` argument, and target-specific seeds are determined by `tar_option_get("seed")` and the target name. `tar_option_set(seed = NA)` disables seed-setting behavior but forcibly invalidates all the affected targets except when `seed` is `FALSE` in the target's `tar_cue()` (#882, @sworland-thyme, @joelnitta).
* Implement a `seed` argument in `tar_cue()` to control whether targets update in response to changing or `NA` seeds (#882, @sworland-thyme, @joelnitta).
* Reduce the number of per-target AWS/GCP storage API calls. Previously there were 3 API calls per target, including 2 HEAD requests. Now there is just 1 for a typical target (unless dependencies have to be downloaded). Relies on S3 strong read-after-write consistency (#958).
* Update the `tar_github_actions()` workflow file to use `@v2` (#960, @kulinar).
* Print helpful hints while debugging a target interactively (#961).
* Only attempt to debug a target when `callr_function` is `NULL` (#961).
* Make formats `"feather"`, `"parquet"`, `"file"`, and `"url"` work with `error = "null"` (#969).
* Declare formats `"keras"` and `"torch"` superseded by `tar_format()`. Documented in the `tar_target()` help file.
* Declare formats `"keras"` and `"torch"` incompatible with `error = "null"`. Documented in the `tar_target()` help file and in a warning thrown by `tar_target()` via `tar_target_raw()`.
* Add a `convert` argument to `tar_format()` to allow custom `store_convert_object()` methods (#970).

# targets 0.13.5

* Use `any_of()` instead of `all_of()` in tests to ensure compatibility with `tidyselect` 1.1.2.9000 (#928, @hadley).
* Make the `run.R` from `use_targets()` executable (#929, @petrbouchal).
* Add `#!/usr/bin/env Rscript` to the top of `run.R` from `use_targets()` (#929, @petrbouchal).

# targets 0.13.4

* Implement custom alternative to `skip_on_cran()` to avoid https://github.com/r-lib/testthat/issues/1470#issuecomment-1248145555.
* Skip more tests on CRAN.

# targets 0.13.3

## Enhancements

* Print "no targets found" when there are no targets in the pipeline to check or build, or if the `names` argument of `tar_make()` does not identify any such targets in the pipeline (#923, @llrs).
* Ignore `.packageName`, `.__NAMESPACE__.`, and `.__S3MethodsTable__.` when importing objects from packages with the `imports` option of `tar_option_set()`.
* Import datasets from packages in the `imports` option of `tar_option_set()` (#926, @joelnitta).
* Print target-specific elapsed runtimes in the verbose and timestamp reporters.
* Improve error messages in functions like `tar_read()` and `tar_load()` when the data store is missing.

# targets 0.13.2

## Bug fixes

* Do not incorrectly reference feather resources for parquet storage.

## Enhancements

* Simplify and improve error handling.
* In the `command` column of `tar_manifest()` output, separate lines with "\n" instead of "\\n" so the text output is straightforward to work with.
* Add a `drop_missing` argument to `tar_manifest()` to hide/show columns with all `NA` values.
* Do not set Parquet version.

# targets 0.13.1

* Fix reverse dependency checks.

# targets 0.13.0

## Bug fixes

* Do not bootstrap the junction of a stem unless the target is branched over (#858, @dipterix).
* For non-"file" AWS targets, immediately delete the scratch file after the target is uploaded (#889, @stuvet).

## New features

* Allow extra arguments to `paws` functions via `...` in `tar_resources_aws()` (#855, @michkam89).
* Add `tar_source()` to conveniently source R scripts (e.g. in `_targets.R`).

## Enhancements

* Color ordinary `targets` messages the default theme color, and color warnings and errors red (#856, @gorkang).
* Automatically supply job names in the scripts generated by `use_targets()`.
* Inherit resources one-by-one in nested fashion from `tar_option_get("resources")` (#892). See the revised `"Resources"` section of the `tar_resources()` help file for details.

# targets 0.12.1

## New features

* Add arguments `legend` and `color` to further configure `tar_mermaid()` (#848, @noamross).
* For HPC schedulers like SLURM and SGE, `use_targets()` now creates a `job.sh` script to run the pipeline as a cluster job (#839).

## Enhancements

* Use lapply() to source scripts in `use_targets()`. Avoids defining a global variable for the file.
* Recursively find scripts to source in the `use_targets()` `_targets.R` file.
* Refactor error printing.

# targets 0.12.0

## Bug fixes

* Fix `tar_mermaid()` graph ordering.
* Hash the node names and quote the label names of `tar_mermaid()` graphs to avoid JavaScript keywords.
* Remove superfluous line breaks in the node labels of graph visuals.
* Fix metadata migration to version >= 0.10.0 (#812, @tjmahr).
* `data.table::fread()` with encoding equal to `getOption("encoding")` if available (#814, @svraka). Only works with UTF-8 and latin1 because that is what `data.table` supports.
* Force add files in GitHub Actions workflow job (#815, @tarensanders).

## New features

* `use_targets()` now writes a `_targets.R` file tailored to the project in the current working directory (#639, @noamross).
* Move the old `use_targets()` to `use_targets_rmd()`.

## Enhancements

* Load packages when loading data for downstream targets in the pipeline (#713).
* Handle edge case when `getOption("OutDec")` is not `"."` to prevent time stamps from being corrupted (#433, @jarauh).
* Added helper function `tar_load_everything()` to quickly load all targets (#823, @malcolmbarrett)

# targets 0.11.0

## Bug fixes

* Print out the relevant target names if targets have conflicting names.
* Catch all the target warnings instead of just reporting the last one.
* Allow 200 group URL status codes instead of just 200 (#797, @petrbouchal).

## New features

* Add Google Cloud Storage via `tar_target(..., repository = "gcp")` (#720, @markedmondson1234). Special thanks to @markedmondson1234 for the cloud storage utilities in `R/utils_gcp.R`
* `mermaid.js` static graphs with `tar_mermaid()` (#775, @yonicd).
* Implement `tar_target(..., error = "null")`to allow errored targets to return `NULL` and continue (#807, @zoews). Errors are still registered, those targets are not up to date, and downstream targets have an easier time continuing on.
* Implement `tar_assert_finite()`.
* `tar_destroy()`, `tar_delete()`, and `tar_prune()` now attempt to delete cloud data for the appropriate targets (#799). In addition, `tar_exist_objects()` and `tar_objects()` now report about target data in the cloud when applicable. Add a new `cloud` argument to each function to optionally suppress this new behavior.
* Add a `zoom_speed` argument to `tar_visnetwork()` and `tar_glimpse()` (#749, @dipterix).
* Report the total runtime of the pipeline in the `"verbose"`, `"verbose_positives"`, `"timestamp"`, and `"timesamp_positives"` reporters.

## Enhancements

* Allow target name character strings to have attributes (#758, @psanker).
* Sort metadata rows when the pipeline finishes so that version-controlling the metadata is easier (#766, @jameelalsalam).

## Deprecations

* Deprecate the `"aws_*"` storage format values in favor of a new `repository` argument (#803). In other words, `tar_target(..., format = "aws_qs")` is now `tar_target(..., format = "qs", repository = "aws")`. And internally, storage classes with multiple inheritance are created dynamically as opposed to having hard-coded source files. All this paves the way to add new cloud storage platforms without combinatorial chaos.

# targets 0.10.0

## Bug fixes

* Add class `"tar_nonexportable"` to `format = "aws_keras"` and `format = "aws_torch"` stores.
* Export S3 methods of generic `tar_make_interactive_load_target()`.

## New features

* Allow entirely custom storage formats through `tar_target(format = tar_format(...))` (#736).
* Add a new function `tar_call()` to return the `targets` function currently running (from `_targets.R` or a target).
* Add a new function `tar_active()` to tell whether the pipeline is currently running. Detects if it is called from `tar_make()` or similar function.

## Enhancements

* Add `Sys.getenv("TAR_PROJECT")` to the output of `tar_envvars()`.
* Set the `store` field of `tar_runtime` prior to sourcing `_targets.R` so `tar_store()` works in target scripts.
* Explicitly export all the environment variables from `tar_envvars()` to targets run on parallel workers.
* Allow `format = "file"` targets to return `character(0)` (#728, @programLyrique).
* Automatically remove non-targets from the target list and improve target list error messages (#731, @billdenney).
* Link to resources on deploying to RStudio Connect (#745, @ian-flores).

# targets 0.9.1

* Mask pointers in function dependencies (#721, @matthiaskaeding)

# targets 0.9.0

## Highlights

* Track the version ID of AWS S3-backed targets if the bucket is version-enabled (#711). If you put your targets in AWS and the metadata and code under version control, you can `git checkout` a different branch of your code and all you targets will stay up to date.
* Refactor the AWS path format internally. It now consists of arbitrarily extensible key-value pairs so more AWS S3 functionality may be added more seamlessly going forward (#711).
* Switch the AWS S3 backend to `paws` (#711).

## New features

* Add a `region` argument to `tar_resources_aws()` to allow the user to explicitly declare a region for each AWS S3 buckets (@caewok, #681). Different buckets can now have different regions. This feature required modifying the metadata path for AWS storage formats. Before, the first element of the path was simply the bucket name. Now, it is internally formatted like `"bucket=BUCKET:region=REGION"`, where `BUCKET` is the user-supplied bucket name and `REGION` is the user-supplied region name. The new `targets` is back-compatible with the old metadata format, but if you run the pipeline with `targets` >= 0.8.1.9000 and then downgrade to `targets` <= 0.8.1, any AWS targets will break.
* Add new reporters `timestamp_positives"` and `"verbose_positives"` that omit messages for skipped targets (@psanker, #683).
* Implement `tar_assert_file()`.
* Implement `tar_reprex()` for creating easier reproducible examples of pipelines.
* Implement `tar_store()` to get the path to the store of the currently running pipeline (#714, @MilesMcBain).
* Automatically write a `_targets/user/` folder to encourage `gittargets` users to put custom files there for data version control.

## Bug fixes

* Make sure `tar_path()` uses the current store path of the currently running pipeline instead of `tar_config_get("store")` (#714, @MilesMcBain).

## Enhancements

* Refactor the automatic `.gitignore` file inside the data store to allow the metadata to be committed to version control more easily (#685, #711).
* Document target name requirements in `tar_target()` and `tar_target_raw()` (@tjmahr, #679).
* Catch and relay any the error if a target cannot be checked in `target_should_run.tar_builder()`. These kinds of errors sometimes come up with AWS storage.
* Fix the documentation of the reporters.
* Only write `_targets/.gitignore` for new data stores so the user can delete the `.gitignore` file without it mysteriously reappearing (#685).

# targets 0.8.1

## New features

* Add arguments `strict` and `silent` to allow `tar_load()` and `tar_load_raw()` to bypass targets that cannot be loaded.

## Enhancements

* Improve `tidyselect` docs in `tar_make()` (#640, @dewoller).
* Use namespaced call to `tar_dir()` in `tar_test()` (#642, @billdenney).
* Improve `tar_assert_target_list()` error message (@kkami1115, #654).
* Throw an informative error if a target name starts with a dot (@dipterix, #662).
* Improve help files of `tar_destroy()` and related cleanup functions (@billdenney, #675).

# targets 0.8.0

## Bug fixes

* Hash the correct files in `tar_target(target_name, ..., format = "aws_file")`. Previously, `_targets/objects/target_name` was also hashed if it existed.

## New features

* Implement a new `tar_config_unset()` function to delete one or more configuration settings from the YAML configuration file.
* Implement the `TAR_CONFIG` environment variable to set the default file path of the YAML configuration file with project settings (#622, @yyzeng, @atusy, @nsheff, @wdkrnls). If `TAR_CONFIG` is not set, the file path is still `_targets.yaml`.
* Restructure the YAML configuration file format to handle configuration information for multiple projects (using the `config` package) and support the `TAR_PROJECT` environment variable to select the current active project for a given R session. The old single-project format is gracefully deprecated (#622, @yyzeng, @atusy, @nsheff, @wdkrnls).
* Implement `retrieval = "none"` and `storage = "none"` to anticipate loading/saving targets from other languages, e.g. Julia (@MilesMcBain).
* Add a new `tar_definition()` function to get the target definition object of the current target while that target is running in a pipeline.
* If called inside an AWS target, `tar_path()` now returns the path to the staging file instead of `_targets/objects/target_name`. This ensures you can still write to `tar_path()` in `storage = "none"` targets and the package will automatically hash the right file and upload it to the cloud. (This behavior does not apply to formats `"file"` and `"aws_file"`, where it is never necessary to set `storage = "none"`.)

## Enhancements

* Use `eval(parse(text = ...), envir = tar_option_set("envir")` instead of `source()` in the `_targets.R` file for Target Markdown.
* Allow feather and parquet formats to accept objects of class `RecordBatch` and `Table` (@MilesMcBain).
* Let `knitr` load the Target Markdown engine (#469, @nviets, @yihui). Minimum `knitr` version is now `1.34`.
* In the `tar_resources_future()` help file, encourage the use of `plan` to specify resources.

# targets 0.7.0

## Bug fixes

* Ensure `error = "continue"` does not cause errored targets to have `NULL` values.
* Relay output and messages in Target Markdown interactive mode (using the R/default `knitr` engine).

## New features

* Expose the `poll_connection`, `stdout`, and `stderr` arguments of `callr::r_bg()` in `tar_watch()` (@mpadge).
* Add new helper functions to list targets in each progress category: `tar_started()`, `tar_skipped()`, `tar_built()`, `tar_canceled()`, and `tar_errored()`.
* Add new helper functions `tar_interactive()`, `tar_noninteractive()`, and `tar_toggle()` to differentially suppress code in non-interactive and interactive mode in Target Markdown (#607, @33Vito).

## Enhancements

* Handle `future` errors within targets (#570, @stuvet).
* Handle storage errors within targets (#571, @stuvet).
* In Target Markdown in non-interactive mode, suppress messages if the `message` `knitr` chunk option is `FALSE` (#574, @jmbuhr).
* In Target Markdown, if `tar_interactive` is not set, choose interactive vs non-interactive mode based on `isTRUE(getOption("knitr.in.progress"))` instead of `interactive()`.
* Convert errors loading dependencies into errors running targets (@stuvet).

# targets 0.6.0

## Bug fixes

* Allow `tar_poll()` to lose and then regain connection to the progress file.
* Make sure changes to the `tar_group` column of `iteration = "group"` data frames do not invalidate slices (#507, @lindsayplatt).

## New features

* In Target Markdown, add a new `tar_interactive` global option to select interactive mode or non-interactive mode (#469).
* Highlight a graph neighborhood when the user clicks a node. Control the neighborhood degree with new arguments `degree_from` and `degree_to` of `tar_visnetwork()` and `tar_glimpse()` (#474, @rgayler).
* Make the target script path configurable in `tar_config_set()` (#476).
* Add a `tar_script` chunk option in Target Markdown to control where the `{targets}` language engine writes the target script and helper scripts (#478).
* Add new arguments `script` and `store` to choose custom paths to the target script file and data store for individual function calls (#477).
* Allow users to set an alternative path to the YAML configuration file for the current R session (#477). Most users have no reason to set this path, it is only for niche applications like Shiny apps with `targets` backends. Unavoidably, the path gets reset to `_targets.yaml` when the session restarts.
* Add new `_targets.yaml` config options `reporter_make`, `reporter_outdated`, and `workers` to control function argument defaults shared across multiple functions called outside `_targets.R` (#498, @ianeveperry).
* Add `tar_load_globals()` for debugging, testing, prototyping, and teaching (#496, @malcolmbarrett).
* Add structure to the `resources` argument of `tar_target()` to avoid conflicts among formats and HPC backends (#489). Includes user-side helper functions like `tar_resources()` and `tar_resources_aws()` to build the required data structures.
* Log skipped targets in `_targets/meta/progress` and display then in `tar_progress()`, `tar_poll()`, `tar_watch()`, `tar_progress_branches()`, `tar_progress_summary()`, and `tar_visnetwork()` (#514). Instead of writing each skip line separately to `_targets/meta/progress`, accumulate skip lines in a queue and then write them all out in bulk when something interesting happens. This avoids a lot of overhead in certain cases.
* Add a `shortcut` argument to `tar_make()`, `tar_make_clustermq()`, `tar_make_future()`, `tar_outdated()`, and `tar_sitrep()` to more efficiently skip parts of the pipeline (#522, #523, @jennysjaarda, @MilesMcBain, @kendonB).
* Support `names` and `shortcut` in graph data frames and graph visuals (#529).
* Move `allow` and `exclude` to the network behind the graph visuals rather than the visuals themselves (#529).
* Add a new "progress" display to the `tar_watch()` app to show verbose progress info and metadata.
* Add a new `workspace_on_error` argument of `tar_option_set()` to supersede `error = "workspace"`. Helps control workspace behavior independently of the `error` argument of `tar_target()` (#405, #533, #534, @mattwarkentin, @xinstein).
* Implement `error = "abridge"` in `tar_target()` and related functions. If a target errors out with this option, the target itself stops, any currently running targets keeps, and no new targets launch after that (#533, #534, @xinstein).
* Add a menu prompt to `tar_destroy()` which can be suppressed with `TAR_ASK = "false"` (#542, @gofford).
* Support functions `tar_older()` and `tar_newer()` to help users identify and invalidate targets at regular times or intervals.

## Deprecations

* In Target Markdown, deprecate the `targets` chunk option in favor of `tar_globals` (#469).
* Deprecate `error = "workspace"` in `tar_target()` and related functions. Use `tar_option_set(workspace_on_error = TRUE)` instead (#405, #533, @mattwarkentin, @xinstein).

## Performance

* Reset the backoff upper bound when concluding a target or shutting down a `clustermq` worker (@rich-payne).
* Set more aggressive default backoff bound of 0.1 seconds (previous: 5 seconds) and set a more aggressive minimum of 0.001 seconds (previous: 0.01 seconds) (@rich-payne).
* Speed up the summary and forecast reporters by only printing to the console every quarter second.
* Avoid superfluous calls to `store_sync_file_meta.default()` on small files.
* In `tar_watch()`, take several measures to avoid long computation times rendering the graph:
    * Expose arguments `display` and `displays` to `tar_watch()` so the user can select which display shows first.
    * Make `"summary"` the default display instead of `"graph"`.
    * Set `outdated` to `FALSE` by default.

## Enhancements

* Simplify the Target Markdown example.
* Warn about unnamed chunks in Target Markdown.
* Redesign option system to be more object-oriented and rigorous. Also export most options to HPC workers (#475).
* Simplify config system to let API function arguments take control (#483).
* In `tar_read()` for targets with `format = "aws_file"`, download the file back to the path the user originally saved it when the target ran.
* Replace the `TAR_MAKE_REPORTER` environment variable with `targets::tar_config_get("reporter_make")`.
* Use `eval(parse(text = readLines("_targets.R")), envir = some_envir)` and related techniques instead of the less controllable `source()`. Expose an `envir` argument to many functions for further control over evaluation if `callr_function` is `NULL`.
* Drop `out.attrs` when hashing groups of data frames to extend #507 to `expand.grid()` (#508).
* Increase the number of characters in errors and warnings up to 2048.
* Refactor assertions to automatically generate better messages.
* Export assertions, conditions, and language utilities in packages that build on top of `targets`.
* Change `GITHUBPAT` to `GITHUB_TOKEN` in the `tar_github_actions()` YAML file (#554, @eveyp).
* Support the `eval` chunk option in Target Markdown (#552, @fkohrt).
* Record time stamps in the metadata `time` column for all builder targets, regardless of storage format.

# targets 0.5.0

## Bug fixes

* Export in-memory config settings from `_targets.yaml` to parallel workers.

## New features

* Add a limited-scope `exclude` argument to `tar_watch()` and `tar_watch_server()` (#458, @gorkang).
* Write a `.gitignore` file to ignore everything in `_targets/meta/` except `.gitignore` and `_targets/meta/meta`.
* Target Markdown: add `knitr` engines for pipeline construction and prototyping from within literate programming documents (#469, @cderv, @nviets, @emilyriederer, @ijlyttle, @GShotwell, @gadenbuie, @tomsing1). Huge thanks to @cderv on this one for answering my deluge of questions, helping me figure out what was and was not possible in `knitr`, and ultimately circling me back to a successful approach.
* Add an RStudio R Markdown template for Target Markdown (#469).
* Implement `use_targets()`, which writes the Target Markdown template to the project root (#469).
* Implement `tar_unscript()` to clean up scripts written by Target Markdown.

## Enhancements

* Enable priorities in `tar_make()` and `tar_manifest()`.
* Show the priority in the print method of stem and pattern targets.
* Throw informative errors if the secondary arguments to `pattern = slice()` or `pattern = sample()` are invalid.
* In `tar_target_raw()`, assert that commands have length 1 when converted to expressions.
* Handle errors and post failure artifacts in the Github Actions YAML file.
* Rewrite the documentation on invalidation rules in `tar_cue()` (@maelle).
* Drop `dplyr` groups and `"grouped_df"` class in `tar_group()` (`tarchetypes` discussion #53, @kendonB).
* Assign branch names to dynamic branching return values produced by `tar_read()` and `tar_read_raw()`.

# targets 0.4.2

## Bug fixes

* Do not use time stamps to monitor the config file (e.g. `_targets.yaml`). Fixes CRAN check errors from version 0.4.1.

# targets 0.4.1

* Fix CRAN test error on Windows R-devel.
* Do not inherit `roxygen2` docstrings from `shiny`.
* Handle more missing `Suggests:` packages.
* Unset the config lock before reading `targets.yaml` in the `callr` process.

# targets 0.4.0

## Bug fixes

* Avoid `file.rename()` errors when migrating staged temporary files (#410).
* Return correct error messages from feather and parquet formats (#388). Now calling `assert_df()` from `store_assert_format()` instead of `store_cast_object()`. And now those last two functions are not called at all if the target throws an error.
* Retry writing lines to database files so Windows machines can run `tar_poll()` at the same time as the pipeline (#393).
* Rename file written by `tar_renv()` to `_targets_packages.R` (#397).
* Ensure metadata is loaded to compute labels properly when `outdated = FALSE` in `tar_visnetwork()`.

## New features

* Implement `tar_timestamp()` and `tar_timestamp_raw()` to get the last modified timestamp of a target's data (#378).
* Implement `tar_progress_summary()` to compactly summarize all pipeline progress (#380).
* Add a `characters` argument of `tar_traceback()` to cap the traceback line lengths (#383).
* Add new "summary" and "about" views to `tar_watch()` (#382).
* Implement `tar_poll()` to repeatedly poll runtime progress in the R console (#381). `tar_poll()` is a lightweight alternative to `tar_watch()`.
* Change the color of the "dormant" status in the graph.
* Add a `tar_envvar()` function to list values of special environment variables supported in `targets`. The help file explains each environment variable in detail.
* Support extra project-level configuration settings with `_targets.yaml` (#297). New functions `tar_config_get()` and `tar_config_set()` interact with the `_targets.yaml` file. Currently only supports the `store` field to set the data store path to something other than `_targets/`.

## Performance

* Shut down superfluous persistent workers earlier in dynamic branching and when all remaining targets have `deployment = "main"` (#398, #399, #404, @pat-s).

## Enhancements

* Attempt to print only the useful part of the traceback in `tar_traceback()` (#383).
* Add a line break at the end of the "summary" reporter so warnings do not mangle the output.
* In `tar_watch()`, use `shinybusy` instead of `shinycssloaders` and keep current output on display while new output is rendering (#386, @rcorty).
* Right-align the headers and counts in the "summary" and "forecast" reporters.
* Add a timestamp to the "summary" reporter.
* Make the reporters show when a target ends (#391, @mattwarkentin).
* Make the reporters show when a pattern ends if the pattern built at least one target and none of the targets errored or canceled.
* Use words "start" and "built" in reporters.
* Use the region of the AWS S3 bucket instead of the local `AWS_DEFAULT_REGION` environment variable (`check_region = TRUE`; #400, @tomsing1).
* In `tar_meta()`, return `POSIXct` times in the time zone of the calling system (#131).
* Throw informative error messages when a target's name or command is missing (#413, @liutiming).
* Bring back ALTREP in `qs::qread()` now that `qs` 0.24.1 requires `stringfish` >= 1.5.0 (#147, @glep).
* Relax dynamic branching checks so `pattern = slice(...)` can take multiple indexes (#406, #419, @djbirke, @alexgphayes)

# targets 0.3.1

## Bug fixes

* `queue$enqueue()` is now `queue$prepend()` and always appends to the front of the queue (#371).

## Enhancements

* Throw a warning if `devtools::load_all()` or similar is detected inside `_targets.R` (#374).

## CRAN

* Skip `feather` and `parquet` tests on CRAN.

# targets 0.3.0

## Bug fixes

* Fix the "write target at cursor" RStudio addin and move cursor between the parentheses.

## New features

* Add a `backoff` option in `tar_option_set()` to set the maximum upper bound (seconds) for the polling interval (#333).
* Add a new `tar_github_actions()` function to write a GitHub Actions workflow file for continuous deployment of data analysis pipelines (#339, @jaredlander).
* Add a new `TAR_MAKE_REPORTER` environment variable to globally set the reporter of the `tar_make*()` functions (#345, @alexpghayes).
* Support new storage formats "feather", "parquet", "aws_feather", and "aws_parquet" (#355, @riazarbi).

## Performance

* Implement an exponential backoff algorithm for polling the priority queue in `tar_make_clustermq()` and `tar_make_future()` (#333). 
* In `tar_make_future()`, try to submit a target every time a worker is polled.
* In `tar_make_future()`, poll workers in order of target priority.
* Avoid the time delay in exiting on error (from https://github.com/r-lib/callr/issues/185).
* Clone target objects for the pipeline and scrape more `targets` internal objects out of the environment in order to avoid accidental massive data transfers to workers.

## Enhancements

* Use `rlang::check_installed()` inside `assert_package()` (#331, @malcolmbarrett).
* Allow `tar_destroy(destroy = "process")`.
* In `tar_watch()`, increase default `seconds` to 15 (previously 5).
* In `tar_watch()`, debounce instead of throttle inputs.
* In `tar_watch()`, add an action button to refresh the outputs.
* Always deduplicate metadata after `tar_make()`. Will help compute a cache key on GitHub Actions and similar services.
* Deprecate `tar_deduplicate()` due to the item above.
* Reorder information in timestamped messages.
* Document RNG seed generation in `tar_target_raw()`, `tar_meta()`, and `tar_seed()` (#357, @alexpghayes).
* Switch meaning of `%||%` and `%|||%` to conform to historical precedent.
* Only show a command line spinner if `reporter = "silent"` (#364, @matthiasgomolka).
* Target and pipeline objects no longer have an `envir` element.

# targets 0.2.0

## Bug fixes

* In `tar_load()`, subset metadata to avoid accidental attempts to load global objects in `tidyselect` calls.
* Do not register a pattern as running unless an actual branch is about to start (#304).
* Use a name spec in `vctrs::vec_c()` (#320, @joelnitta).

## New features

* Add a new `names` argument to `tar_objects()` and `tar_workspaces()` with `tidyselect` functionality.
* Record info on the main process (PID, R version, `targets` version) in `_targets/meta/process` and write new functions `tar_process()` and `tar_pid()` to retrieve the data (#291, #292).
* Add a new `targets_only` argument to `tar_meta()`.
* Add new functions `tar_helper()` and `tar_helper_raw()` to write general-purpose R scripts, using tidy evaluation for as a template mechanism (#290, #291, #292, #306).
* Export functions to check the existence of various pieces of local storage: `tar_exist_meta()`, `tar_exist_objects()`, `tar_exist_progress()`, `tar_exist_progress()`, `tar_exist_script()` (#310).
* Add a new `supervise` argument to `tar_watch()`.
* Add a new `complete_only` argument to `tar_meta()` to optionally return only complete rows (no `NA` values).
* Catch `callr` errors and refer users to the debugging chapter of the manual.

## Enhancements

* Improve error messages of invalid arguments (#298, @brunocarlin). Removes partial argument matching in most cases.
* By default, locally enable `crayon` if an only if the calling process is interactive (#302, @ginolhac). Can still be disabled with `options(crayon.enabled = FALSE)` in `_targets.R`.
* Improve error handling and message for `format = "url"` when the HTTP response status code is not 200 (#303, @petrbouchal).
* Add more `extras` packages to `tar_renv()` (to support `tar_watch()`).
* Show informative message instead of error in `tar_watch()` if `_targets.R` does not exist.
* Clear up the documentation of the `names` argument of `tar_load()` (#314, @jameelalsalam).
* Do not override `nobody` in custom `curl` handles (#315, @riazarbi).
* Rename "running" to "started" in the progress metadata. This avoids the implicit claim that `targets` is somehow actively monitoring each job, e.g. through a connection or heartbeat (#318).
* Set `errormode = "warn"` in `getVDigest()` for files to work around https://github.com/eddelbuettel/digest/issues/49 for network drives on Windows. `targets` already runs those file checks anyway. (#316, @boshek).
* If a package fails to load, print the library paths `targets` tried to load from.

# targets 0.1.0

## Bug fixes

* `tar_test()` now skips all tests on Solaris in order to fix the problems shown on the CRAN check page.
* Enable `allow` and `exclude` to work on imports in `tar_visnetwork()` and `tar_glimpse()`.
* Put `visNetwork` legends on right to avoid crowding the graph.

## Performance

* Call `force()` on subpipeline objects to eliminate high-memory promises in target objects. Allows targets to be deployed to workers much faster when `retreival` is `"main"` (#279).

## New features

* Add a new box to the `tar_watch()` app to tabulate progress on dynamic branches (#273, @mattwarkentin).
* Store `type`, `parent`, and `branches` in progress data for `tar_watch()` (#273, @mattwarkentin).
* Add a `fields` argument in `tar_progress()` and default to `"progress"` for back compatibility (#273, @mattwarkentin).
* Add a new `tar_progress_branches()` function to tabulate branch progress (#273, @mattwarkentin).
* Add new "refresh" switch to `tar_watch()` to toggle automatic refreshing and force a refresh.

## Enhancements

* Exclude `.Random.seed` by default in `tar_visnetwork()`.
* Spelling: "cancelled" changed to "canceled".
* Enhance controls and use of space in the `tar_watch()` app.
* Centralize internal path management utilities.

## Configuration

* Skip `clustermq` tests on Solaris.

# targets 0.0.2

## CRAN response

* Avoid starting the description with the package name.
* Remove `if(FALSE)` blocks from help files to fix "unexecutable code" warnings (`tar_glimpse()`, `tar_visnetwork()`, and `tar_watch()`).
* Remove commented code in the examples (`tar_edit()`, `tar_watch_ui()`, and `tar_watch_server()`).
* Ensure that all examples, tests, and vignettes do not write to the user's home file space. (Fixed an example of `tar_workspace()`.)

## Enhancements

* Use JOSS paper in `CITATION`.

# targets 0.0.1

## Enhancements

* Accept lists of target objects at the end of `_targets.R` (#253).
* Deprecate `tar_pipeline()` and `tar_bind()` because of the above (#253).
* Always show a special message when the pipeline finishes (#258, @petrbouchal).
* Disable `visNetwork` stabilization (#264, @mattwarkentin).
* Use default `visNetwork` font size.
* Relay errors as condition messages if `error` is `"continue"` (#267, @liutiming).

# targets 0.0.0.9003

## Bug fixes

* Ensure pattern-only pipelines can be defined so they can be combined again later with `tar_bind()` (#245, @yonicd).
* Implement safeguards around `igraph` topological sort.

## Enhancements

* Topologically sort the rows of `tar_manifest()` (#263, @sctyner).

## Breaking changes

* Make patterns composable (#212, @glep, @djbirke).
* Allow workspaces to load nonexportable objects (#214).
* Make workspace files super light by saving only a reference to the required dependencies (#214).
* Add a new `workspaces` argument to `tar_option_set()` to specify which targets will save their workspace files during `tar_make()` (#214).
* Change `error = "save"` to `error = "workspace"` to so it is clearer that saving workspaces no longer duplicates data (#214).
* Rename `what` to `destroy` in `tar_destroy()`.
* Remove `tar_undebug()` because is redundant with `tar_destroy(destroy = "workspaces")`.

## New features

* Make patterns composable (#212).
* Add new dynamic branching patterns `head()`, `tail()`, and `sample()` to provide functionality equivalent to `drake`'s `max_expand` (#56).
* Add a new `tar_pattern()` function to emulate dynamic branching outside a pipeline.
* Add a new `level_separation` argument to `tar_visnetwork()` and `tar_glimpse()` to control the aspect ratio (#226).
* Track functions from multiple packages with the `imports` argument to `tar_option_set()` (#239).
* Add color for "built" progress if `outdated` is `FALSE` in `tar_visnetwork()`.
* Tweak colors in `tar_visnetwork()` to try to account for color blindness.

## Enhancements

* Return full patterns from `tar_manifest()`.
* Record package load errors in progress and metadata (#228, @psychelzh).
* `tar_renv()` now invokes `_targets.R` through a background process just like `tar_outdated()` etc. so it can account for more hidden packages (#224, @mattwarkentin).
* Set `deployment` equal to `"main"` for all targets in `tar_make()`. This ensures `tar_make()` does not waste time waiting for nonexistent files to ship over a nonexistent network file system (NFS). `tar_make_clustermq()` or `tar_make_future()` could use NFS, so they still leave `deployment` alone.

# targets 0.0.0.9002

## Breaking changes

* Add a new `size` field to the metadata to allow `targets` to make better judgments about when to rehash files (#180). We now compare hashes to check file size differences instead of doing messy floating point comparisons with ad hoc tolerances. It breaks back compatibility with old projects, but the error message is informative, and this is all still before the first official release.
* Change "local" to "main" and "remote" to "worker" in the `storage`, `retrieval`, and `deployment` settings (#183, @mattwarkentin).
* Ensure function dependencies are sorted before computing the function hash (GitHub commit f15face7d72c15c2d1098da959492bdbfcddb425).
* Move `garbage_collection` to a target-level setting, i.e. argument to `tar_target()` and `tar_option_set()` (#194). Previously was an argument to the `tar_make*()` functions.
* Allow `tar_name()` and `tar_path()` to run outside the pipeline with debugging-friendly default return values.

## Bug fixes

* Stop sending target return values over the network when `storage` is `"remote"` (#182, @mattwarkentin).
* Shorten lengths of warnings and error messages to 128 characters (#186, @gorkang).
* Restrict in-memory metadata to avoid incorrectly recycling deleted targets (#191).
* Marshal nonexportable dependencies before sending them to workers. Transport data through `target$subpipeline` rather than `target$cache` to make that happen (#209, @mattwarkentin).

## New features

* Add a new function `tar_bind()` to combine pipeline objects.
* Add `tar_seed()` to get the random number generator seed of the target currently running.

## Enhancements

* Allow target-specific `future::plan()`s through the `resources` argument of `tar_target()` (#198, @mattwarkentin).
* Use `library()` instead of `require()` in `command_load_packages()`.
* Evaluate commands directly in `targets$cache$targets$envir` to improve convenience in interactive debugging (`ls()` just works now.) This is reasonably safe now that the cache is populated at the last minute and cleared as soon as possible (#209, #210).

# targets 0.0.0.9000

* First version.
