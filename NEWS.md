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
* Allow workspaces to load unexportable objects (#214).
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
* Serialize unexportable dependencies before sending them to workers. Transport data through `target$subpipeline` rather than `target$cache` to make that happen (#209, @mattwarkentin).

## New features

* Add a new function `tar_bind()` to combine pipeline objects.
* Add `tar_seed()` to get the random number generator seed of the target currently running.

## Enhancements

* Allow target-specific `future::plan()`s through the `resources` argument of `tar_target()` (#198, @mattwarkentin).
* Use `library()` instead of `require()` in `command_load_packages()`.
* Evaluate commands directly in `targets$cache$targets$envir` to improve convenience in interactive debugging (`ls()` just works now.) This is reasonably safe now that the cache is populated at the last minute and cleared as soon as possible (#209, #210).

# targets 0.0.0.9000

* First version.
