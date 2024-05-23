The `tar_watch()` app visualizes the status and progress of a `targets` pipeline. 

## Displays

* __summary__: overall runtime progress summary of the pipeline. In the table, the "time" column shows the last time a target started, finished, errored, or canceled itself. The "since" columns shows how long ago that was.
* __branches__: like the summary view except with specific information about dynamic branching progress.
* __progress__: a large searchable table of progress information and metadata.
* __graph__: show the `tar_visnetwork()` dependency graph. If this graph may be slow to refresh, consider toggling the outdated switch, speeding up your `_targets.R` file, or selecting another view.

## Refresh settings

* __Refresh once__: click this button to force the displays in the app to refresh.
* __Refresh periodically__: toggle whether the app refreshes automatically every few seconds.
* __Refresh seconds__: how often to refresh the displays in the app when the "Refresh periodically" switch is turned on.

## Graph settings

* __targets_only__: whether to show only the targets or also show functions and other global objects.
* __outdated__: whether to color-code nodes depending on whether they are up to date. This feature may cause if the number of targets is enormous, so you may consider turning it off to just color by runtime progress in some cases.
* __label__: labels to append to the node names to optionally show the size, runtime, and number of branches of targets based on past recorded info in the metadata.

* __level_separation__: how wide the graph should be.
