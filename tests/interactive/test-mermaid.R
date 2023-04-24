mermaid <- function(x) {
  DiagrammeR::mermaid(paste0(x, collapse = "\n"))
}

# Should show a glimpse.
net <- glimpse_init(pipeline_cross())
vis <- mermaid_init(network = net)
vis$update()
mermaid(vis$visual)

# Should show an inspection with everything outdated.
net <- inspection_init(pipeline_cross())
vis <- mermaid_init(network = net)
vis$update()
mermaid(vis$visual)

# Same but with everything queued.
net <- inspection_init(pipeline_cross(), outdated = FALSE)
vis <- mermaid_init(network = net)
vis$update()
mermaid(vis$visual)

# Should show an inspection with everything up to date
local_init(pipeline = pipeline_cross(), reporter = "summary")$run()
net <- inspection_init(pipeline_cross())
vis <- mermaid_init(network = net)
vis$update()
mermaid(vis$visual)

# Same with everything built.
net <- inspection_init(pipeline_cross(), outdated = FALSE)
vis <- mermaid_init(network = net)
vis$update()
mermaid(vis$visual)

# Same with everything skipped.
local_init(pipeline = pipeline_cross(), reporter = "summary")$run()
net <- inspection_init(pipeline_cross(), outdated = FALSE)
vis <- mermaid_init(network = net)
vis$update()
mermaid(vis$visual)

# Should show all optional labels.
tar_script({
  list(
    tar_target(x, seq_len(3)),
    tar_target(y, x, pattern = map(x)),
    tar_target(z, y, pattern = map(y))
  )
})
tar_make()
mermaid(tar_mermaid(label = c("time", "size", "branches")))

# Same, but no labels.
mermaid(tar_mermaid())

# Labels for time and branches
net <- inspection_init(pipeline_cross())
vis <- mermaid_init(network = net, label = c("time", "branches"))
vis$update()
mermaid(vis$visual)

# Should show everything at h and downstream outdated
# and everything else up to date.
envir <- new.env(parent = baseenv())
evalq({
  f <- function(x) g(x) + h
  g <- function(x) i
  h <- 1
  i <- 1
}, envir = envir)
tar_option_set(envir = envir)
pipeline <- pipeline_init(
  list(
    target_init("w", quote(1)),
    target_init("x", quote(f(1))),
    target_init("y", quote(g(2))),
    target_init("z", quote(x + y))
  )
)
local_init(pipeline)$run()
evalq(h <- 2, envir = envir)
pipeline <- pipeline_init(
  list(
    target_init("w", quote(1)),
    target_init("x", quote(f(1))),
    target_init("y", quote(g(2))),
    target_init("z", quote(x + y))
  )
)
net <- inspection_init(pipeline)
vis <- mermaid_init(network = net)
vis$update()
mermaid(vis$visual)

# Should show an empty widget.
net <- glimpse_init(pipeline_init())
vis <- mermaid_init(network = net)
vis$update()
mermaid(vis$visual)

# Should show one started target.
tar_option_set(envir = new.env(parent = globalenv()))
x <- target_init("w", quote(Sys.sleep(100))) # Sleep for a long time.
pipeline <- pipeline_init(list(x))
local_init(pipeline = pipeline)$run() # Manually cancel (ESC or CTRL-C)
x <- target_init("w", quote(Sys.sleep(100)))
pipeline <- pipeline_init(list(x))
net <- inspection_init(pipeline)
vis <- mermaid_init(network = net)
vis$update()
mermaid(vis$visual)

# Should show one canceled target.
pipeline <- pipeline_init(list(target_init("w", quote(targets::tar_cancel()))))
local_init(pipeline = pipeline)$run()
pipeline <- pipeline_init(list(target_init("w", quote(targets::tar_cancel()))))
net <- inspection_init(pipeline)
vis <- mermaid_init(network = net)
vis$update()
mermaid(vis$visual)

# Should show one errored target.
x <- target_init("x", quote(stop("123")))
pipeline <- pipeline_init(list(x))
local_init(pipeline = pipeline)$run()
x <- target_init("x", quote(stop("123")))
pipeline <- pipeline_init(list(x))
net <- inspection_init(pipeline)
vis <- mermaid_init(network = net)
vis$update()
mermaid(vis$visual)

# Should still show one errored target.
tar_destroy()
x <- target_init("x", quote(stop("123")), error = "continue")
pipeline <- pipeline_init(list(x))
local_init(pipeline = pipeline)$run()
x <- target_init("x", quote(stop("123")), error = "continue")
pipeline <- pipeline_init(list(x))
net <- inspection_init(pipeline)
vis <- mermaid_init(network = net)
vis$update()
mermaid(vis$visual)

# Should show one errored map and one up-to-date stem.
tar_destroy()
w <- target_init("w", quote(seq_len(2)))
x <- target_init(
  "x",
  quote(stopifnot(w < 1.5)),
  pattern = quote(map(w)),
  error = "continue"
)
pipeline <- pipeline_init(list(w, x))
local_init(pipeline = pipeline)$run()
w <- target_init("w", quote(seq_len(2)))
x <- target_init(
  "x",
  quote(stopifnot(w < 1.5)),
  pattern = quote(map(w)),
  error = "continue"
)
pipeline <- pipeline_init(list(w, x))
net <- inspection_init(pipeline)
vis <- mermaid_init(network = net)
vis$update()
mermaid(vis$visual)

# Should still show one errored map and one up-to-date stem.
tar_destroy()
w <- target_init("w", quote(seq_len(2)))
x <- target_init(
  "x",
  quote(stopifnot(w < 1.5)),
  pattern = quote(map(w))
)
pipeline <- pipeline_init(list(w, x))
local_init(pipeline = pipeline)$run()
w <- target_init("w", quote(seq_len(2)))
x <- target_init(
  "x",
  quote(stopifnot(w < 1.5)),
  pattern = quote(map(w)),
  error = "continue"
)
pipeline <- pipeline_init(list(w, x))
net <- inspection_init(pipeline)
vis <- mermaid_init(network = net)
vis$update()
mermaid(vis$visual)

# Should one canceled map and one up-to-date stem.
tar_destroy()
w <- target_init("w", quote(seq_len(2)))
x <- target_init(
  "x",
  quote(targets::tar_cancel(w < 1.5)),
  pattern = quote(map(w))
)
pipeline <- pipeline_init(list(w, x))
local_init(pipeline = pipeline)$run()
w <- target_init("w", quote(seq_len(2)))
x <- target_init(
  "x",
  quote(stopifnot(w < 1.5)),
  pattern = quote(map(w)),
  error = "continue"
)
pipeline <- pipeline_init(list(w, x))
net <- inspection_init(pipeline)
vis <- mermaid_init(network = net)
vis$update()
mermaid(vis$visual)

# Should show three targets.
tar_script({
  g <- function(x) x - 1
  f <- function(x) g(x) + 1
  list(
    tar_target(y1, f(1)),
    tar_target(y2, 1 + 1),
    tar_target(z, y1 + y2)
  )
})
mermaid(tar_mermaid())

# Should show a just y1 and y2.
mermaid(tar_mermaid(allow = starts_with("y")))

# Should show just z.
mermaid(tar_mermaid(exclude = starts_with("y"), targets_only = TRUE))

# Should show y1, y2, and z.
mermaid(tar_mermaid(names = starts_with("z"), targets_only = TRUE))

# Should show just z.
tar_make()
mermaid(tar_mermaid(names = starts_with("z"), shortcut = TRUE))

# Should show a graph of 3 targets and f() and g().
mermaid(tar_mermaid(targets_only = FALSE))

# Should show a graph of 3 targets, f(), g(), and miscellaneous globals.
mermaid(tar_mermaid(targets_only = FALSE, callr_function = NULL))

# Should show a status of targets as built.
mermaid(tar_mermaid(targets_only = FALSE, outdated = FALSE))

# Should show a graph of just y1 and y2.
mermaid(tar_mermaid(allow = starts_with("y")))

# Should show a graph of z, f, and g.
mermaid(tar_mermaid(exclude = starts_with("y")))

# Should show a graph of everything.
mermaid(tar_mermaid(names = starts_with("z")))

# Should show a graph of just z.
tar_make()
mermaid(tar_mermaid(names = starts_with("z"), shortcut = TRUE))

# Should show status queued (light gray).
tar_destroy()
tar_mermaid(outdated = FALSE)

# Should show a canceled target.
tar_script(list(tar_target(y1, tar_cancel())))
tar_make()
mermaid(tar_mermaid(outdated = FALSE))

# Clean up.
unlink("_targets.R")
unlink("_targets", recursive = TRUE)
