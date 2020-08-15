# Should show a glimpse.
net <- glimpse_init(pipeline_cross())
vis <- visual_init(network = net)
vis$update()
vis$visnetwork

# Should show an inspection with everything outdated.
net <- inspection_init(pipeline_cross())
vis <- visual_init(network = net)
vis$update()
vis$visnetwork

# Should show an inspection with everything up to date
local_init(pipeline = pipeline_cross(), reporter = "silent")$run()
net <- inspection_init(pipeline_cross())
vis <- visual_init(network = net)
vis$update()
vis$visnetwork

# Should show all optional labels.
tar_script({
  tar_pipeline(
    tar_target(x, seq_len(3)),
    tar_target(y, x, pattern = map(x)),
    tar_target(z, y, pattern = map(y))
  )
})
tar_make()
tar_visnetwork(label = c("time", "size", "branches"))

# Same, but no labels.
tar_visnetwork()

# Labels for time and branches
net <- inspection_init(pipeline_cross())
vis <- visual_init(network = net, label = c("time", "size", "branches"))
vis$update()
vis$visnetwork

# Should show everything at h and downstream outdated
# and everything else up to date.
envir <- new.env(parent = baseenv())
evalq({
  f <- function(x) g(x) + h
  g <- function(x) i
  h <- 1
  i <- 1
}, envir = envir)
pipeline <- pipeline_init(
  list(
    target_init("w", quote(1), envir = envir),
    target_init("x", quote(f(1)), envir = envir),
    target_init("y", quote(g(2)), envir = envir),
    target_init("z", quote(x + y), envir = envir)
  )
)
local_init(pipeline)$run()
evalq(h <- 2, envir = envir)
pipeline <- pipeline_init(
  list(
    target_init("w", quote(1), envir = envir),
    target_init("x", quote(f(1)), envir = envir),
    target_init("y", quote(g(2)), envir = envir),
    target_init("z", quote(x + y), envir = envir)
  )
)
net <- inspection_init(pipeline)
vis <- visual_init(network = net)
vis$update()
vis$visnetwork

# Should show an empty widget.
net <- glimpse_init(pipeline_init())
vis <- visual_init(network = net)
vis$update()
vis$visnetwork

# Should show one running target.
x <- target_init("w", quote(Sys.sleep(100))) # Sleep for a long time.
pipeline <- pipeline_init(list(x))
# Manually cancel (ESC or CTRL-C):
local_init(pipeline = pipeline)$run()
x <- target_init("w", quote(Sys.sleep(100)))
pipeline <- pipeline_init(list(x))
net <- inspection_init(pipeline)
vis <- visual_init(network = net)
vis$update()
vis$visnetwork

# Should show one canceled target.
pipeline <- pipeline_init(list(target_init("w", quote(targets::tar_cancel()))))
local_init(pipeline = pipeline)$run()
pipeline <- pipeline_init(list(target_init("w", quote(targets::tar_cancel()))))
net <- inspection_init(pipeline)
vis <- visual_init(network = net)
vis$update()
vis$visnetwork

# Should show one errored target.
x <- target_init("x", quote(stop("123")))
pipeline <- pipeline_init(list(x))
local_init(pipeline = pipeline)$run()
x <- target_init("x", quote(stop("123")))
pipeline <- pipeline_init(list(x))
net <- inspection_init(pipeline)
vis <- visual_init(network = net)
vis$update()
vis$visnetwork

# Should still show one errored target.
tar_destroy()
x <- target_init("x", quote(stop("123")), error = "continue")
pipeline <- pipeline_init(list(x))
local_init(pipeline = pipeline)$run()
x <- target_init("x", quote(stop("123")), error = "continue")
pipeline <- pipeline_init(list(x))
net <- inspection_init(pipeline)
vis <- visual_init(network = net)
vis$update()
vis$visnetwork

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
vis <- visual_init(network = net)
vis$update()
vis$visnetwork

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
vis <- visual_init(network = net)
vis$update()
vis$visnetwork


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
vis <- visual_init(network = net)
vis$update()
vis$visnetwork

# Should show a glimpse of three targets.
tar_script({
  g <- function(x) x - 1
  f <- function(x) g(x) + 1
  tar_pipeline(
    tar_target(y1, f(1)),
    tar_target(y2, 1 + 1),
    tar_target(z, y1 + y2)
  )
})
tar_glimpse()

# Should show a glimpse of just y1 and y2.
tar_glimpse(allow = starts_with("y"))

# Should show a graph of 3 targets and f() and g().
tar_visnetwork(targets_only = FALSE)

# Should show a graph of 3 targets, f(), and miscellaneous globals.
tar_visnetwork(targets_only = FALSE, callr_function = NULL)

# Should show a status of targets as undefined (gray).
tar_visnetwork(targets_only = FALSE, outdated = FALSE)

# Should show a graph of just y1 and y2.
tar_visnetwork(allow = starts_with("y"))

# Should show status undefined (gray).
tar_visnetwork(outdated = FALSE)

# Should show a canceled target.
tar_script(tar_pipeline(tar_target(y1, tar_cancel())))
tar_make()
tar_visnetwork(outdated = FALSE)
           
unlink("_targets.R")
unlink("_targets", recursive = TRUE)
