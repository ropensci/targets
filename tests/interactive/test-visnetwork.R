# Should show a glimpse.
net <- glimpse_init(pipeline_cross())
vis <- visnetwork_init(network = net, degree_from = 1, degree_to = 1)
vis$update()
# Click on the nodes and see highlighted neighborhoods of degree 1.
vis$visual

# Should show an inspection with everything outdated.
net <- inspection_init(pipeline_cross())
vis <- visnetwork_init(network = net, degree_from = 2, degree_to = 0)
vis$update()
# Click on the nodes and see highlighted neighborhoods
# of upstream degree 2 and downstream degree 0.
vis$visual

# Same but with everything queued.
net <- inspection_init(pipeline_cross(), outdated = FALSE)
vis <- visnetwork_init(network = net, degree_from = 2, degree_to = 0)
vis$update()
vis$visual

# Should show an inspection with everything up to date
local_init(pipeline = pipeline_cross(), reporter = "summary")$run()
net <- inspection_init(pipeline_cross())
vis <- visnetwork_init(network = net)
vis$update()
vis$visual

# Same with everything completed.
net <- inspection_init(pipeline_cross(), outdated = FALSE)
vis <- visnetwork_init(network = net)
vis$update()
vis$visual

# Same with everything skipped.
local_init(pipeline = pipeline_cross(), reporter = "summary")$run()
net <- inspection_init(pipeline_cross(), outdated = FALSE)
vis <- visnetwork_init(network = net)
vis$update()
vis$visual

# Should show all optional labels.
tar_script({
  list(
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
vis <- visnetwork_init(network = net, label = c("time", "branches"))
vis$update()
vis$visual

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
vis <- visnetwork_init(network = net)
vis$update()
vis$visual

# Should show an empty widget.
net <- glimpse_init(pipeline_init())
vis <- visnetwork_init(network = net)
vis$update()
vis$visual

# Should show one dispatched target.
tar_option_set(envir = new.env(parent = globalenv()))
x <- target_init("w", quote(Sys.sleep(100))) # Sleep for a long time.
pipeline <- pipeline_init(list(x))
local_init(pipeline = pipeline)$run() # Manually cancel (ESC or CTRL-C)
x <- target_init("w", quote(Sys.sleep(100)))
pipeline <- pipeline_init(list(x))
net <- inspection_init(pipeline)
vis <- visnetwork_init(network = net)
vis$update()
vis$visual

# Should show one canceled target.
pipeline <- pipeline_init(list(target_init("w", quote(targets::tar_cancel()))))
local_init(pipeline = pipeline)$run()
pipeline <- pipeline_init(list(target_init("w", quote(targets::tar_cancel()))))
net <- inspection_init(pipeline)
vis <- visnetwork_init(network = net)
vis$update()
vis$visual

# Should show one errored target.
x <- target_init("x", quote(stop("123")))
pipeline <- pipeline_init(list(x))
local_init(pipeline = pipeline)$run()
x <- target_init("x", quote(stop("123")))
pipeline <- pipeline_init(list(x))
net <- inspection_init(pipeline)
vis <- visnetwork_init(network = net)
vis$update()
vis$visual

# Should still show one errored target.
tar_destroy()
x <- target_init("x", quote(stop("123")), error = "continue")
pipeline <- pipeline_init(list(x))
local_init(pipeline = pipeline)$run()
x <- target_init("x", quote(stop("123")), error = "continue")
pipeline <- pipeline_init(list(x))
net <- inspection_init(pipeline)
vis <- visnetwork_init(network = net)
vis$update()
vis$visual

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
vis <- visnetwork_init(network = net)
vis$update()
vis$visual

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
vis <- visnetwork_init(network = net)
vis$update()
vis$visual

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
vis <- visnetwork_init(network = net)
vis$update()
vis$visual

# Should show a glimpse of three targets.
tar_script({
  g <- function(x) x - 1
  f <- function(x) g(x) + 1
  list(
    tar_target(y1, f(1)),
    tar_target(y2, 1 + 1),
    tar_target(z, y1 + y2)
  )
})
tar_glimpse()

# Should show a glimpse of just y1 and y2.
tar_glimpse(allow = starts_with("y"))

# Should show a glimpse of just z.
tar_glimpse(exclude = starts_with("y"))

# Should show a glimpse of y1, y2, and z.
tar_glimpse(names = starts_with("z"))

# Should show a glimpse of just z.
tar_glimpse(names = starts_with("z"), shortcut = TRUE)

# Should show a graph of 3 targets and f() and g().
tar_visnetwork(targets_only = FALSE)

# Should show a graph of 3 targets, f(), g(), and miscellaneous globals.
tar_visnetwork(targets_only = FALSE, callr_function = NULL)

# Should show a status of targets as queued (light gray).
tar_visnetwork(targets_only = FALSE, outdated = FALSE)

# Should show a graph of just y1 and y2.
tar_visnetwork(allow = starts_with("y"))

# Should show a graph of z, f, and g.
tar_visnetwork(exclude = starts_with("y"))

# Should show a graph of everything.
tar_visnetwork(names = starts_with("z"))

# Should show a graph of just z.
tar_make()
tar_visnetwork(names = starts_with("z"), shortcut = TRUE)

# Should show status queued (light gray).
tar_destroy()
tar_visnetwork(outdated = FALSE)

# Should show a canceled target.
tar_script(list(tar_target(y1, tar_cancel())))
tar_make()
tar_visnetwork(outdated = FALSE)

# Neighborhoods
tar_script({
  g <- function(x) x - 1
  f <- function(x) g(x) + 1
  list(
    tar_target(y1, f(1)),
    tar_target(y2, 1 + 1),
    tar_target(z, y1 + y2),
    tar_target(a, z),
    tar_target(b, a),
    tar_target(c, a),
    tar_target(d, c),
    tar_target(e, d)
  )
})

# Click for highlighted neighborhoods
# of upstream degree 1 and downstream degree 2.
tar_glimpse(degree_to = 2)

# Same with tar_visnetwork()
tar_visnetwork(degree_to = 2)

# Should slow down zoom speed.
tar_glimpse(zoom_speed = 0.2)

# Should slow down zoom speed.
tar_visnetwork(zoom_speed = 0.2)

unlink("_targets.R")
unlink("_targets", recursive = TRUE)
