# Use sparingly to minimize AWS costs.
# And afterwards, manually verify that all the buckets are gone.
tar_test("dynamic branch references have complete path info", {
  skip_if_no_aws()
  s3 <- paws.storage::s3()
  bucket_name <- random_bucket_name()
  s3$create_bucket(Bucket = bucket_name)
  on.exit(aws_s3_delete_bucket(bucket_name))
  code <- substitute(
    {
      library(ggplot2)
      library(targets)
      library(tarchetypes)
      library(tibble)
      tar_option_set(
        repository = "aws",
        resources = tar_resources(
          aws = tar_resources_aws(
            prefix = "_targets",
            bucket = bucket_name
          )
        )
      )
      # From
      # https://github.com/wjschne/spiro/blob/ \
      #   87f73ec37ceb0a7a9d09856ada8ae28d587a2ebd/R/spirograph.R
      # Adapted under the CC0 1.0 Universal license:
      # https://github.com/wjschne/spiro/blob/ \
      #   87f73ec37ceb0a7a9d09856ada8ae28d587a2ebd/LICENSE.md
      spirograph_points <- function(fixed_radius, cycling_radius) {
        t <- seq(1, 30 * pi, length.out = 1e4)
        diff <- (fixed_radius - cycling_radius)
        ratio <- diff / cycling_radius
        x <- diff * cos(t) + cos(t * ratio)
        y <- diff * sin(t) - sin(t * ratio)
        tibble(
          x = x,
          y = y,
          fixed_radius = fixed_radius,
          cycling_radius = cycling_radius
        )
      }
      plot_spirographs <- function(points) {
        label <- "fixed_radius = %s, cycling_radius = %s"
        points$parameters <- sprintf(
          label,
          points$fixed_radius,
          points$cycling_radius
        )
        ggplot(points) +
          geom_point(aes(x = x, y = y, color = parameters), size = 0.1) +
          facet_wrap(~parameters) +
          theme_gray(16) +
          guides(color = "none")
      }
      list(
        tar_target(fixed_radius, sample.int(n = 10, size = 2)),
        tar_target(cycling_radius, sample.int(n = 10, size = 2)),
        tar_target(
          points,
          spirograph_points(fixed_radius, cycling_radius),
          pattern = map(fixed_radius, cycling_radius)
        ),
        tar_target(
          single_plot,
          plot_spirographs(points),
          pattern = map(points),
          iteration = "list"
        ),
        tar_target(
          combined_plot,
          plot_spirographs(points)
        )
      )
    },
    env = list(bucket_name = bucket_name)
  )
  do.call(tar_script, list(code = code))
  for (index in seq_len(2L)) {
    tar_make(reporter = "silent")
    expect_s3_class(tar_read(combined_plot), "ggplot")
  }
})
