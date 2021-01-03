test_that("keras and future with main storage and retrieval", {
  # Requires Python TensorFlow and Keras.
  # Cannot test inside the RStudio IDE.
  unlink("_targets", recursive = TRUE)
  on.exit(unlink("_targets", recursive = TRUE))
  skip_if_not_installed("future")
  skip_if_not_installed("keras")
  on.exit(future::plan(future::sequential), add = TRUE)
  future::plan(future::multisession)
  old_envir <- tar_option_get("envir")
  on.exit(tar_option_set(envir = old_envir), add = TRUE)
  envir <- new.env(parent = globalenv())
  tar_option_set(envir = envir)
  envir$f <- function() {
    model <- keras::keras_model_sequential() %>%
      keras::layer_conv_2d(
        filters = 32,
        kernel_size = c(3, 3),
        activation = "relu",
        input_shape = c(28, 28, 1)
      ) %>%
      keras::layer_conv_2d(
        filters = 64,
        kernel_size = c(3, 3),
        activation = "relu"
      ) %>%
      keras::layer_max_pooling_2d(pool_size = c(2, 2)) %>%
      keras::layer_dropout(rate = 0.25) %>%
      keras::layer_flatten() %>%
      keras::layer_dense(units = 128, activation = "relu") %>%
      keras::layer_dropout(rate = 0.5) %>%
      keras::layer_dense(units = 10, activation = "softmax")
    keras::compile(
      model,
      loss = "categorical_crossentropy",
      optimizer = keras::optimizer_adadelta(),
      metrics = "accuracy"
    )
    model
  }
  x <- tar_target_raw(
    name = "abc",
    command = quote(f()),
    format = "keras"
  )
  pipeline <- pipeline_init(list(x))
  future <- future_init(pipeline)
  future$run()
  expect_true(
    inherits(
      target_read_value(pipeline_get_target(pipeline, "abc"))$object,
      "keras.engine.training.Model"
    )
  )
})

test_that("keras and future with worker storage and retrieval", {
  # Requires Python TensorFlow and Keras.
  # Start up a new process for this one.
  # Also cannot test inside the RStudio IDE.
  unlink("_targets", recursive = TRUE)
  on.exit(unlink("_targets", recursive = TRUE))
  skip_if_not_installed("future")
  skip_if_not_installed("keras")
  on.exit(future::plan(future::sequential), add = TRUE)
  future::plan(future::multisession)
  old_envir <- tar_option_get("envir")
  on.exit(tar_option_set(envir = old_envir), add = TRUE)
  envir <- new.env(parent = globalenv())
  tar_option_set(envir = envir)
  envir$f <- function() {
    model <- keras::keras_model_sequential() %>%
      keras::layer_conv_2d(
        filters = 32,
        kernel_size = c(3, 3),
        activation = "relu",
        input_shape = c(28, 28, 1)
      ) %>%
      keras::layer_conv_2d(
        filters = 64,
        kernel_size = c(3, 3),
        activation = "relu"
      ) %>%
      keras::layer_max_pooling_2d(pool_size = c(2, 2)) %>%
      keras::layer_dropout(rate = 0.25) %>%
      keras::layer_flatten() %>%
      keras::layer_dense(units = 128, activation = "relu") %>%
      keras::layer_dropout(rate = 0.5) %>%
      keras::layer_dense(units = 10, activation = "softmax")
    keras::compile(
      model,
      loss = "categorical_crossentropy",
      optimizer = keras::optimizer_adadelta(),
      metrics = "accuracy"
    )
    model
  }
  x <- tar_target_raw(
    name = "abc",
    command = quote(f()),
    format = "keras",
    storage = "worker",
    retrieval = "worker"
  )
  pipeline <- pipeline_init(list(x))
  future <- future_init(pipeline)
  future$run()
  expect_true(
    inherits(
      target_read_value(pipeline_get_target(pipeline, "abc"))$object,
      "keras.engine.training.Model"
    )
  )
})
