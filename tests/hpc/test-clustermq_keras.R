tar_test("keras and clustermq with main storage and retrieval", {
  # Requires Python TensorFlow and Keras.
  # Cannot test inside the RStudio IDE.
  skip_on_os("windows")
  skip_if_not_installed("clustermq")
  skip_if_not_installed("keras")
  tar_runtime$set_fun("tar_make_clustermq")
  on.exit(tar_runtime$unset_fun())
  tar_script({
    tar_option_set(packages = "keras")
    options(clustermq.scheduler = "multicore")
    f <- function() {
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
    list(
      tar_target_raw(
        name = "abc",
        command = quote(f()),
        format = "keras"
      )
    )
  })
  tar_make_clustermq()
  expect_true(inherits(tar_read(abc), "keras.engine.training.Model"))
})

tar_test("keras and clustermq with worker storage and retrieval", {
  # Requires Python TensorFlow and Keras.
  # Cannot test inside the RStudio IDE.
  skip_on_os("windows")
  skip_if_not_installed("clustermq")
  skip_if_not_installed("keras")
  tar_runtime$set_fun("tar_make_clustermq")
  on.exit(tar_runtime$unset_fun())
  tar_script({
    tar_option_set(packages = "keras")
    options(clustermq.scheduler = "multicore")
    f <- function() {
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
    list(
      tar_target_raw(
        name = "abc",
        command = quote(f()),
        format = "keras",
        storage = "worker",
        retrieval = "worker"
      )
    )
  })
  tar_make_clustermq()
  expect_true(inherits(tar_read(abc), "keras.engine.training.Model"))
})
