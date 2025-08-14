pipeline_order <- function() {
  pipeline_init(
    list(
      tar_target_raw(name = "data1", command = quote(seq_len(10))),
      tar_target_raw(name = "data2", command = quote(seq_len(20))),
      tar_target_raw(name = "min1", command = quote(min(data1))),
      tar_target_raw(name = "min2", command = quote(min(data2))),
      tar_target_raw(name = "max1", command = quote(max(data1))),
      tar_target_raw(name = "max2", command = quote(max(data2))),
      tar_target_raw(name = "mins", command = quote(c(min1, min2))),
      tar_target_raw(name = "maxes", command = quote(c(max1, max2))),
      tar_target_raw(name = "all", command = quote(c(mins, maxes)))
    )
  )
}

pipeline_map <- function(storage = "main") {
  pipeline_init(
    list(
      tar_target_raw(
        name = "data0",
        command = quote(c(1L + 1L)),
        storage = storage
      ),
      tar_target_raw(
        name = "data1",
        command = quote(seq_len(3L)),
        storage = storage
      ),
      tar_target_raw(
        name = "data2",
        command = quote(seq_len(3L) + 3L),
        storage = storage
      ),
      tar_target_raw(
        name = "map1",
        command = quote(data1 + sum(data0)),
        pattern = quote(map(data1)),
        storage = storage
      ),
      tar_target_raw(
        name = "map2",
        command = quote(data1 + data2),
        pattern = quote(map(data1, data2)),
        storage = storage
      ),
      tar_target_raw(
        name = "map3",
        command = quote(map1 + 1L),
        pattern = quote(map(map1)),
        storage = storage
      ),
      tar_target_raw(
        name = "map4",
        command = quote(map1 + map2),
        pattern = quote(map(map1, map2)),
        storage = storage
      ),
      tar_target_raw(
        name = "map5",
        command = quote(map1 + data2),
        pattern = quote(map(map1, data2)),
        storage = storage
      ),
      tar_target_raw(
        name = "map6",
        command = quote(sum(map1) + sum(data2)),
        pattern = quote(map(data2)),
        storage = storage
      )
    )
  )
}

pipeline_cross <- function() {
  pipeline_init(
    list(
      tar_target_raw(
        name = "data1",
        command = quote(seq_len(3))
      ),
      tar_target_raw(
        name = "data2",
        command = quote(rev(seq_len(3)))
      ),
      tar_target_raw(
        name = "map1",
        command = quote(data1 + 10L),
        pattern = quote(map(data1))
      ),
      tar_target_raw(
        name = "cross1",
        command = quote(data1 + 1L),
        pattern = quote(cross(data1))
      ),
      tar_target_raw(
        name = "cross2",
        command = quote(data1 + data2),
        pattern = quote(cross(data1, data2))
      ),
      tar_target_raw(
        name = "cross3",
        command = quote(data1 + map1),
        pattern = quote(cross(data1, map1))
      ),
      tar_target_raw(
        name = "out1",
        command = quote(sum(cross1))
      ),
      tar_target_raw(
        name = "out2",
        command = quote(sum(cross2))
      ),
      tar_target_raw(
        name = "out3",
        command = quote(sum(cross3))
      ),
      tar_target_raw(
        name = "out4",
        command = quote(out1 + out2)
      ),
      tar_target_raw(
        name = "out5",
        command = quote(out2 + out3)
      ),
      tar_target_raw(
        name = "out6",
        command = quote(cumsum(cross3)[seq_len(2L)])
      ),
      tar_target_raw(
        name = "map2",
        command = quote(as.integer(out6^2)),
        pattern = quote(map(out6))
      )
    )
  )
}
