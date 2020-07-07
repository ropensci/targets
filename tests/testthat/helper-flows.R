pipeline_order <- function() {
  pipeline_init(
    list(
      target_init(name = "data1", expr = quote(seq_len(10))),
      target_init(name = "data2", expr = quote(seq_len(20))),
      target_init(name = "min1", expr = quote(min(data1))),
      target_init(name = "min2", expr = quote(min(data2))),
      target_init(name = "max1", expr = quote(max(data1))),
      target_init(name = "max2", expr = quote(max(data2))),
      target_init(name = "mins", expr = quote(c(min1, min2))),
      target_init(name = "maxes", expr = quote(c(max1, max2))),
      target_init(name = "all", expr = quote(c(mins, maxes)))
    )
  )
}

pipeline_map <- function(storage = "local") {
  pipeline_init(
    list(
      target_init(
        name = "data0",
        expr = quote(c(1L + 1L)),
        storage = storage
      ),
      target_init(
        name = "data1",
        expr = quote(seq_len(3L)),
        storage = storage
      ),
      target_init(
        name = "data2",
        expr = quote(seq_len(3L) + 3L),
        storage = storage
      ),
      target_init(
        name = "map1",
        expr = quote(data1 + sum(data0)),
        pattern = quote(map(data1)),
        storage = storage
      ),
      target_init(
        name = "map2",
        expr = quote(data1 + data2),
        pattern = quote(map(data1, data2)),
        storage = storage
      ),
      target_init(
        name = "map3",
        expr = quote(map1 + 1L),
        pattern = quote(map(map1)),
        storage = storage
      ),
      target_init(
        name = "map4",
        expr = quote(map1 + map2),
        pattern = quote(map(map1, map2)),
        storage = storage
      ),
      target_init(
        name = "map5",
        expr = quote(map1 + data2),
        pattern = quote(map(map1, data2)),
        storage = storage
      ),
      target_init(
        name = "map6",
        expr = quote(sum(map1) + sum(data2)),
        pattern = quote(map(data2)),
        storage = storage
      )
    )
  )
}

pipeline_cross <- function() {
  pipeline_init(
    list(
      target_init(
        name = "data1",
        expr = quote(seq_len(3))
      ),
      target_init(
        name = "data2",
        expr = quote(rev(seq_len(3)))
      ),
      target_init(
        name = "map1",
        expr = quote(data1 + 10L),
        pattern = quote(map(data1))
      ),
      target_init(
        name = "cross1",
        expr = quote(data1 + 1L),
        pattern = quote(cross(data1))
      ),
      target_init(
        name = "cross2",
        expr = quote(data1 + data2),
        pattern = quote(cross(data1, data2))
      ),
      target_init(
        name = "cross3",
        expr = quote(data1 + map1),
        pattern = quote(cross(data1, map1))
      ),
      target_init(
        name = "out1",
        expr = quote(sum(cross1))
      ),
      target_init(
        name = "out2",
        expr = quote(sum(cross2))
      ),
      target_init(
        name = "out3",
        expr = quote(sum(cross3))
      ),
      target_init(
        name = "out4",
        expr = quote(out1 + out2)
      ),
      target_init(
        name = "out5",
        expr = quote(out2 + out3)
      ),
      target_init(
        name = "out6",
        expr = quote(cumsum(cross3)[seq_len(2L)])
      ),
      target_init(
        name = "map2",
        expr = quote(as.integer(out6 ^ 2)),
        pattern = quote(map(out6))
      )
    )
  )
}
