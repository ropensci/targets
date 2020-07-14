upstream_vertices <- function(graph, from) {
  nbhd_vertices(
    graph = graph,
    vertices = from,
    mode = "in",
    order = igraph::gorder(graph)
  )
}

nbhd_vertices <- function(graph, vertices, mode, order) {
  vertices <- intersect(vertices, igraph::V(graph)$name)
  from <- vertices
  level <- 0L
  while (length(from) && level < order) {
    from <- targets_adjacent_vertices(graph, v = from, mode = mode)
    from <- setdiff(from, vertices)
    vertices <- c(vertices, from)
    level <- level + 1L
  }
  vertices
}

targets_adjacent_vertices <- function(graph, v, mode) {
  opt <- igraph::igraph_opt("return.vs.es")
  on.exit(igraph::igraph_options(return.vs.es = opt))
  igraph::igraph_options(return.vs.es = FALSE)
  index <- igraph::adjacent_vertices(graph = graph, v = v, mode = mode)
  index <- unlist(index, use.names = FALSE)
  index <- unique(index)
  igraph::V(graph)$name[index + 1]
}

leaves <- function(igraph) {
  is_leaf <- igraph::degree(igraph, mode = "in") == 0L
  igraph::V(igraph)[is_leaf]$name
}

topo_sort_by_priority <- function(igraph, pipeline) {
  browser()
}
