hash_imports <- function(envir) {
  hash_imports_graph(envir, graph_envir(envir))
}

hash_imports_graph <- function(envir, graph) {
  order <- topo_sort_igraph(graph)
  hashes <- new.env(parent = emptyenv())
  lapply(order, hash_import, hashes = hashes, envir = envir, graph = graph)
  hash <- map_chr(order, get_field, collection = hashes)
  type <- map_chr(order, type_import, envir = envir)
  data_frame(name = order, type = type, data = hash)
}

graph_envir <- function(envir) {
  graph_edges(edges_envir(envir))
}

type_import <- function(name, envir) {
  object <- get(x = name, envir = envir, inherits = FALSE)
  ifelse(is.function(object), "function", "object")
}

graph_edges <- function(edges) {
  graph <- igraph::graph_from_data_frame(edges)
  graph <- igraph::simplify(graph)
  stopifnot(igraph::is_dag(graph))
  graph
}

edges_envir <- function(envir) {
  names <- fltr(names(envir), ~!is_internal_name(.x, envir))
  from <- lapply(names, envir_deps, envir = envir, names = names)
  lengths <- map_int(from, length)
  to <- lapply(seq_along(names), rep_to, names = names, lengths = lengths)
  from <- as.character(unlist(from))
  to <- as.character(unlist(to))
  out <- data_frame(from = unlist(from), to = unlist(to))
}

is_internal_name <- function(name, envir) {
  is_internal_object(get(x = name, envir = envir, inherits = FALSE))
}

is_internal_object <- function(object) {
  inherits(object, c("tar_algorithm", "tar_pipeline", "tar_target"))
}

envir_deps <- function(name, envir, names) {
  value <- get(x = name, envir = envir, inherits = FALSE)
  deps <- if_any(is.function(value), deps_function(value), character(0))
  deps <- c(deps, name)
  intersect(deps, names)
}

rep_to <- function(index, names, lengths) {
  rep(x = names[index], times = lengths[index])
}

hash_import <- function(name, hashes, envir, graph) {
  value <- get(x = name, envir = envir, inherits = FALSE)
  hash_object(value, name, hashes, graph)
}

hash_object <- function(value, name, hashes, graph) {
  UseMethod("hash_object")
}

#' @export
hash_object.character <- function(value, name, hashes, graph) {
  base <- paste(value, collapse = " ")
  assign(x = name, value = digest_chr64(base), envir = hashes)
}

#' @export
hash_object.function <- function(value, name, hashes, graph) {
  str <- deparse_safe(value)
  deps <- sort(names(igraph::neighbors(graph = graph, v = name, mode = "in")))
  dep_hashes <- unlist(lapply(deps, get_field, collection = hashes))
  base <- paste(c(str, dep_hashes), collapse = " ")
  assign(x = name, value = digest_chr64(base), envir = hashes)
}

#' @export
hash_object.default <- function(value, name, hashes, graph) {
  assign(x = name, value = digest_obj64(value), envir = hashes)
}
