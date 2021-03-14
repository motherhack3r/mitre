#' Returns a list of nodes and edges (neighbors) based on input node.
#'
#' @param node MITRE Standard Id
#' @param direction value should be: "from", "to" or "both"
#' @param mitrenet MITRE network built with this package
#' @param verbose default is FALSE
#'
#' @return list of nodes and edges
#' @export
getNodeNeighbors <- function(nodes = c("T1104"), direction = "both",
                             mitrenet = getLatestDataSet()[["mitrenet"]], verbose = FALSE) {
  # Collect input node
  nnodes <- mitrenet$nodes[mitrenet$nodes$id %in% nodes, ]
  eedges <- utils::head(mitrenet$edges, 0)

  if (direction %in% c("from", "both")) {
    # Find edges from node
    efrom <- mitrenet$edges[mitrenet$edges$from %in% nodes, ]
    # Collect destination nodes
    nfrom <- mitrenet$nodes[mitrenet$nodes$id %in% efrom$to, ]
    # Update Neighbors
    nnodes <- dplyr::bind_rows(nnodes, nfrom)
    eedges <- dplyr::bind_rows(eedges, efrom)
  }

  if (direction %in% c("to", "both")) {
    # Find edges where destination is node
    eto <- mitrenet$edges[mitrenet$edges$to %in% nodes, ]
    # Collect those nodes
    nto <- mitrenet$nodes[mitrenet$nodes$id %in% eto$from, ]
    # Update Neighbors
    nnodes <- dplyr::bind_rows(nnodes, nto)
    eedges <- dplyr::bind_rows(eedges, eto)
  }

  nnodes <- unique(nnodes)
  eedges <- unique(eedges)

  nn <- list(nodes = nnodes,
             edges = eedges)

  return(nn)
}

#' Given a mitre network it returns the same without deprecated nodes
#'
#' @param mitrenet MITRE network built with this package
#' @param verbose default is FALSE
#'
#' @return list of nodes and edges
#' @export
omitDeprecated <- function(mitrenet = getLatestDataSet()[["mitrenet"]], verbose = FALSE) {
  # Select nodes not deprecated
  nodes <- mitrenet$nodes
  nodes <- unique(nodes[!nodes$shadow, ])

  # Select edges related to any node
  edges <- mitrenet$edges[((mitrenet$edges$from %in% nodes$id) &
                             (mitrenet$edges$to %in% nodes$id)), ]
  edges <- unique(edges)

  # Select nodes with relationships
  nodes <- nodes[((nodes$id %in% edges$from) |
                    (nodes$id %in% edges$to)), ]

  # Select edges related to nodes
  edges <- edges[((edges$from %in% nodes$id) &
                    (edges$to %in% nodes$id)), ]
  edges <- unique(edges)

  mitrenet$nodes <- nodes
  mitrenet$edges <- edges

  return(mitrenet)
}

#' Given a mitre network it returns the same as igraph
#'
#' @param mitrenet MITRE network built with this package
#' @param verbose default is FALSE
#'
#' @return igraph
#' @export
as_igraph <- function(mitrenet = getLatestDataSet()[["mitrenet"]], verbose = FALSE) {
  nodes <- mitrenet$nodes
  edges <- mitrenet$edges

  edges <- dplyr::left_join(edges, nodes[, c("id", "name")], by = c("from"="id"))
  edges$src <- edges$name
  edges$name <- NULL
  edges <- dplyr::left_join(edges, nodes[, c("id", "name")], by = c("to"="id"))
  edges$dst <- edges$name
  edges$name <- NULL

  # kk <- nodes[!(nodes$id %in% unique(c(edges$from, edges$to))), ]
  # kk <- edges[is.na(edges$to) | is.na(edges$from),]
  # kk <- edges[is.na(edges$src) | is.na(edges$dst),]
  # kk <- nodes[which(nodes$shadow),]

  ig <- igraph::graph_from_data_frame(edges, directed = T, vertices = nodes)

  return(ig)
}
