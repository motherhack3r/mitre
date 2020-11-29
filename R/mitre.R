#' Nodes and Edges ready for digraphs. Include shield and att&ck objects.
#'
#' @param verbose default is FALSE
#'
#' @return list of two data frames: nodes and edges
#' @export
#'
#' @examples
#' \donttest{
#' mitrenet <- getMitreNetwork()
#' }
getMitreNetwork <- function(verbose = FALSE) {
  if (verbose) print(paste("[*][SHIELD] Start ETL process."))
  shield <- getShieldData()
  shield_nodes <- shield$shieldnet$nodes
  shield_edges <- shield$shieldnet$edges

  if (verbose) print(paste("[*][ATT&CK] Start ETL process."))
  attck <- getAttckData()
  attck_nodes <- attck$attcknet$nodes
  attck_edges <- attck$attcknet$edges

  nodes <- rbind(shield_nodes, attck_nodes)
  edges <- rbind(shield_edges, attck_edges)

  mitrenet <- list(edges = edges,
                   nodes = nodes)

  return(mitrenet)
}
