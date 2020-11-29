#' Title
#'
#' @param verbose
#'
#' @return
#' @export
#'
#' @examples
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
