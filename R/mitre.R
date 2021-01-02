#' Nodes and Edges ready for digraphs. Include shield and att&ck objects.
#'
#' @param verbose default is FALSE
#'
#' @return list of two data frames: nodes and edges
#' @export
#'
#' @examples
#' \donttest{
#' mitrenet <- mitre::getMitreNetwork()
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

#' Title
#'
#' @return
#' @export
updateRawData <- function(verbose = FALSE) {
  # ATT&CK
  if (verbose) print(paste("[*][ATT&CK] Download ATT&CK MOBILE ..."))
  attck.mob.raw.url <- "https://raw.githubusercontent.com/mitre/cti/master/mobile-attack/mobile-attack.json"
  utils::download.file(url = attck.mob.raw.url, destfile = "data-raw/attack-mobile.json", quiet = !verbose)
  if (verbose) print(paste("[*][ATT&CK] Download ATT&CK ENTERPRISE ..."))
  attck.ent.raw.url <- "https://raw.githubusercontent.com/mitre/cti/master/enterprise-attack/enterprise-attack.json"
  utils::download.file(url = attck.ent.raw.url, destfile = "data-raw/attack-enterprise.json", quiet = !verbose)

  # SHIELD
  if (verbose) print(paste("[*][SHIELD] Download Tactics ..."))
  tactics_url <- "https://raw.githubusercontent.com/MITRECND/mitrecnd.github.io/master/_data/tactics.json"
  utils::download.file(url = tactics_url, destfile = "data-raw/shield-tactics.json", quiet = !verbose)
  if (verbose) print(paste("[*][SHIELD] Download Tactics ..."))
  tech_url <- "https://raw.githubusercontent.com/MITRECND/mitrecnd.github.io/master/_data/techniques.json"
  utils::download.file(url = tech_url, destfile = "data-raw/shield-techniques.json", quiet = !verbose)
  if (verbose) print(paste("[*][SHIELD] Download Opportunities ..."))
  opport_url <- "https://raw.githubusercontent.com/MITRECND/mitrecnd.github.io/master/_data/opportunities.json"
  utils::download.file(url = opport_url, destfile = "data-raw/shield-opportunities.json", quiet = !verbose)
  if (verbose) print(paste("[*][SHIELD] Download Procedures ..."))
  proced_url <- "https://raw.githubusercontent.com/MITRECND/mitrecnd.github.io/master/_data/procedures.json"
  utils::download.file(url = proced_url, destfile = "data-raw/shield-procedures.json", quiet = !verbose)
  if (verbose) print(paste("[*][SHIELD] Download Use cases ..."))
  usecase_url <- "https://raw.githubusercontent.com/MITRECND/mitrecnd.github.io/master/_data/use_cases.json"
  utils::download.file(url = usecase_url, destfile = "data-raw/shield-use_cases.json", quiet = !verbose)
  if (verbose) print(paste("[*][SHIELD] Download Tactic details ..."))
  tact_det_url <- "https://raw.githubusercontent.com/MITRECND/mitrecnd.github.io/master/_data/tactic_details.json"
  utils::download.file(url = tact_det_url, destfile = "data-raw/shield-tactic_details.json", quiet = !verbose)
  if (verbose) print(paste("[*][SHIELD] Download Technique details ..."))
  tech_det_url <- "https://raw.githubusercontent.com/MITRECND/mitrecnd.github.io/master/_data/technique_details.json"
  utils::download.file(url = tech_det_url, destfile = "data-raw/shield-technique_details.json", quiet = !verbose)
}

