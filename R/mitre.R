#' Nodes and Edges ready for digraphs. Include CVE, shield and ATT&CK objects.
#'
#' @param verbose default is FALSE
#'
#' @return list of two data frames: nodes and edges
#' @export
#'
#' @examples
#' \donttest{
#' mitrenet <- mitre::getMitreNetwork(T)
#' }
getMitreNetwork <- function(verbose = FALSE) {
  if (verbose) print(paste("[#][SHIELD] Start ETL process."))
  shield <- getShieldData(verbose)
  shield_nodes <- shield$shieldnet$nodes
  shield_edges <- shield$shieldnet$edges

  if (verbose) print(paste("[#][ATT&CK] Start ETL process."))
  attck <- getAttckData(verbose)
  attck_nodes <- attck$attcknet$nodes
  attck_edges <- attck$attcknet$edges

  if (verbose) print(paste("[#][CVE] Start ETL process."))
  mitre.cves <- getCVEData(verbose)
  cve_nodes <- mitre.cves$cvenet$nodes
  cve_edges <- mitre.cves$cvenet$edges

  nodes <- rbind(shield_nodes, attck_nodes, cve_nodes)
  edges <- rbind(shield_edges, attck_edges, cve_edges)

  mitrenet <- list(edges = edges,
                   nodes = nodes)

  return(mitrenet)
}

#' Download from official sources raw files saving them in [package_path]/data-raw/
#'
#' @param verbose default is FALSE
#'
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

  # CVE
  for (year in 2002:strftime(Sys.Date(), "%Y")) {
    if (verbose) print(paste0("[*][CVE] Download ", year," CVEs ..."))
    cve_url <- paste0("https://nvd.nist.gov/feeds/json/cve/1.1/nvdcve-1.1-", year,".json.gz")
    utils::download.file(url = cve_url, destfile = paste0("data-raw/cve-", year,".json.gz"), quiet = !verbose)
  }

  # CWE
  cwe.url  <- "http://cwe.mitre.org/data/xml/cwec_latest.xml.zip"
  utils::download.file(url = cwe.url, destfile = paste0("data-raw/cwe-mitre.xml.zip"), quiet = !verbose)
  utils::unzip(zipfile = paste0("data-raw/cwe-mitre.xml.zip"),
               exdir = paste0("data-raw"),
               overwrite = T)

  # CPE
  cpe.url  <- "http://static.nvd.nist.gov/feeds/xml/cpe/dictionary/official-cpe-dictionary_v2.3.xml.zip"
  utils::download.file(url = cpe.url, destfile = "data-raw/cpe-mitre.xml.zip", quiet = !verbose)
  utils::unzip(zipfile = "data-raw/cpe-mitre.xml.zip", exdir = "data-raw")
}

