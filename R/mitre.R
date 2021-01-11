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

  if (verbose) print(paste("[#][CWE] Start ETL process."))
  mitre.cwes <- getCWEData(verbose)
  cwe_nodes <- mitre.cwes$cwenet$nodes
  cwe_edges <- mitre.cwes$cwenet$edges

  if (verbose) print(paste("[#][CPE] Start ETL process."))
  mitre.cpes <- getCPEData(verbose)
  cpe_nodes <- mitre.cpes$cpenet$nodes
  cpe_edges <- mitre.cpes$cpenet$edges

  if (verbose) print(paste("[#][CAPEC] Start ETL process."))
  mitre.capec <- getCAPECData(verbose)
  capec_nodes <- mitre.capec$capecnet$nodes
  capec_edges <- mitre.capec$capecnet$edges

  nodes <- dplyr::bind_rows(shield_nodes, attck_nodes, cve_nodes, cwe_nodes, cpe_nodes, capec_nodes)
  edges <- dplyr::bind_rows(shield_edges, attck_edges, cve_edges, cwe_edges, cpe_edges, capec_edges)

  mitrenet <- list(edges = edges,
                   nodes = nodes)

  standards <- list(shield = shield,
                    attck = attck,
                    cpe = mitre.cpes,
                    cve = mitre.cves,
                    cwe = mitre.cwes,
                    capec = mitre.capec)
  mitre.data <- list(standards = standards,
                     mitrenet = mitrenet)

  return(mitre.data)
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
  if (verbose) print(paste("[*][CWE] Download latest XML definitions ..."))
  cwe.url  <- "http://cwe.mitre.org/data/xml/cwec_latest.xml.zip"
  utils::download.file(url = cwe.url, destfile = "data-raw/cwe-mitre.xml.zip", quiet = !verbose)
  utils::unzip(zipfile = paste0("data-raw/cwe-mitre.xml.zip"),
               exdir = paste0("data-raw"),
               overwrite = T)

  # CPE
  if (verbose) print(paste("[*][CPE] Download latest XML definitions ..."))
  cpe.url  <- "http://static.nvd.nist.gov/feeds/xml/cpe/dictionary/official-cpe-dictionary_v2.3.xml.zip"
  utils::download.file(url = cpe.url, destfile = "data-raw/cpe-mitre.xml.zip", quiet = !verbose)
  utils::unzip(zipfile = "data-raw/cpe-mitre.xml.zip", exdir = "data-raw")

  # CAPEC
  if (verbose) print(paste("[*][CAPEC] Download latest XML definitions ..."))
  capec.url  <- "https://capec.mitre.org/data/xml/capec_latest.xml"
  utils::download.file(url = capec.url, destfile = "data-raw/capec_latest.xml", quiet = !verbose)
}

