#' Download latest R data sets from Github previously parsed with this package.
#'
#' @param verbose default is FALSE
#'
#' @return list of standards and network
#' @export
#'
#' @examples
#' \donttest{
#' mitre.data <- mitre::getLatestDataSet(T)
#' }
getLatestDataSet <- function(verbose = FALSE) {
  t <- tempfile()
  download.file(url = "https://github.com/motherhack3r/mitre-datasets/raw/master/beta/mitre_v0.3.0.9002.rds",
                destfile = t, quiet = T)
  mitre.data <- readRDS(t)
  file.remove(t)
  return(mitre.data)
}

#' ETL process for all standards, it also create a list of nodes and edges
#' representing the relationships between standard objects. It needs raw files
#' downloaded from official MITRE repositories stored in a folder named "data-raw";
#' set downloadLatest parameter to TRUE and the function will create it for you.
#'
#' @param verbose default is FALSE
#' @param downloadLatest default as FALSE, set to TRUE to download latest raw source files from official MITRE repositories
#'
#' @return list of two data frames: nodes and edges
#' @export
#'
#' @examples
#' \donttest{
#' mitredata <- mitre::parseRawData(T)
#' }
parseRawData <- function(verbose = FALSE, downloadLatest = TRUE) {
  if (downloadLatest) downloadRawData(verbose)
  if (!dir.exists("data-raw")) stop('Please, set downloadLatest to TRUE or ensure that your working directory contains a folder named "data-raw" with MITRE raw files.', call. = FALSE)

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

  if (verbose) print(paste("[#][CAR] Start ETL process."))
  mitre.car <- getCARData(verbose)
  car_nodes <- mitre.capec$carnet$nodes
  car_edges <- mitre.capec$carnet$edges

  nodes <- dplyr::bind_rows(shield_nodes, attck_nodes, cve_nodes, cwe_nodes, cpe_nodes, capec_nodes, car_nodes)
  edges <- dplyr::bind_rows(shield_edges, attck_edges, cve_edges, cwe_edges, cpe_edges, capec_edges, car_edges)

  mitrenet <- list(edges = edges,
                   nodes = nodes)

  standards <- list(shield = shield,
                    attck = attck,
                    cpe = mitre.cpes,
                    cve = mitre.cves,
                    cwe = mitre.cwes,
                    capec = mitre.capec,
                    car = mitre.car)
  mitre.data <- list(standards = standards,
                     mitrenet = mitrenet)

  return(mitre.data)
}

#' Download from official sources raw files saving them in [working_directory]/data-raw/
#'
#' @param verbose default is FALSE
#' @export
#'
#' @examples
#' \donttest{
#' mitre::downloadRawData(T)
#' }
downloadRawData <- function(verbose = FALSE) {
  # Create "data-raw" folder
  if (!dir.exists("data-raw")) dir.create("data-raw")

  # ATT&CK
  if (verbose) print(paste("[*][ATT&CK] Download ATT&CK MOBILE ..."))
  attck.mob.raw.url <- "https://raw.githubusercontent.com/mitre/cti/master/mobile-attack/mobile-attack.json"
  utils::download.file(url = attck.mob.raw.url, destfile = "data-raw/attack-mobile.json", quiet = T)
  if (verbose) print(paste("[*][ATT&CK] Download ATT&CK ENTERPRISE ..."))
  attck.ent.raw.url <- "https://raw.githubusercontent.com/mitre/cti/master/enterprise-attack/enterprise-attack.json"
  utils::download.file(url = attck.ent.raw.url, destfile = "data-raw/attack-enterprise.json", quiet = T)

  # SHIELD
  if (verbose) print(paste("[*][SHIELD] Download Tactics ..."))
  tactics_url <- "https://raw.githubusercontent.com/MITRECND/mitrecnd.github.io/master/_data/tactics.json"
  utils::download.file(url = tactics_url, destfile = "data-raw/shield-tactics.json", quiet = T)
  if (verbose) print(paste("[*][SHIELD] Download Tactics ..."))
  tech_url <- "https://raw.githubusercontent.com/MITRECND/mitrecnd.github.io/master/_data/techniques.json"
  utils::download.file(url = tech_url, destfile = "data-raw/shield-techniques.json", quiet = T)
  if (verbose) print(paste("[*][SHIELD] Download Opportunities ..."))
  opport_url <- "https://raw.githubusercontent.com/MITRECND/mitrecnd.github.io/master/_data/opportunities.json"
  utils::download.file(url = opport_url, destfile = "data-raw/shield-opportunities.json", quiet = T)
  if (verbose) print(paste("[*][SHIELD] Download Procedures ..."))
  proced_url <- "https://raw.githubusercontent.com/MITRECND/mitrecnd.github.io/master/_data/procedures.json"
  utils::download.file(url = proced_url, destfile = "data-raw/shield-procedures.json", quiet = T)
  if (verbose) print(paste("[*][SHIELD] Download Use cases ..."))
  usecase_url <- "https://raw.githubusercontent.com/MITRECND/mitrecnd.github.io/master/_data/use_cases.json"
  utils::download.file(url = usecase_url, destfile = "data-raw/shield-use_cases.json", quiet = T)
  if (verbose) print(paste("[*][SHIELD] Download Tactic details ..."))
  tact_det_url <- "https://raw.githubusercontent.com/MITRECND/mitrecnd.github.io/master/_data/tactic_details.json"
  utils::download.file(url = tact_det_url, destfile = "data-raw/shield-tactic_details.json", quiet = T)
  if (verbose) print(paste("[*][SHIELD] Download Technique details ..."))
  tech_det_url <- "https://raw.githubusercontent.com/MITRECND/mitrecnd.github.io/master/_data/technique_details.json"
  utils::download.file(url = tech_det_url, destfile = "data-raw/shield-technique_details.json", quiet = T)

  # CVE
  for (year in 2002:strftime(Sys.Date(), "%Y")) {
    if (verbose) print(paste0("[*][CVE] Download ", year," CVEs ..."))
    cve_url <- paste0("https://nvd.nist.gov/feeds/json/cve/1.1/nvdcve-1.1-", year,".json.gz")
    utils::download.file(url = cve_url, destfile = paste0("data-raw/cve-", year,".json.gz"), quiet = T)
  }

  # CWE
  if (verbose) print(paste("[*][CWE] Download latest XML definitions ..."))
  cwe.url  <- "http://cwe.mitre.org/data/xml/cwec_latest.xml.zip"
  utils::download.file(url = cwe.url, destfile = "data-raw/cwe-mitre.xml.zip", quiet = T)
  utils::unzip(zipfile = paste0("data-raw/cwe-mitre.xml.zip"),
               exdir = paste0("data-raw"),
               overwrite = T)

  # CPE
  if (verbose) print(paste("[*][CPE] Download latest XML definitions ..."))
  cpe.url  <- "http://static.nvd.nist.gov/feeds/xml/cpe/dictionary/official-cpe-dictionary_v2.3.xml.zip"
  utils::download.file(url = cpe.url, destfile = "data-raw/cpe-mitre.xml.zip", quiet = T)
  utils::unzip(zipfile = "data-raw/cpe-mitre.xml.zip", exdir = "data-raw")

  # CAPEC
  if (verbose) print(paste("[*][CAPEC] Download latest XML definitions ..."))
  capec.url  <- "https://capec.mitre.org/data/xml/capec_latest.xml"
  utils::download.file(url = capec.url, destfile = "data-raw/capec_latest.xml", quiet = T)

  # CTI
  if (verbose) print(paste("[*][CTI] Download latest YAML definitions ..."))
  download.file(url = "https://github.com/mitre/cti/archive/master.zip",
                destfile = "data-raw/cti.zip", quiet = T)
  unzip(zipfile = "data-raw/cti.zip", exdir = "data-raw")

  # domains <- c("pre-attack", "enterprise-attack", "mobile-attack", "ics-attack")
  # objects <- c("attack-pattern", "intrusion-set", "malware", "tool",
  #             "course-of-action", "x-mitre-tactic", "x-mitre-matrix")
  # sapply(domains,
  #        function(domain) {
  #          sapply(objects,
  #                 function(object) {
  #                   gitfiles <- getGitHubCTIfiles(domain = domain, object = object)
  #                   if (!dir.exists(paste0("data-raw/", domain))) dir.create(paste0("data-raw/", domain))
  #                   if (nrow(gitfiles) > 0) {
  #                     apply(gitfiles, 1, function(x) {
  #                       download.file(url = x["src.file"],
  #                                     destfile = paste0("data-raw/", domain, "/", x["filename"]),
  #                                     quiet = T)
  #                     })
  #                     if (verbose) print(paste("[*][CTI][", domain, "] Downloaded",
  #                                              nrow(gitfiles), object, "definitions"))
  #                   }
  #                 })
  #        })
}

