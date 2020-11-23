#' Retrieve current CTI definitions from github, and return data sets as list of
#' data frames: tactics, techniques, ...
#'
#' @return ETL process that read source data from \url{https://github.com/MITRECND/mitrecnd.github.io/tree/master/_data} .
#'   The data frame columns are: id, name, description, long_description.
#' @export
#' @examples
#' \dontrun{
#' attck <- getCurrentCTIdata()
#' }
getCurrentCTIdata <- function() {
  capec.raw.url <- "https://raw.githubusercontent.com/mitre/cti/master/capec/stix-capec.json"
  capec.raw.file <- tempfile(pattern = "mitre_capec_", fileext = ".json")
  utils::download.file(url = capec.raw.url, destfile = capec.raw.file)

  attck.mob.raw.url <- "https://raw.githubusercontent.com/mitre/cti/master/mobile-attack/mobile-attack.json"
  attck.mob.raw.file <- tempfile(pattern = "mitre_attckmob_", fileext = ".json")
  utils::download.file(url = attck.mob.raw.url, destfile = attck.mob.raw.file)

  attck.ics.raw.url <- "https://raw.githubusercontent.com/mitre/cti/master/ics-attack/ics-attack.json"
  attck.ics.raw.file <- tempfile(pattern = "mitre_attckics_", fileext = ".json")
  utils::download.file(url = attck.ics.raw.url, destfile = attck.ics.raw.file)

  attck.ent.raw.url <- "https://raw.githubusercontent.com/mitre/cti/master/enterprise-attack/enterprise-attack.json"
  attck.ent.raw.file <- tempfile(pattern = "mitre_attckent_", fileext = ".json")
  utils::download.file(url = attck.ent.raw.url, destfile = attck.ent.raw.file)


  ####### ATT&CK TACTICS
  # MOBILE
  attck.mob.raw <- jsonlite::fromJSON(attck.mob.raw.file)[["objects"]]
  attck.mob <- data.frame(id = attck.mob.raw$id,
                          type = as.factor(attck.mob.raw$type),
                          stringsAsFactors = F)
  attck.mob.raw <- RJSONIO::fromJSON(content = attck.mob.raw.file)
  attck.mob.raw <- attck.mob.raw[["objects"]]
  attck.raw <- attck.mob.raw[which(attck.mob$type == "x-mitre-tactic")]

  # ICS
  attck.ics.raw <- jsonlite::fromJSON(attck.ics.raw.file)[["objects"]]
  attck.ics <- data.frame(id = attck.ics.raw$id,
                          type = as.factor(attck.ics.raw$type),
                          stringsAsFactors = F)
  attck.ics.raw <- RJSONIO::fromJSON(content = attck.ics.raw.file)
  attck.ics.raw <- attck.ics.raw[["objects"]]
  attck.raw <- c(attck.raw, attck.ics.raw[which(attck.ics$type == "x-mitre-tactic")])

  # ENT
  attck.ent.raw <- jsonlite::fromJSON(attck.ent.raw.file)[["objects"]]
  attck.ent <- data.frame(id = attck.ent.raw$id,
                          type = as.factor(attck.ent.raw$type),
                          stringsAsFactors = F)
  attck.ent.raw <- RJSONIO::fromJSON(content = attck.ent.raw.file)
  attck.ent.raw <- attck.ent.raw[["objects"]]
  attck.raw <- c(attck.raw, attck.ent.raw[which(attck.ent$type == "x-mitre-tactic")])

  names(attck.raw) <- sapply(attck.raw,
                             function(x) {
                               sapply(x[["external_references"]],
                                      function(y)
                                        y[["external_id"]][which((y[["source_name"]]=="mitre-attack") |
                                                                   (y[["source_name"]]=="mitre-ics-attack"))])
                             })

  # Tidy Tactics data frame
  attck.tactics <- lapply(attck.raw, function(x) x[names(x) != "external_references"])
  attck.tactics <- plyr::ldply(attck.tactics, rbind.data.frame)
  attck.tactics$mitreid <- attck.tactics$.id
  attck.tactics <- attck.tactics[, c("mitreid", "id", "type", "name", "description",
                                     "x_mitre_shortname", "created", "modified",
                                     "object_marking_refs", "created_by_ref")]

  attck <- list(tactics = attck.tactics)

  return(attck)
}
