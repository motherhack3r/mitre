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
  # capec.raw.url <- "https://raw.githubusercontent.com/mitre/cti/master/capec/stix-capec.json"
  # capec.raw.file <- tempfile(pattern = "mitre_capec_", fileext = ".json")
  # utils::download.file(url = capec.raw.url, destfile = capec.raw.file)

  attck.mob.raw.url <- "https://raw.githubusercontent.com/mitre/cti/master/mobile-attack/mobile-attack.json"
  attck.mob.raw.file <- tempfile(pattern = "mitre_attckmob_", fileext = ".json")
  utils::download.file(url = attck.mob.raw.url, destfile = attck.mob.raw.file)

  attck.ics.raw.url <- "https://raw.githubusercontent.com/mitre/cti/master/ics-attack/ics-attack.json"
  attck.ics.raw.file <- tempfile(pattern = "mitre_attckics_", fileext = ".json")
  utils::download.file(url = attck.ics.raw.url, destfile = attck.ics.raw.file)

  attck.ent.raw.url <- "https://raw.githubusercontent.com/mitre/cti/master/enterprise-attack/enterprise-attack.json"
  attck.ent.raw.file <- tempfile(pattern = "mitre_attckent_", fileext = ".json")
  utils::download.file(url = attck.ent.raw.url, destfile = attck.ent.raw.file)


  ####### PARSE ATT&CK Objects
  # MOBILE
  attck.mob.raw <- jsonlite::fromJSON(attck.mob.raw.file)[["objects"]]
  attck.mob <- data.frame(id = attck.mob.raw$id,
                          type = as.factor(attck.mob.raw$type),
                          stringsAsFactors = F)
  attck.mob.raw <- RJSONIO::fromJSON(content = attck.mob.raw.file)
  attck.mob.raw <- attck.mob.raw[["objects"]]
  attck.raw <- attck.mob.raw[which(attck.mob$type %in% c("x-mitre-tactic",
                                                         "attack-pattern",
                                                         "intrusion-set",
                                                         "malware", "tool",
                                                         "course-of-action"))]
  tactics.raw <- attck.mob.raw[which(attck.mob$type == "x-mitre-tactic")]
  techniques.raw <- attck.mob.raw[which(attck.mob$type == "attack-pattern")]
  groups.raw <- attck.mob.raw[which(attck.mob$type == "intrusion-set")]
  software.raw <- attck.mob.raw[which(attck.mob$type %in% c("malware", "tool"))]
  mitigation.raw <- attck.mob.raw[which(attck.mob$type == "course-of-action")]
  relations.raw <- attck.mob.raw[which(attck.mob$type == "relationship")]

  # ICS
  attck.ics.raw <- jsonlite::fromJSON(attck.ics.raw.file)[["objects"]]
  attck.ics <- data.frame(id = attck.ics.raw$id,
                          type = as.factor(attck.ics.raw$type),
                          stringsAsFactors = F)
  attck.ics.raw <- RJSONIO::fromJSON(content = attck.ics.raw.file)
  attck.ics.raw <- attck.ics.raw[["objects"]]
  attck.raw <- c(attck.raw, attck.ics.raw[which(attck.ics$type %in% c("x-mitre-tactic",
                                                                      "attack-pattern",
                                                                      "intrusion-set",
                                                                      "malware", "tool",
                                                                      "course-of-action"))])
  tactics.raw <- c(tactics.raw, attck.ics.raw[which(attck.ics$type == "x-mitre-tactic")])
  techniques.raw <- c(techniques.raw, attck.ics.raw[which(attck.ics$type == "attack-pattern")])
  groups.raw <- c(groups.raw, attck.ics.raw[which(attck.ics$type == "intrusion-set")])
  software.raw <- c(software.raw, attck.ics.raw[which(attck.ics$type %in% c("malware", "tool"))])
  mitigation.raw <- c(mitigation.raw, attck.ics.raw[which(attck.ics$type == "course-of-action")])
  relations.raw <- c(relations.raw, attck.ics.raw[which(attck.ics$type == "relationship")])

  # ENT
  attck.ent.raw <- jsonlite::fromJSON(attck.ent.raw.file)[["objects"]]
  attck.ent <- data.frame(id = attck.ent.raw$id,
                          type = as.factor(attck.ent.raw$type),
                          stringsAsFactors = F)
  attck.ent.raw <- RJSONIO::fromJSON(content = attck.ent.raw.file)
  attck.ent.raw <- attck.ent.raw[["objects"]]
  attck.raw <- c(attck.raw, attck.ent.raw[which(attck.ent$type %in% c("x-mitre-tactic",
                                                                      "attack-pattern",
                                                                      "intrusion-set",
                                                                      "malware", "tool",
                                                                      "course-of-action"))])
  tactics.raw <- c(tactics.raw, attck.ent.raw[which(attck.ent$type == "x-mitre-tactic")])
  techniques.raw <- c(techniques.raw, attck.ent.raw[which(attck.ent$type == "attack-pattern")])
  groups.raw <- c(groups.raw, attck.ent.raw[which(attck.ent$type == "intrusion-set")])
  software.raw <- c(software.raw, attck.ent.raw[which(attck.ent$type %in% c("malware", "tool"))])
  mitigation.raw <- c(mitigation.raw, attck.ent.raw[which(attck.ent$type == "course-of-action")])
  relations.raw <- c(relations.raw, attck.ent.raw[which(attck.ent$type == "relationship")])

  # TIDY MODEL
  model.raw <- attck.raw
  names(model.raw) <- sapply(model.raw,
                                  function(x) {
                                    y <- plyr::ldply(x[["external_references"]],rbind)
                                    y$external_id[y$source_name %in% c("mitre-attack",
                                                                       "mitre-ics-attack",
                                                                       "mitre-mobile-attack")]
                                  })
  attck.model <- lapply(model.raw,
                             function(x)
                               x[names(x) %in% c("id", "type", "name", "description",
                                                 "created", "modified",
                                                 "object_marking_refs", "created_by_ref")])
  attck.model <- plyr::ldply(attck.model, rbind.data.frame)
  attck.model$mitreid <- attck.model$id
  attck.model$id <- attck.model$.id
  attck.model <- attck.model[, c("id", "mitreid", "type", "name", "description",
                                           "created", "modified",
                                           "object_marking_refs", "created_by_ref")]

  # TIDY RELATIONS
  attck.relations <- lapply(relations.raw,
                             function(x)
                               x[names(x) %in% c("id", "created_by_ref", "object_marking_refs",
                                                 "source_ref", "relationship_type", "target_ref",
                                                 "type", "modified", "created")])
  attck.relations <- plyr::ldply(attck.relations, rbind.data.frame)
  attck.relations$mitreid <- attck.relations$id
  attck.relations$id <- attck.relations$.id
  attck.relations <- attck.relations[, c("type", "source_ref", "target_ref", "relationship_type",
                                         "created_by_ref", "object_marking_refs",
                                         "modified", "created")]

  attck.relations <- dplyr::left_join(attck.relations, attck.model[, c("mitreid","id")], by = c("source_ref" = "mitreid"))
  attck.relations$source_id <- attck.relations$id
  attck.relations$id <- NULL
  attck.relations <- dplyr::left_join(attck.relations, attck.model[, c("mitreid","id")], by = c("target_ref" = "mitreid"))
  attck.relations$target_id <- attck.relations$id
  attck.relations$id <- NULL

  ####### TIDY TACTICS
  names(tactics.raw) <- sapply(tactics.raw,
                             function(x) {
                               sapply(x[["external_references"]],
                                      function(y)
                                        y[["external_id"]][which(y[["source_name"]] %in%
                                                                   c("mitre-attack",
                                                                     "mitre-ics-attack",
                                                                     "mitre-mobile-attack"))])
                             })
  attck.tactics <- lapply(tactics.raw, function(x) x[names(x) != "external_references"])
  attck.tactics <- plyr::ldply(attck.tactics, rbind.data.frame)
  attck.tactics$mitreid <- attck.tactics$id
  attck.tactics$id <- attck.tactics$.id
  attck.tactics <- attck.tactics[, c("id", "mitreid", "type", "name", "description",
                                     "created", "modified",
                                     "object_marking_refs", "created_by_ref")]

  ####### TIDY TECHNIQUES
  names(techniques.raw) <- sapply(techniques.raw,
                               function(x) {
                                 y <- plyr::ldply(x[["external_references"]],rbind)
                                 y$external_id[y$source_name %in% c("mitre-attack",
                                                                    "mitre-ics-attack",
                                                                    "mitre-mobile-attack")]
                                 })
  attck.techniques <- lapply(techniques.raw,
                             function(x)
                               x[names(x) %in% c("id", "type", "name", "description",
                                                 "created", "modified",
                                                 "object_marking_refs", "created_by_ref")])
  attck.techniques <- plyr::ldply(attck.techniques, rbind.data.frame)
  attck.techniques$mitreid <- attck.techniques$id
  attck.techniques$id <- attck.techniques$.id
  attck.techniques <- attck.techniques[, c("id", "mitreid", "type", "name", "description",
                                     "created", "modified",
                                     "object_marking_refs", "created_by_ref")]

  ####### TIDY GROUPS
  names(groups.raw) <- sapply(groups.raw,
                                function(x) {
                                  y <- plyr::ldply(x[["external_references"]],rbind)
                                  y$external_id[y$source_name %in% c("mitre-attack",
                                                                     "mitre-ics-attack",
                                                                     "mitre-mobile-attack")]
                                })
  attck.groups <- lapply(groups.raw,
                           function(x)
                             x[names(x) %in% c("id", "type", "name", "description",
                                               "created", "modified",
                                               "object_marking_refs", "created_by_ref")])
  attck.groups <- plyr::ldply(attck.groups, rbind.data.frame)
  attck.groups$mitreid <- attck.groups$id
  attck.groups$id <- attck.groups$.id
  attck.groups <- attck.groups[, c("id", "mitreid", "type", "name", "description",
                                       "created", "modified",
                                       "object_marking_refs", "created_by_ref")]

  ####### TIDY SOFTWARE
  names(software.raw) <- sapply(software.raw,
                                function(x) {
                                  y <- plyr::ldply(x[["external_references"]],rbind)
                                  y$external_id[y$source_name %in% c("mitre-attack",
                                                                     "mitre-ics-attack",
                                                                     "mitre-mobile-attack")]
                                })
  attck.software <- lapply(software.raw,
                           function(x)
                             x[names(x) %in% c("id", "type", "name", "description",
                                               "created", "modified",
                                               "object_marking_refs", "created_by_ref")])
  attck.software <- plyr::ldply(attck.software, rbind.data.frame)
  attck.software$mitreid <- attck.software$id
  attck.software$id <- attck.software$.id
  attck.software <- attck.software[, c("id", "mitreid", "type", "name", "description",
                                       "created", "modified",
                                       "object_marking_refs", "created_by_ref")]

  ####### TIDY MITIGATIONS
  names(mitigation.raw) <- sapply(mitigation.raw,
                                  function(x) {
                                    y <- plyr::ldply(x[["external_references"]],rbind)
                                    y$external_id[y$source_name %in% c("mitre-attack",
                                                                       "mitre-ics-attack",
                                                                       "mitre-mobile-attack")]
                                  })
  attck.mitigation <- lapply(mitigation.raw,
                             function(x)
                               x[names(x) %in% c("id", "type", "name", "description",
                                                 "created", "modified",
                                                 "object_marking_refs", "created_by_ref")])
  attck.mitigation <- plyr::ldply(attck.mitigation, rbind.data.frame)
  attck.mitigation$mitreid <- attck.mitigation$id
  attck.mitigation$id <- attck.mitigation$.id
  attck.mitigation <- attck.mitigation[, c("id", "mitreid", "type", "name", "description",
                                           "created", "modified",
                                           "object_marking_refs", "created_by_ref")]

  ####### ATT&CK DATA
  attck <- list(tactic = attck.tactics,
                technique = attck.techniques,
                group = attck.groups,
                software = attck.software,
                mitigation = attck.mitigation)

  df <- plyr::ldply(attck, rbind.data.frame)
  df$idrel <- df$type
  df$type <- df$.id
  df$.id <- NULL

  attck <- list(nodes = df,
                relations = attck.relations)
  return(attck)
}
