#' ETL process that download current attck definitions and return a list of
#' data frames for each object. The list also contains a visNetwork object with
#' ATT&CK objects as nodes and all relations as edges.
#'
#' @param verbose Default set as FALSE
#'
#' @return list of data frames
#' @export
#'
#' @examples
#' \donttest{
#' attck <- mitre::getAttckData()
#' attck_tactics <- attck[["tactics"]]
#' attck_techniques <- attck[["techniques"]]
#' attck_groups <- attck[["groups"]]
#' attck_software <- attck[["software"]]
#' attck_mitigation <- attck[["mitigation"]]
#' attcknet <- attck[["attcknet"]]
#' }
getAttckData <- function(verbose = FALSE) {
  # ATT&CK MOBILE
  if (verbose) print(paste("[*][ATT&CK][MOB] Download ATT&CK MOBILE..."))
  attck.mob.raw.url <- "https://raw.githubusercontent.com/mitre/cti/master/mobile-attack/mobile-attack.json"
  attck.mob.raw.file <- tempfile(pattern = "mitre_attckmob_", fileext = ".json")
  utils::download.file(url = attck.mob.raw.url, destfile = attck.mob.raw.file, quiet = !verbose)

  if (verbose) print(paste("[*][ATT&CK][MOB] Parsing ..."))
  attck.mob.raw <- jsonlite::fromJSON(attck.mob.raw.file)[["objects"]]

  attck.mob.raw$mitreid <- sapply(attck.mob.raw$external_references,
                                    function(x)
                                      x[x$source_name %in% c("mitre-attack",
                                                             "mitre-ics-attack",
                                                             "mitre-mobile-attack"),
                                        "external_id"])
  attck.mob.raw$kill_chain_phases <- sapply(attck.mob.raw$kill_chain_phases,
                                            function(x)
                                              as.character(jsonlite::toJSON(x, null = "list")))
  attck.mob.raw$x_mitre_platforms <- sapply(attck.mob.raw$x_mitre_platforms,
                                            function(x)
                                              as.character(jsonlite::toJSON(x, null = "list")))

  mob <- attck.mob.raw[c("id", "mitreid", "type", "name", "description",
                         "x_mitre_is_subtechnique", "x_mitre_deprecated",
                         "x_mitre_version", "x_mitre_old_attack_id",
                         "x_mitre_detection", "revoked", "source_ref",
                         "relationship_type", "target_ref", "identity_class",
                         "x_mitre_shortname", "definition_type",
                         "kill_chain_phases", "x_mitre_platforms",
                         "created_by_ref", "modified", "created")]
  mob$x_mitre_deprecated[is.na(mob$x_mitre_deprecated)] <- FALSE
  mob$revoked[is.na(mob$revoked)] <- FALSE
  mob <- mob[which(!mob$x_mitre_deprecated), ]
  mob <- mob[which(!mob$revoked), ]
  mob.rels <- mob[mob$type == "relationship", ]
  mob.rels$mitreid <- NULL
  mob.rels <- mob.rels[, colSums(is.na(mob.rels)) < nrow(mob.rels)]
  mob <- mob[mob$type %in% c("x-mitre-tactic",
                             "attack-pattern",
                             "intrusion-set",
                             "malware", "tool",
                             "course-of-action"),]
  mob$mitreid <- unlist(mob$mitreid)
  mob <- mob[, colSums(is.na(mob)) < nrow(mob)]

  tactics.raw <- mob[mob$type == "x-mitre-tactic", ]
  techniques.raw <- mob[mob$type == "attack-pattern", ]
  groups.raw <- mob[mob$type  == "intrusion-set", ]
  software.raw <- mob[mob$type %in% c("malware", "tool"), ]
  mitigation.raw <- mob[mob$type == "course-of-action", ]

  # ATT&CK ENTERPRISE
  if (verbose) print(paste("[*][ATT&CK][ENT] Download ATT&CK ENTERPRISE ..."))
  attck.ent.raw.url <- "https://raw.githubusercontent.com/mitre/cti/master/enterprise-attack/enterprise-attack.json"
  attck.ent.raw.file <- tempfile(pattern = "mitre_attckent_", fileext = ".json")
  utils::download.file(url = attck.ent.raw.url, destfile = attck.ent.raw.file, quiet = !verbose)

  if (verbose) print(paste("[*][ATT&CK][ENT] Parsing ..."))
  attck.ent.raw <- jsonlite::fromJSON(attck.ent.raw.file)[["objects"]]

  attck.ent.raw$mitreid <- sapply(attck.ent.raw$external_references,
                                  function(x)
                                    x[x$source_name %in% c("mitre-attack",
                                                           "mitre-ics-attack",
                                                           "mitre-mobile-attack"),
                                      "external_id"])
  attck.ent.raw$kill_chain_phases <- sapply(attck.ent.raw$kill_chain_phases,
                                            function(x)
                                              as.character(jsonlite::toJSON(x, null = "list")))
  attck.ent.raw$x_mitre_platforms <- sapply(attck.ent.raw$x_mitre_platforms,
                                            function(x)
                                              as.character(jsonlite::toJSON(x, null = "list")))

  ent <- attck.ent.raw[c("id", "mitreid", "type", "name", "description",
                         "x_mitre_is_subtechnique", "x_mitre_deprecated",
                         "x_mitre_version", "x_mitre_old_attack_id",
                         "x_mitre_detection", "revoked", "source_ref",
                         "relationship_type", "target_ref", "identity_class",
                         "x_mitre_shortname", "definition_type",
                         "kill_chain_phases", "x_mitre_platforms",
                         "created_by_ref", "modified", "created")]

  ent$x_mitre_deprecated[is.na(ent$x_mitre_deprecated)] <- FALSE
  ent$revoked[is.na(ent$revoked)] <- FALSE
  ent <- ent[which(!ent$x_mitre_deprecated), ]
  ent <- ent[which(!ent$revoked), ]
  ent.rels <- ent[ent$type == "relationship", ]
  ent.rels$mitreid <- NULL
  ent.rels <- ent.rels[, colSums(is.na(ent.rels)) < nrow(ent.rels)]
  ent <- ent[ent$type %in% c("x-mitre-tactic",
                             "attack-pattern",
                             "intrusion-set",
                             "malware", "tool",
                             "course-of-action"),]
  ent$mitreid <- unlist(ent$mitreid)
  ent <- ent[, colSums(is.na(ent)) < nrow(ent)]

  tactics.raw <- rbind(tactics.raw, ent[ent$type == "x-mitre-tactic", ])
  techniques.raw <- rbind(techniques.raw, ent[ent$type == "attack-pattern", ])
  groups.raw <- rbind(groups.raw, ent[ent$type  == "intrusion-set", ])
  software.raw <- rbind(software.raw, ent[ent$type %in% c("malware", "tool"), ])
  mitigation.raw <- rbind(mitigation.raw, ent[ent$type == "course-of-action", ])

  if (verbose) print(paste("[*][ATT&CK][graph] Building nodes ..."))
  ## NODES
  # Ref: https://datastorm-open.github.io/visNetwork/nodes.html
  nodes <- data.frame(
    id = character(0),
    label = character(0),
    group = character(0),
    value = numeric(0),
    shape = character(0),
    title = character(0),
    color = character(0),
    shadow = logical(0)
  )
  attck_nodes <- nodes

  attck.raw <- rbind(tactics.raw, techniques.raw, groups.raw, software.raw, mitigation.raw)
  nodetype <- data.frame(type = c("x-mitre-tactic", "attack-pattern", "intrusion-set",
                                "malware", "tool", "course-of-action"),
                       shape = c("triangle", "square", "ellipse",
                                 "box", "box", "diamond"),
                       value = c(5, 4, 4, 4, 4, 5),
                       color = c("darkred", "orange", "brown",
                                 "red", "magenta", "grey"))
  attck.df <- dplyr::left_join(attck.raw, nodetype, by = "type")
  attck.df$id <- attck.df$mitreid
  attck.df$label <- attck.df$id
  attck.df$group <- attck.df$type
  attck.df$title <- paste0("<p><b>", attck.df$name, "</b><br>", attck.df$description, "</p>")
  attck.df$shadow <- rep(FALSE, nrow(attck.df))
  attck.df <- attck.df[, names(nodes)]

  attck_nodes <- unique(rbind(attck_nodes, attck.df))

  if (verbose) print(paste("[*][ATT&CK][graph] Building edges ..."))
  ## EDGES
  # Ref: https://datastorm-open.github.io/visNetwork/edges.html
  edges <- data.frame(
    from = character(0),
    to = character(0),
    label = character(0),
    arrows = numeric(0),
    title = character(0)
  )

  relations <- rbind(mob.rels, ent.rels)

  attck.df <- relations[, c("source_ref", "target_ref", "relationship_type")]
  attck.df <- attck.df[attck.df$relationship_type != "revoked-by", ]
  attck.df <- unique(attck.df)

  attck.df <- dplyr::left_join(attck.df,
                               attck.raw[, c("id", "mitreid")],
                               by = c("source_ref" = "id"))
  attck.df$from <- attck.df$mitreid
  attck.df$mitreid <- NULL
  attck.df$source_ref <- NULL
  attck.df <- unique(attck.df)
  attck.df <- dplyr::left_join(attck.df,
                               attck.raw[, c("id", "mitreid")],
                               by = c("target_ref" = "id"))
  attck.df$to <- attck.df$mitreid
  attck.df$mitreid <- NULL
  attck.df$target_ref <- NULL
  attck.df <- unique(attck.df)
  attck.df$label <- attck.df$relationship_type
  attck.df$title <- attck.df$label
  attck.df$arrows <- rep("to", nrow(attck.df))

  attck_edges <- attck.df[, names(edges)]
  ###### CTI ISSUE: remove relations between revoked or deprecated objects
  attck_edges_rev <- attck_edges[which(is.na(attck_edges$from) | is.na(attck_edges$to)), ]
  attck_edges <- attck_edges[which(!is.na(attck_edges$from) & !is.na(attck_edges$to)), ]

  if (verbose) print(paste("[!][ATT&CK][graph] CTI issue patch ..."))
  ###### CTI ISSUE: some unique cti-mitre-id are used in different objects
  n_occur <- data.frame(table(attck_nodes$id))
  attck_nodes_bad <- attck_nodes[which(attck_nodes$id %in% as.character(n_occur[n_occur$Freq > 1, "Var1"])), ]
  attck_nodes <- attck_nodes[-which(attck_nodes$id %in% as.character(n_occur[n_occur$Freq > 1, "Var1"])), ]

  if (verbose) print(paste("[*][ATT&CK][graph] Building visNetwork ..."))
  attck_edges$team <- rep("RED", nrow(attck_edges))
  attcknet <- list(nodes = attck_nodes,
                   edges = attck_edges)

  tactics.raw <- tactics.raw[, colSums(is.na(tactics.raw)) < nrow(tactics.raw)]
  techniques.raw <- techniques.raw[, colSums(is.na(techniques.raw)) < nrow(techniques.raw)]
  groups.raw <- groups.raw[, colSums(is.na(groups.raw)) < nrow(groups.raw)]
  software.raw <- software.raw[, colSums(is.na(software.raw)) < nrow(software.raw)]
  mitigation.raw <- mitigation.raw[, colSums(is.na(mitigation.raw)) < nrow(mitigation.raw)]

  if (verbose) print(paste("[*][ATT&CK] Building output ..."))
  tactics.raw$description <- stringr::str_trim(tactics.raw$description)
  tactics.raw$kill_chain_phases <- NULL
  tactics.raw$x_mitre_platforms <- NULL
  groups.raw$kill_chain_phases <- NULL
  groups.raw$x_mitre_platforms <- NULL
  software.raw$kill_chain_phases <- NULL
  mitigation.raw$kill_chain_phases <- NULL
  mitigation.raw$x_mitre_platforms <- NULL
  attck <- list(tactics = tactics.raw,
                techniques = techniques.raw,
                groups = groups.raw,
                software = software.raw,
                mitigation = mitigation.raw,
                attcknet = attcknet)

  return(attck)
}
