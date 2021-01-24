#' ETL process that download current attck definitions and return a list of
#' data frames for each object. The list also contains a visNetwork object with
#' ATT&CK objects as nodes and all relations as edges.
#'
#' @param verbose Default set as FALSE
#'
#' @return list of data frames
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
  if (verbose) print(paste("[*][ATT&CK] Starting parsers ..."))
  tactics <- buildAttckTactics(verbose)

  if (verbose) print(paste("[*][ATT&CK] Techniques enrichment with latest CTI definitions ..."))
  techniques <- attck$techniques
  techniques$modified <- as.POSIXct.POSIXlt(strptime(techniques$modified, format = "%Y-%m-%dT%H:%M:%S"))
  techniques$created <- as.POSIXct.POSIXlt(strptime(techniques$created, format = "%Y-%m-%dT%H:%M:%S"))

  # Adapt CTI column names
  cti.tech <- buildAttckTechniques(verbose)
  techmap <- c("id"="id.cti",
               "mitreid" = "entry.id",
               "name" = "entry.title",
               "description" = "entry.text",
               "summary" = "description",
               "x_mitre_deprecated" = "deprecated",
               "x_mitre_detection" = "detection")
  cti.tech <- dplyr::rename(cti.tech, dplyr::all_of(techmap))
  # cti.tech$type <- rep("attack-pattern", nrow(cti.tech))
  # cti.tech$x_mitre_deprecated <- as.logical(cti.tech$x_mitre_deprecated)
  # cti.tech$revoked <- as.logical(cti.tech$revoked)
  cti.tech <- cti.tech[, c(names(techniques)[which(names(techniques)
                                                   %in% names(cti.tech))],
                           names(cti.tech)[which(!(names(cti.tech)
                                                   %in% names(techniques)))])]
  # Enrich nodes in both data sets
  selectedCols <- c("mitreid", names(cti.tech)[which(!(names(cti.tech) %in% names(techniques)))])
  techniques <- dplyr::left_join(techniques, cti.tech[, selectedCols],
                              by = "mitreid")
  # Add techniques only in CTI
  cti.tech <- cti.tech[!(cti.tech$mitreid %in% techniques$mitreid), ]
  techniques <- dplyr::bind_rows(techniques, cti.tech)
  techniques$x_mitre_platforms <- NULL

  attck$tactics <- tactics
  attck$techniques <- techniques

  # Include CTI data to network
  attck.df <- attck$tactics[!(attck$tactics$mitreid %in% attck$attcknet$nodes$id), ]
  ctinodes <- data.frame(
    id = attck.df$mitreid,
    label = attck.df$mitreid,
    group = attck.df$type,
    value = rep(5, nrow(attck.df)),
    shape = rep("triangle", nrow(attck.df)),
    title = paste0("<p><b>", attck.df$name, "</b><br>", attck.df$description, "</p>"),
    color = rep("gold", nrow(attck.df)),
    shadow = attck.df$x_mitre_deprecated,
    team = rep("RED", nrow(attck.df))
  )
  if (verbose) print(paste("[*][ATT&CK] Adding", nrow(ctinodes), "new tactics ..."))
  attck$attcknet$nodes <- rbind(attck$attcknet$nodes, ctinodes)

  attck.df <- attck$techniques[!(attck$techniques$mitreid %in% attck$attcknet$nodes$id), ]
  ctinodes <- data.frame(
    id = attck.df$mitreid,
    label = attck.df$mitreid,
    group = attck.df$type,
    value = rep(4, nrow(attck.df)),
    shape = rep("square", nrow(attck.df)),
    title = paste0("<p><b>", attck.df$name, "</b><br>", attck.df$description, "</p>"),
    color = rep("lightblue", nrow(attck.df)),
    shadow = attck.df$x_mitre_deprecated,
    team = rep("RED", nrow(attck.df))
  )
  if (verbose) print(paste("[*][ATT&CK] Adding", nrow(ctinodes), "new techniques ..."))
  attck$attcknet$nodes <- rbind(attck$attcknet$nodes, ctinodes)

  # Include CTI relationships

  edges <- data.frame(
    from = character(0),
    to = character(0),
    label = character(0),
    arrows = numeric(0),
    title = character(0),
    dashes = logical(0),
    team = character(0)
  )

  # Technique -> CAPEC
  tech2capec <- techniques[!is.na(techniques$capec), c("mitreid", "capec")]
  tech2capec <-
    tidyr::unnest(
      dplyr::mutate(tech2capec,
                    capec = strsplit(as.character(capec), ",")),
      capec)
  names(tech2capec) <- c("from", "to")
  tech2capec$from <- stringr::str_trim(tech2capec$from)
  tech2capec$to <- stringr::str_trim(tech2capec$to)
  tech2capec$label <- rep("include", nrow(tech2capec))
  tech2capec$arrows <- rep("to", nrow(tech2capec))
  tech2capec$title <- rep("include", nrow(tech2capec))
  tech2capec$dashes <- rep(TRUE, nrow(tech2capec))
  tech2capec$team <- rep("RED", nrow(tech2capec))

  if (verbose) print(paste("[*][ATT&CK] Adding", nrow(tech2capec), "CAPEC relationships ..."))
  attck[["attcknet"]][["edges"]] <- rbind(attck[["attcknet"]][["edges"]], tech2capec)

  # Technique -> CVE
  tech2cve <- techniques[!is.na(techniques$cve), c("mitreid", "cve")]
  tech2cve <-
    tidyr::unnest(
      dplyr::mutate(tech2cve,
                    cve = strsplit(as.character(cve), ",")),
      cve)
  names(tech2cve) <- c("from", "to")
  tech2cve$from <- stringr::str_trim(tech2cve$from)
  tech2cve$to <- stringr::str_trim(tech2cve$to)
  tech2cve$label <- rep("exploit", nrow(tech2cve))
  tech2cve$arrows <- rep("to", nrow(tech2cve))
  tech2cve$title <- rep("exploit", nrow(tech2cve))
  tech2cve$dashes <- rep(TRUE, nrow(tech2cve))
  tech2cve$team <- rep("RED", nrow(tech2cve))

  if (verbose) print(paste("[*][ATT&CK] Adding", nrow(tech2cve), "CVE relationships ..."))
  attck[["attcknet"]][["edges"]] <- rbind(attck[["attcknet"]][["edges"]], tech2cve)

  return(attck)
}

#' ETL process that download current attck definitions and return a list of
#' data frames for each object. The list also contains a visNetwork object with
#' ATT&CK objects as nodes and all relations as edges.
#'
#' @param verbose Default set as FALSE
#'
#' @return list of data frames
#'
#' @examples
#' \donttest{
#' attck <- mitre::parseAttckData()
#' }
parseAttckData <- function(verbose = FALSE) {
  # ATT&CK MOBILE
  if (verbose) print(paste("[*][ATT&CK][MOB] Parsing ..."))
  attck.mob.raw <- jsonlite::fromJSON("data-raw/attack-mobile.json")[["objects"]]

  # Omit deprecated and revoked objects
  attck.mob.raw$x_mitre_deprecated[is.na(attck.mob.raw$x_mitre_deprecated)] <- FALSE
  attck.mob.raw$revoked[is.na(attck.mob.raw$revoked)] <- FALSE
  attck.mob.raw <- attck.mob.raw[which(!attck.mob.raw$x_mitre_deprecated), ]
  attck.mob.raw <- attck.mob.raw[which(!attck.mob.raw$revoked), ]

  # Extract MITRE id from external references
  attck.mob.raw$mitreid <- sapply(attck.mob.raw$external_references,
                                    function(x)
                                      x[x$source_name %in% c("mitre-attack",
                                                             "mitre-ics-attack",
                                                             "mitre-mobile-attack"),
                                        "external_id"])

  # Extract relations between techniques and tactics from kill chain
  mob.rels.tt <- attck.mob.raw[which(sapply(attck.mob.raw$kill_chain_phases, length) > 0),
                     c("id", "kill_chain_phases")]
  names(mob.rels.tt$kill_chain_phases) <- mob.rels.tt$id
  mob.rels.tt <- mob.rels.tt$kill_chain_phases
  mob.rels.tt <- dplyr::bind_rows(mob.rels.tt, .id = "id")
  mob.rels.tt <- dplyr::left_join(mob.rels.tt,
                                  attck.mob.raw[, c("id", "x_mitre_shortname")],
                                  by = c("phase_name" = "x_mitre_shortname"))
  mob.rels.tt <- mob.rels.tt[ , c("id.x", "id.y")]
  names(mob.rels.tt) <- c("source_ref", "target_ref")
  mob.rels.tt$relationship_type <- rep("accomplishes", nrow(mob.rels.tt))
  attck.mob.raw$kill_chain_phases <- sapply(attck.mob.raw$kill_chain_phases,
                                            function(x)
                                              as.character(jsonlite::toJSON(x, null = "list")))

  # Extract relations between techniques and platforms from platforms
  # mob.platforms <- attck.mob.raw[which(sapply(attck.mob.raw$x_mitre_platforms, length) > 0),
  #                                c("id", "x_mitre_platforms")]
  # names(mob.platforms$x_mitre_platforms) <- mob.platforms$id
  # mob.platforms <- mob.platforms$x_mitre_platforms %>%
  #   bind_rows(.id = "id") %>%
  #   gather(key = "from", "to") %>%
  #   unique
  attck.mob.raw$x_mitre_platforms <- sapply(attck.mob.raw$x_mitre_platforms,
                                            function(x)
                                              as.character(jsonlite::toJSON(x, null = "list")))

  # Tidy mobile data frame
  mob <- attck.mob.raw[c("id", "mitreid", "type", "name", "description",
                         "x_mitre_is_subtechnique", "x_mitre_deprecated",
                         "x_mitre_version", "x_mitre_old_attack_id",
                         "x_mitre_detection", "revoked", "source_ref",
                         "relationship_type", "target_ref", "identity_class",
                         "x_mitre_shortname", "definition_type",
                         "kill_chain_phases", "x_mitre_platforms",
                         "created_by_ref", "modified", "created")]

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
  mob$domain <- rep("mobile-attack", nrow(mob))

  tactics.raw <- mob[mob$type == "x-mitre-tactic", ]
  techniques.raw <- mob[mob$type == "attack-pattern", ]
  groups.raw <- mob[mob$type  == "intrusion-set", ]
  software.raw <- mob[mob$type %in% c("malware", "tool"), ]
  mitigation.raw <- mob[mob$type == "course-of-action", ]

  # ATT&CK ENTERPRISE
  if (verbose) print(paste("[*][ATT&CK][ENT] Parsing ..."))
  attck.ent.raw <- jsonlite::fromJSON("data-raw/attack-enterprise.json")[["objects"]]

  # Omit deprecated and revoked objects
  attck.ent.raw$x_mitre_deprecated[is.na(attck.ent.raw$x_mitre_deprecated)] <- FALSE
  attck.ent.raw$revoked[is.na(attck.ent.raw$revoked)] <- FALSE
  attck.ent.raw <- attck.ent.raw[which(!attck.ent.raw$x_mitre_deprecated), ]
  attck.ent.raw <- attck.ent.raw[which(!attck.ent.raw$revoked), ]

  # Extract MITRE id from external references
  attck.ent.raw$mitreid <- sapply(attck.ent.raw$external_references,
                                  function(x)
                                    x[x$source_name %in% c("mitre-attack",
                                                           "mitre-ics-attack",
                                                           "mitre-mobile-attack"),
                                      "external_id"])

  # Extract relations between techniques and tactics from kill chain
  ent.rels.tt <- attck.ent.raw[which(sapply(attck.ent.raw$kill_chain_phases, length) > 0),
                               c("id", "kill_chain_phases")]
  names(ent.rels.tt$kill_chain_phases) <- ent.rels.tt$id
  ent.rels.tt <- ent.rels.tt$kill_chain_phases
  ent.rels.tt <- dplyr::bind_rows(ent.rels.tt, .id = "id")
  ent.rels.tt <- dplyr::left_join(ent.rels.tt,
                                  attck.ent.raw[, c("id", "x_mitre_shortname")],
                                  by = c("phase_name" = "x_mitre_shortname"))
  ent.rels.tt <- ent.rels.tt[ , c("id.x", "id.y")]
  names(ent.rels.tt) <- c("source_ref", "target_ref")
  ent.rels.tt$relationship_type <- rep("accomplishes", nrow(ent.rels.tt))
  attck.ent.raw$kill_chain_phases <- sapply(attck.ent.raw$kill_chain_phases,
                                            function(x)
                                              as.character(jsonlite::toJSON(x, null = "list")))

  # Extract relations between techniques and platforms from platforms
  attck.ent.raw$x_mitre_platforms <- sapply(attck.ent.raw$x_mitre_platforms,
                                            function(x)
                                              as.character(jsonlite::toJSON(x, null = "list")))

  # Tidy mobile data frame
  ent <- attck.ent.raw[c("id", "mitreid", "type", "name", "description",
                         "x_mitre_is_subtechnique", "x_mitre_deprecated",
                         "x_mitre_version", "x_mitre_old_attack_id",
                         "x_mitre_detection", "revoked", "source_ref",
                         "relationship_type", "target_ref", "identity_class",
                         "x_mitre_shortname", "definition_type",
                         "kill_chain_phases", "x_mitre_platforms",
                         "created_by_ref", "modified", "created")]

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
  ent$domain <- rep("enterprise-attack", nrow(ent))

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
                       color = c("gold", "lightblue", "indianred",
                                 "azure", "grey", "darkseagreen"))
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
  attck.df <- rbind(attck.df, mob.rels.tt, ent.rels.tt)
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
  n_occur <- as.data.frame(table(attck_nodes$id), stringsAsFactors = FALSE)
  attck_nodes_bad <- attck_nodes[which(attck_nodes$id %in% as.character(n_occur[n_occur$Freq > 1, "Var1"])), ]
  attck_nodes <- attck_nodes[which((attck_nodes$id %in% as.character(n_occur[n_occur$Freq == 1, "Var1"]))), ]

  if (verbose) print(paste("[*][ATT&CK][graph] Building visNetwork ..."))
  attck_nodes$team <- rep("RED", nrow(attck_nodes))
  attck_edges$dashes <- rep(FALSE, nrow(attck_edges))
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
  tactics.raw$modified <- as.POSIXct.POSIXlt(strptime(tactics.raw$modified, format = "%Y-%m-%dT%H:%M:%S"))
  tactics.raw$created <- as.POSIXct.POSIXlt(strptime(tactics.raw$created, format = "%Y-%m-%dT%H:%M:%S"))

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
