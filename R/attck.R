#' ETL process that download current attck definitions and return a list of
#' data frames for each object. The list also contains a visNetwork object with
#' ATT&CK objects as nodes and all relations as edges.
#'
#' @param verbose Default set as FALSE
#'
#' @return list of data frames
getAttckData <- function(verbose = FALSE) {
  if (verbose) print(paste("[*][ATT&CK] Starting parsers ..."))

  if (verbose) print(paste("[*][ATT&CK] Building TACTICS ..."))
  tactics <- buildAttckTactics(verbose)

  if (verbose) print(paste("[*][ATT&CK] Building TECHNIQUES ..."))
  techniques <- buildAttckTechniques(verbose)

  if (verbose) print(paste("[*][ATT&CK] Building MITIGATIONS ..."))
  mitigations <- buildAttckMitigations(verbose)

  if (verbose) print(paste("[*][ATT&CK] Building GROUPS ..."))
  groups <- buildAttckGroups(verbose)

  if (verbose) print(paste("[*][ATT&CK] Building SOFTWARE ..."))
  software <- buildAttckSoftware(verbose)

  if (verbose) print(paste("[*][ATT&CK] Building RELATIONS ..."))
  relations <- buildAttckRelations(verbose)

  attck_nodes <- createATTCKnodes(tactics, techniques, mitigations,
                                  groups, software, verbose)
  attck_edges <- createATTCKedges(tactics, techniques, mitigations,
                                  groups, software, relations, verbose)

  attck <- list(tactics = tactics,
                techniques = techniques,
                mitigations = mitigations,
                groups = groups,
                software = software,
                attcknet = list(nodes = attck_nodes,
                                edges = attck_edges))

  return(attck)
}

#' Create edges from ATTCK data frames
#'
#' @param tactics data.frame
#' @param techniques data.frame
#' @param mitigations data.frame
#' @param groups data.frame
#' @param software data.frame
#' @param relations data.frame
#' @param verbose Default set as FALSE
#'
#' @return data.frame
createATTCKedges <- function(tactics, techniques, mitigations, groups, software, relations, verbose) {
  # CREATE NETWORK EDGES
  if (verbose) print(paste("[*][ATT&CK] Creating graph EDGES ..."))
  attck_edges <- data.frame(
    from = character(0),
    to = character(0),
    label = character(0),
    arrows = numeric(0),
    title = character(0),
    dashes = logical(0),
    team = character(0)
  )
  rels <- relations[, c("id.cti", "src", "srctype", "relation", "dst", "dsttype", "x_mitre_deprecated")]

  mapids <- tactics[, c("mitreid", "id")]
  names(mapids) <- c("mitreid", "id.cti")
  mapids <- rbind(mapids, dplyr::select(techniques, "mitreid", "id.cti"))
  mapids <- rbind(mapids, dplyr::select(mitigations, "mitreid", "id.cti"))
  mapids <- rbind(mapids, dplyr::select(groups, "mitreid", "id.cti"))
  mapids <- rbind(mapids, dplyr::select(software, "mitreid", "id.cti"))

  rels <- dplyr::left_join(rels, mapids, by = c("src"="id.cti"), keep = FALSE)
  rels$from <- rels$mitreid
  rels$mitreid <- NULL
  rels <- dplyr::left_join(rels, mapids, by = c("dst"="id.cti"), keep = FALSE)
  rels$to <- rels$mitreid
  rels$mitreid <- NULL
  ctiedges <- data.frame(
    from = rels$from,
    to = rels$to,
    label = rels$relation,
    arrows = rep("to", nrow(rels)),
    title = rels$relation,
    dashes = rels$x_mitre_deprecated,
    team = rep("PURPLE", nrow(rels))
  )
  if (verbose) print(paste("[*][ATT&CK] Adding", nrow(ctiedges), "default relationships ..."))
  attck_edges <- rbind(attck_edges, ctiedges)

  # Technique -> Tactic
  tech2tact <- techniques[, c("mitreid", "tactic", "revoked", "x_mitre_deprecated")]
  tech2tact$dashes <- tech2tact$revoked | tech2tact$x_mitre_deprecated
  s <- strsplit(tech2tact$tactic, split = ", ")
  tech2tact <- data.frame(mitreid = rep(tech2tact$mitreid, sapply(s, length)),
                          tactic = unlist(s),
                          dashes = rep(tech2tact$dashes, sapply(s, length)))
  tech2tact <- tech2tact[which(!is.na(tech2tact$tactic)), ]
  s <- tactics[, c("mitreid", "x_mitre_shortname")]
  tech2tact <- dplyr::left_join(tech2tact, s, by = c("tactic" = "x_mitre_shortname"))
  names(tech2tact) <- c("from", "tactic", "dashes", "to")
  tech2tact <- tech2tact[which(!is.na(tech2tact$to)), ]
  tech2tact$label <- rep("kill_chain_phase", nrow(tech2tact))
  tech2tact$arrows <- rep("to", nrow(tech2tact))
  tech2tact$title <- rep("kill_chain_phase", nrow(tech2tact))
  tech2tact$team <- rep("RED", nrow(tech2tact))
  tech2tact$tactic <- NULL
  if (verbose) print(paste("[*][ATT&CK] Adding", nrow(tech2tact), "Tactic relationships ..."))
  attck_edges <- rbind(attck_edges, tech2tact)

  # Technique -> CAPEC
  tech2capec <- techniques[!is.na(techniques$capec), c("mitreid", "capec")]
  tech2capec <-
    tidyr::unnest(
      dplyr::mutate(tech2capec,
                    capec = strsplit(as.character(.data$capec), ",")),
      "capec")
  names(tech2capec) <- c("from", "to")
  tech2capec$from <- stringr::str_trim(tech2capec$from)
  tech2capec$to <- stringr::str_trim(tech2capec$to)
  tech2capec$label <- rep("include", nrow(tech2capec))
  tech2capec$arrows <- rep("to", nrow(tech2capec))
  tech2capec$title <- rep("include", nrow(tech2capec))
  tech2capec$dashes <- rep(TRUE, nrow(tech2capec))
  tech2capec$team <- rep("RED", nrow(tech2capec))
  if (verbose) print(paste("[*][ATT&CK] Adding", nrow(tech2capec), "CAPEC relationships ..."))
  attck_edges <- rbind(attck_edges, tech2capec)

  # Technique -> CVE
  tech2cve <- techniques[!is.na(techniques$cve), c("mitreid", "cve")]
  tech2cve <-
    tidyr::unnest(
      dplyr::mutate(tech2cve,
                    cve = strsplit(as.character(.data$cve), ",")),
      "cve")
  names(tech2cve) <- c("from", "to")
  tech2cve$from <- stringr::str_trim(tech2cve$from)
  tech2cve$to <- stringr::str_trim(tech2cve$to)
  tech2cve$label <- rep("exploit", nrow(tech2cve))
  tech2cve$arrows <- rep("to", nrow(tech2cve))
  tech2cve$title <- rep("exploit", nrow(tech2cve))
  tech2cve$dashes <- rep(TRUE, nrow(tech2cve))
  tech2cve$team <- rep("RED", nrow(tech2cve))
  if (verbose) print(paste("[*][ATT&CK] Adding", nrow(tech2cve), "CVE relationships ..."))
  attck_edges <- rbind(attck_edges, tech2cve)

  return(attck_edges)
}

createATTCKnodes <- function(tactics, techniques, mitigations, groups, software, verbose) {
  # CREATE NETWORK NODES
  if (verbose) print(paste("[*][ATT&CK] Creating graph NODES ..."))
  attck_nodes <- data.frame(
    id = character(0),
    label = character(0),
    group = character(0),
    value = numeric(0),
    shape = character(0),
    title = character(0),
    color = character(0),
    shadow = logical(0)
  )

  # TACTIC NODES
  ctinodes <- data.frame(
    id = tactics$mitreid,
    label = tactics$mitreid,
    group = tactics$type,
    value = rep(5, nrow(tactics)),
    shape = rep("triangle", nrow(tactics)),
    title = paste0("<p><b>", tactics$name, "</b><br>", tactics$description, "</p>"),
    color = rep("gold", nrow(tactics)),
    shadow = tactics$x_mitre_deprecated,
    team = rep("RED", nrow(tactics))
  )
  if (verbose) print(paste("[*][ATT&CK] Adding", nrow(ctinodes), "tactic nodes ..."))
  attck_nodes <- rbind(attck_nodes, ctinodes)

  # TECHNIQUES NODES
  ctinodes <- data.frame(
    id = techniques$mitreid,
    label = techniques$mitreid,
    group = techniques$type,
    value = rep(4, nrow(techniques)),
    shape = rep("square", nrow(techniques)),
    title = paste0("<p><b>", techniques$name, "</b><br>", techniques$description, "</p>"),
    color = rep("lightblue", nrow(techniques)),
    shadow = techniques$x_mitre_deprecated,
    team = rep("RED", nrow(techniques))
  )
  if (verbose) print(paste("[*][ATT&CK] Adding", nrow(ctinodes), "technique nodes ..."))
  attck_nodes <- rbind(attck_nodes, ctinodes)

  # MITIGATION NODES
  ctinodes <- data.frame(
    id = mitigations$mitreid,
    label = mitigations$mitreid,
    group = mitigations$type,
    value = rep(4, nrow(mitigations)),
    shape = rep("square", nrow(mitigations)),
    title = paste0("<p><b>", mitigations$name, "</b><br>", mitigations$description, "</p>"),
    color = rep("lightblue", nrow(mitigations)),
    shadow = mitigations$x_mitre_deprecated,
    team = rep("RED", nrow(mitigations))
  )
  if (verbose) print(paste("[*][ATT&CK] Adding", nrow(ctinodes), "mitigation nodes ..."))
  attck_nodes <- rbind(attck_nodes, ctinodes)

  # GROUPS NODES
  ctinodes <- data.frame(
    id = groups$mitreid,
    label = groups$mitreid,
    group = groups$type,
    value = rep(4, nrow(groups)),
    shape = rep("square", nrow(groups)),
    title = paste0("<p><b>", groups$name, "</b><br>", groups$description, "</p>"),
    color = rep("lightblue", nrow(groups)),
    shadow = groups$x_mitre_deprecated,
    team = rep("RED", nrow(groups))
  )
  if (verbose) print(paste("[*][ATT&CK] Adding", nrow(ctinodes), "group nodes ..."))
  attck_nodes <- rbind(attck_nodes, ctinodes)

  # SOFTWARE NODES
  ctinodes <- data.frame(
    id = software$mitreid,
    label = software$mitreid,
    group = software$type,
    value = rep(4, nrow(software)),
    shape = rep("square", nrow(software)),
    title = paste0("<p><b>", software$name, "</b><br>", software$description, "</p>"),
    color = rep("lightblue", nrow(software)),
    shadow = software$x_mitre_deprecated,
    team = rep("RED", nrow(software))
  )
  if (verbose) print(paste("[*][ATT&CK] Adding", nrow(ctinodes), "software nodes ..."))
  attck_nodes <- rbind(attck_nodes, ctinodes)

  return(attck_nodes)
}
