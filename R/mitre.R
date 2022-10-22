#' Download latest R data sets from Github previously parsed with this package.
#'
#' @param verbose default is FALSE
#'
#' @return list, containing standards as data frames
#' @export
#'
#' @examples
#' \dontrun{
#' standards <- mitre::getLatestDataSet(TRUE)
#' }
getLatestDataSet <- function(verbose = FALSE) {
  rawurl <- "https://github.com/motherhack3r/mitre-datasets/raw/master/historic/20220416/standards.rds"
  rawfile <- tempfile(fileext = ".rds")
  utils::download.file(url = rawurl, destfile = rawfile, quiet = !verbose)
  standards <- readRDS(rawfile)

  return(standards)
}

#' Create a list of nodes and edges related to all standards in data folder.
#'
#' @param verbose logical, FALSE by default. Change it to see the process messages.
#' @param as_igraph logical, TRUE by default. Change it to get list of nodes and edges.
#' @param standards list of data.frames
#'
#' @return list, containing nodes and edges as data frames
#' @export
#'
#' @examples
#' \dontrun{
#' mitrenet <- mitre::build_network(as_igraph = FALSE)
#' }
build_network <- function(standards = standards, verbose = FALSE, as_igraph = TRUE) {
  if (verbose) print(paste0("[NET] Building nodes ..."))
  nodes <- build_nodes(standards, verbose)
  if (verbose) print(paste0("[NET] Building edges ..."))
  edges <- build_edges(standards, verbose)

  if (verbose) print(paste0("[NET] Cleaning network ..."))
  edges <- dplyr::left_join(edges, nodes[, c("id", "standard")],
                            by = c("from_std" = "standard"))
  edges$from <- edges$id
  edges$id <- NULL
  edges <- dplyr::left_join(edges, nodes[, c("id", "standard")],
                            by = c("to_std" = "standard"))
  edges$to <- edges$id
  edges$id <- NULL

  # Select complete edges and its nodes
  edges <- edges[!(is.na(edges$to) | is.na(edges$from)),]
  nodes <- nodes[nodes$id %in% unique(c(edges$from, edges$to)), ]


  if (as_igraph) {
    nodes$shape[nodes$shape %in% c("triangle", "box")] <- "circle"
    mitrenet <- igraph::graph_from_data_frame(edges, directed = T, vertices = nodes)
  } else {
    mitrenet <- list(nodes = nodes, edges = edges)
  }
  return(mitrenet)
}

#' Transform all standards as nodes in a data frame.
#'
#' \code{id} : The id of the node unique value for all standard elements.
#' \code{label} : The label is the piece of text shown in or under the node, depending on the shape.
#' \code{group} : When not undefined, the group of node(s)
#' \code{type} : Used as subgroup to classify different object from
#' \code{value} : When a value is set, the nodes will be scaled using the options in the scaling object defined above.
#' \code{title} : Title to be displayed when the user hovers over the node. The title can be an HTML element or a string containing plain text or HTML.
#' \code{standard} : The id of the standard
#' \code{shape} : The shape defines what the node looks like. The types with the label inside of it are: ellipse, circle, database, box, text. The ones with the label outside of it are: image, circularImage, diamond, dot, star, triangle, triangleDown, square and icon.
#' \code{color} : Color for the node.
#' \code{hidden} : When true, the node will not be shown. It will still be part of the physics simulation though!
#' \code{mass} : Default to 1. The barnesHut physics model (which is enabled by default) is based on an inverted gravity model. By increasing the mass of a node, you increase it's repulsion. Values lower than 1 are not recommended.
#' \code{description} : Description could include extra information or nested data which include other columns from original data frame observation.
#'
#' @param standards list of data.frames
#' @param verbose logical, FALSE by default. Change it to see the process messages.
#'
#' @importFrom rlang .data
#'
#' @return data.frame
build_nodes <- function(standards = standards, verbose = FALSE) {
  nodes <- newNode()

  ### CPE
  if (verbose) print(paste0("[+][CPE] extracting nodes ..."))
  cpe.nodes <- standards$cpe$cpe.nist[, c("title", "cpe.23", "deprecated")]
  names(cpe.nodes) <- c("label", "title", "hidden")
  cpe.nodes$id <- rep(NA, nrow(cpe.nodes))
  cpe.nodes$group <- rep("cpe", nrow(cpe.nodes))
  cpe.nodes$type <- rep("cpe", nrow(cpe.nodes))
  cpe.nodes$value <- rep(1, nrow(cpe.nodes))
  cpe.nodes$standard <- cpe.nodes$title
  cpe.nodes$shape <- rep("circle", nrow(cpe.nodes))
  cpe.nodes$color <- rep("blue", nrow(cpe.nodes))
  cpe.nodes$mass <- rep(1, nrow(cpe.nodes))
  cpe.nodes$description <- cpe.nodes$label
  if (verbose) print("[+][CPE] Node label cleansing ...")
  # cpe.nodes$label <- janitor::make_clean_names(cpe.nodes$label)
  cpe.nodes$label <- stringr::str_replace_all(cpe.nodes$label, "[^[:print:]]|@|\\$", "")

  nodes <- dplyr::bind_rows(nodes, cpe.nodes)

  ### CVE
  if (verbose) print(paste0("[NET][CVE] extracting nodes ..."))
  cve.nodes <- standards$cve$cve.nist[, c("cve.id", "description", "cvss3.score", "cvss2.score", "references")]
  cve.nodes$id <- rep(NA, nrow(cve.nodes))
  cve.nodes$label <- cve.nodes$cve.id
  cve.nodes$group <- rep("cve", nrow(cve.nodes))
  cve.nodes$type <- rep("cve", nrow(cve.nodes))
  cve.nodes$cvss3.score[is.na(cve.nodes$cvss3.score)] <- -1
  cve.nodes$cvss2.score[is.na(cve.nodes$cvss2.score)] <- -1
  cve.nodes$value <- as.numeric(apply(cve.nodes, 1, function(x) max(x["cvss2.score"], x["cvss3.score"])))
  cve.nodes$title <- cve.nodes$cve.id
  cve.nodes$standard <- cve.nodes$cve.id
  cve.nodes$shape <- rep("triangle", nrow(cve.nodes))
  cve.nodes$color <- rep("blue", nrow(cve.nodes))
  cve.nodes$hidden <- rep(FALSE, nrow(cve.nodes))
  cve.nodes$mass <- cve.nodes$value
  cve.nodes$description <- cve.nodes$description
  cve.nodes <- cve.nodes[, names(nodes)]

  nodes <- dplyr::bind_rows(nodes, cve.nodes)

  ### CWE
  if (verbose) print(paste0("[NET][CWE] extracting nodes ..."))
  cwe.nodes <- dplyr::bind_rows(standards$cwe$cwe.weaknesses,
                                standards$cwe$cwe.categories,
                                standards$cwe$cwe.views)
  cwe.nodes$id <- rep(NA, nrow(cwe.nodes))
  cwe.nodes$label <- cwe.nodes$Code_Standard
  cwe.nodes$group <- rep("cwe", nrow(cwe.nodes))
  cwe.nodes$type <- tolower(cwe.nodes$CWE_Type)
  cwe.nodes$value <- rep(1, nrow(cwe.nodes))
  cwe.nodes$title <- cwe.nodes$Name
  cwe.nodes$standard <- cwe.nodes$Code_Standard
  cwe.nodes$shape <- rep("box", nrow(cwe.nodes))
  cwe.nodes$color <- rep("blue", nrow(cwe.nodes))
  cwe.nodes$hidden <- cwe.nodes$Status %in% c("Deprecated", "Obsolete")
  cwe.nodes$mass <- cwe.nodes$value
  cwe.nodes$description <- cwe.nodes$Description
  cwe.nodes <- cwe.nodes[, names(nodes)]

  nodes <- dplyr::bind_rows(nodes, cwe.nodes)

  ## ATT&CK MITRE
  ### Tactics
  if (verbose) print(paste0("[NET][ATTCK] extracting tactic nodes ..."))
  attck.nodes <- standards$attck$attck.tactics
  attck.nodes$id <- rep(NA, nrow(attck.nodes))
  attck.nodes$label <- attck.nodes$external_id
  attck.nodes$group <- rep("attck", nrow(attck.nodes))
  attck.nodes$type <- rep("tactic", nrow(attck.nodes))
  attck.nodes$value <- rep(1, nrow(attck.nodes))
  attck.nodes$title <- attck.nodes$name
  attck.nodes$standard <- attck.nodes$external_id
  attck.nodes$shape <- rep("box", nrow(attck.nodes))
  attck.nodes$color <- rep("red", nrow(attck.nodes))
  attck.nodes$hidden <- rep(FALSE, nrow(attck.nodes))
  attck.nodes$mass <- attck.nodes$value
  attck.nodes$description <- attck.nodes$description
  attck.nodes <- attck.nodes[, names(nodes)]

  nodes <- dplyr::bind_rows(nodes, attck.nodes)

  ### Techniques
  if (verbose) print(paste0("[NET][ATTCK] extracting technique nodes ..."))
  attck.nodes <- standards$attck$attck.techniques
  attck.nodes$id <- rep(NA, nrow(attck.nodes))
  attck.nodes$label <- attck.nodes$external_id
  attck.nodes$group <- rep("attck", nrow(attck.nodes))
  attck.nodes$type <- rep("technique", nrow(attck.nodes))
  attck.nodes$value <- rep(1, nrow(attck.nodes))
  attck.nodes$title <- attck.nodes$name
  attck.nodes$standard <- attck.nodes$external_id
  attck.nodes$shape <- rep("triangle", nrow(attck.nodes))
  attck.nodes$color <- rep("red", nrow(attck.nodes))
  attck.nodes$hidden <- rep(FALSE, nrow(attck.nodes))
  attck.nodes$mass <- attck.nodes$value
  attck.nodes$description <- attck.nodes$description
  attck.nodes <- attck.nodes[, names(nodes)]

  nodes <- dplyr::bind_rows(nodes, attck.nodes)

  ### Mitigations
  if (verbose) print(paste0("[NET][ATTCK] extracting mitigation nodes ..."))
  attck.nodes <- standards$attck$attck.mitigations
  attck.nodes$id <- rep(NA, nrow(attck.nodes))
  attck.nodes$label <- attck.nodes$external_id
  attck.nodes$group <- rep("attck", nrow(attck.nodes))
  attck.nodes$type <- rep("mitigation", nrow(attck.nodes))
  attck.nodes$value <- rep(1, nrow(attck.nodes))
  attck.nodes$title <- attck.nodes$name
  attck.nodes$standard <- attck.nodes$external_id
  attck.nodes$shape <- rep("square", nrow(attck.nodes))
  attck.nodes$color <- rep("red", nrow(attck.nodes))
  attck.nodes$hidden <- rep(FALSE, nrow(attck.nodes))
  attck.nodes$mass <- attck.nodes$value
  attck.nodes$description <- attck.nodes$description
  attck.nodes <- attck.nodes[, names(nodes)]

  nodes <- dplyr::bind_rows(nodes, attck.nodes)

  ### Software
  if (verbose) print(paste0("[NET][ATTCK] extracting software nodes ..."))
  attck.nodes <- standards$attck$attck.software
  attck.nodes$id <- rep(NA, nrow(attck.nodes))
  attck.nodes$label <- attck.nodes$external_id
  attck.nodes$group <- rep("attck", nrow(attck.nodes))
  attck.nodes$type <- as.character(sapply(attck.nodes$type,
                                          function(x) paste0("software-", x)))
  attck.nodes$value <- rep(1, nrow(attck.nodes))
  attck.nodes$title <- attck.nodes$name
  attck.nodes$standard <- attck.nodes$external_id
  attck.nodes$shape <- rep("square", nrow(attck.nodes))
  attck.nodes$color <- rep("red", nrow(attck.nodes))
  attck.nodes$hidden <- rep(FALSE, nrow(attck.nodes))
  attck.nodes$mass <- attck.nodes$value
  attck.nodes$description <- attck.nodes$description
  attck.nodes <- attck.nodes[, names(nodes)]

  nodes <- dplyr::bind_rows(nodes, attck.nodes)

  ### Groups
  if (verbose) print(paste0("[NET][ATTCK] extracting groups nodes ..."))
  attck.nodes <- standards$attck$attck.groups
  attck.nodes$id <- rep(NA, nrow(attck.nodes))
  attck.nodes$label <- attck.nodes$external_id
  attck.nodes$group <- rep("attck", nrow(attck.nodes))
  attck.nodes$type <- rep("group", nrow(attck.nodes))
  attck.nodes$value <- rep(1, nrow(attck.nodes))
  attck.nodes$title <- attck.nodes$name
  attck.nodes$standard <- attck.nodes$external_id
  attck.nodes$shape <- rep("circle", nrow(attck.nodes))
  attck.nodes$color <- rep("red", nrow(attck.nodes))
  attck.nodes$hidden <- rep(FALSE, nrow(attck.nodes))
  attck.nodes$mass <- attck.nodes$value
  attck.nodes$description <- attck.nodes$description
  attck.nodes <- attck.nodes[, names(nodes)]

  nodes <- dplyr::bind_rows(nodes, attck.nodes)

  ### Data Sources&Components
  if (verbose) print(paste0("[NET][ATTCK] extracting sources and components nodes ..."))
  attck.nodes <- standards$attck$attck.data_component
  attck.nodes$id <- rep(NA, nrow(attck.nodes))
  attck.nodes$label <- attck.nodes$external_id
  attck.nodes$group <- rep("attck", nrow(attck.nodes))
  attck.nodes$type <- rep("data_source", nrow(attck.nodes))
  attck.nodes$value <- rep(1, nrow(attck.nodes))
  attck.nodes$title <- attck.nodes$name
  attck.nodes$standard <- attck.nodes$external_id
  attck.nodes$shape <- rep("circle", nrow(attck.nodes))
  attck.nodes$color <- rep("blue", nrow(attck.nodes))
  attck.nodes$hidden <- rep(FALSE, nrow(attck.nodes))
  attck.nodes$mass <- attck.nodes$value
  attck.nodes$description <- attck.nodes$description
  attck.nodes <- attck.nodes[, names(nodes)]

  nodes <- dplyr::bind_rows(nodes, attck.nodes)

  ## CAPEC MITRE
  if (verbose) print(paste0("[NET][CAPEC] extracting nodes ..."))
  capec.p <- standards$capec$capec.patterns
  capec.p$type <- rep("pattern", nrow(capec.p))
  capec.c <- standards$capec$capec.categories
  capec.c$type <- rep("category", nrow(capec.c))
  capec.v <- standards$capec$capec.views
  capec.v$type <- rep("view", nrow(capec.v))
  capec.nodes <- dplyr::bind_rows(capec.p, capec.c, capec.v)
  rm(capec.p, capec.c, capec.v)
  capec.nodes$label <- capec.nodes$id
  capec.nodes$id <- rep(NA, nrow(capec.nodes))
  capec.nodes$group <- rep("capec", nrow(capec.nodes))
  capec.nodes$type <- capec.nodes$type
  capec.nodes$value <- rep(1, nrow(capec.nodes))
  capec.nodes$title <- capec.nodes$name
  capec.nodes$standard <- capec.nodes$label
  capec.nodes$shape <- rep("box", nrow(capec.nodes))
  capec.nodes$color <- rep("orange", nrow(capec.nodes))
  capec.nodes$hidden <- capec.nodes$status %in% c("Deprecated", "Obsolete")
  capec.nodes$mass <- capec.nodes$value
  capec.nodes$description <- capec.nodes$description
  capec.nodes <- capec.nodes[, names(nodes)]

  nodes <- dplyr::bind_rows(nodes, capec.nodes)

  ## SHIELD MITRE
  ### Tactics
  if (verbose) print(paste0("[NET][SHIELD] extracting tactic nodes ..."))
  shield.nodes <- standards$shield$shield.tactics
  shield.nodes$label <- shield.nodes$id
  shield.nodes$id <- rep(NA, nrow(shield.nodes))
  shield.nodes$group <- rep("shield", nrow(shield.nodes))
  shield.nodes$type <- rep("tactic", nrow(shield.nodes))
  shield.nodes$value <- rep(1, nrow(shield.nodes))
  shield.nodes$title <- shield.nodes$name
  shield.nodes$standard <- shield.nodes$label
  shield.nodes$shape <- rep("box", nrow(shield.nodes))
  shield.nodes$color <- rep("grey", nrow(shield.nodes))
  shield.nodes$hidden <- rep(FALSE, nrow(shield.nodes))
  shield.nodes$mass <- shield.nodes$value
  shield.nodes$description <- shield.nodes$description
  shield.nodes <- shield.nodes[, names(nodes)]

  nodes <- dplyr::bind_rows(nodes, shield.nodes)

  ### Techniques
  if (verbose) print(paste0("[NET][SHIELD] extracting technique nodes ..."))
  shield.nodes <- standards$shield$shield.techniques
  shield.nodes$label <- shield.nodes$id
  shield.nodes$id <- rep(NA, nrow(shield.nodes))
  shield.nodes$group <- rep("shield", nrow(shield.nodes))
  shield.nodes$type <- rep("technique", nrow(shield.nodes))
  shield.nodes$value <- rep(1, nrow(shield.nodes))
  shield.nodes$title <- shield.nodes$name
  shield.nodes$standard <- shield.nodes$label
  shield.nodes$shape <- rep("triangle", nrow(shield.nodes))
  shield.nodes$color <- rep("grey", nrow(shield.nodes))
  shield.nodes$hidden <- rep(FALSE, nrow(shield.nodes))
  shield.nodes$mass <- shield.nodes$value
  shield.nodes$description <- shield.nodes$description
  shield.nodes <- shield.nodes[, names(nodes)]

  nodes <- dplyr::bind_rows(nodes, shield.nodes)

  ### Use Cases
  if (verbose) print(paste0("[NET][SHIELD] extracting use case nodes ..."))
  shield.nodes <- standards$shield$shield.use_cases
  shield.nodes$label <- shield.nodes$id
  shield.nodes$id <- rep(NA, nrow(shield.nodes))
  shield.nodes$group <- rep("shield", nrow(shield.nodes))
  shield.nodes$type <- rep("use_case", nrow(shield.nodes))
  shield.nodes$value <- rep(1, nrow(shield.nodes))
  shield.nodes$title <- shield.nodes$label
  shield.nodes$standard <- shield.nodes$label
  shield.nodes$shape <- rep("circle", nrow(shield.nodes))
  shield.nodes$color <- rep("grey", nrow(shield.nodes))
  shield.nodes$hidden <- rep(FALSE, nrow(shield.nodes))
  shield.nodes$mass <- shield.nodes$value
  shield.nodes$description <- shield.nodes$description
  shield.nodes <- shield.nodes[, names(nodes)]

  nodes <- dplyr::bind_rows(nodes, shield.nodes)

  ### Opportunities
  if (verbose) print(paste0("[NET][SHIELD] extracting opportunity nodes ..."))
  shield.nodes <- standards$shield$shield.opportunities
  shield.nodes$label <- shield.nodes$id
  shield.nodes$id <- rep(NA, nrow(shield.nodes))
  shield.nodes$group <- rep("shield", nrow(shield.nodes))
  shield.nodes$type <- rep("opportunity", nrow(shield.nodes))
  shield.nodes$value <- rep(1, nrow(shield.nodes))
  shield.nodes$title <- shield.nodes$label
  shield.nodes$standard <- shield.nodes$label
  shield.nodes$shape <- rep("square", nrow(shield.nodes))
  shield.nodes$color <- rep("grey", nrow(shield.nodes))
  shield.nodes$hidden <- rep(FALSE, nrow(shield.nodes))
  shield.nodes$mass <- shield.nodes$value
  shield.nodes$description <- shield.nodes$description
  shield.nodes <- shield.nodes[, names(nodes)]

  nodes <- dplyr::bind_rows(nodes, shield.nodes)

  ### Procedures
  if (verbose) print(paste0("[NET][SHIELD] extracting procedure nodes ..."))
  shield.nodes <- standards$shield$shield.procedures
  shield.nodes$label <- shield.nodes$id
  shield.nodes$id <- rep(NA, nrow(shield.nodes))
  shield.nodes$group <- rep("shield", nrow(shield.nodes))
  shield.nodes$type <- rep("procedure", nrow(shield.nodes))
  shield.nodes$value <- rep(1, nrow(shield.nodes))
  shield.nodes$title <- shield.nodes$label
  shield.nodes$standard <- shield.nodes$label
  shield.nodes$shape <- rep("square", nrow(shield.nodes))
  shield.nodes$color <- rep("grey", nrow(shield.nodes))
  shield.nodes$hidden <- rep(FALSE, nrow(shield.nodes))
  shield.nodes$mass <- shield.nodes$value
  shield.nodes$description <- shield.nodes$description
  shield.nodes <- shield.nodes[, names(nodes)]

  nodes <- dplyr::bind_rows(nodes, shield.nodes)

  ## MITRE ENGAGE
  ### Goals
  if (verbose) print(paste0("[NET][ENGAGE] extracting goals nodes ..."))
  engage.nodes <- standards$engage$engage.goals
  engage.nodes$label <- engage.nodes$.id
  engage.nodes$group <- rep("engage", nrow(engage.nodes))
  engage.nodes$type <- rep("goals", nrow(engage.nodes))
  engage.nodes$value <- rep(1, nrow(engage.nodes))
  engage.nodes$title <- engage.nodes$name
  engage.nodes$standard <- engage.nodes$label
  engage.nodes$shape <- rep("triangle", nrow(engage.nodes))
  engage.nodes$color <- rep("green", nrow(engage.nodes))
  engage.nodes$hidden <- rep(FALSE, nrow(engage.nodes))
  engage.nodes$mass <- engage.nodes$value
  engage.nodes$description <- engage.nodes$description
  engage.nodes$id <- rep(NA, nrow(engage.nodes))
  engage.nodes <- engage.nodes[, names(nodes)]

  nodes <- dplyr::bind_rows(nodes, engage.nodes)

  ### Approaches
  if (verbose) print(paste0("[NET][ENGAGE] extracting approaches nodes ..."))
  engage.nodes <- standards$engage$engage.approaches
  engage.nodes$label <- engage.nodes$.id
  engage.nodes$group <- rep("engage", nrow(engage.nodes))
  engage.nodes$type <- rep("approaches", nrow(engage.nodes))
  engage.nodes$value <- rep(1, nrow(engage.nodes))
  engage.nodes$title <- engage.nodes$name
  engage.nodes$standard <- engage.nodes$label
  engage.nodes$shape <- rep("triangle", nrow(engage.nodes))
  engage.nodes$color <- rep("green", nrow(engage.nodes))
  engage.nodes$hidden <- rep(FALSE, nrow(engage.nodes))
  engage.nodes$mass <- engage.nodes$value
  engage.nodes$description <- engage.nodes$description
  engage.nodes$id <- rep(NA, nrow(engage.nodes))
  engage.nodes <- engage.nodes[, names(nodes)]

  nodes <- dplyr::bind_rows(nodes, engage.nodes)

  ### Activities
  if (verbose) print(paste0("[NET][ENGAGE] extracting activities nodes ..."))
  engage.nodes <- standards$engage$engage.activities
  engage.nodes$label <- engage.nodes$.id
  engage.nodes$group <- rep("engage", nrow(engage.nodes))
  engage.nodes$type <- rep("activities", nrow(engage.nodes))
  engage.nodes$value <- rep(1, nrow(engage.nodes))
  engage.nodes$title <- engage.nodes$name
  engage.nodes$standard <- engage.nodes$label
  engage.nodes$shape <- rep("triangle", nrow(engage.nodes))
  engage.nodes$color <- rep("green", nrow(engage.nodes))
  engage.nodes$hidden <- rep(FALSE, nrow(engage.nodes))
  engage.nodes$mass <- engage.nodes$value
  engage.nodes$description <- engage.nodes$description
  engage.nodes$id <- rep(NA, nrow(engage.nodes))
  engage.nodes <- engage.nodes[, names(nodes)]

  nodes <- dplyr::bind_rows(nodes, engage.nodes)

  ### Adversary vulnerabilities
  if (verbose) print(paste0("[NET][ENGAGE] extracting adversary vulnerabilities nodes ..."))
  engage.nodes <- standards$engage$engage.av
  engage.nodes$label <- engage.nodes$eav_id
  engage.nodes$group <- rep("engage", nrow(engage.nodes))
  engage.nodes$type <- rep("adversary_vulnerability", nrow(engage.nodes))
  engage.nodes$value <- rep(1, nrow(engage.nodes))
  engage.nodes$title <- engage.nodes$eav_id
  engage.nodes$standard <- engage.nodes$label
  engage.nodes$shape <- rep("star", nrow(engage.nodes))
  engage.nodes$color <- rep("purple", nrow(engage.nodes))
  engage.nodes$hidden <- rep(FALSE, nrow(engage.nodes))
  engage.nodes$mass <- engage.nodes$value
  engage.nodes$description <- engage.nodes$eav
  engage.nodes$id <- rep(NA, nrow(engage.nodes))
  engage.nodes <- engage.nodes[, names(nodes)]

  nodes <- dplyr::bind_rows(nodes, engage.nodes)

  ## CAR MITRE
  ### Analytics
  if (verbose) print(paste0("[NET][CAR] extracting analytic nodes ..."))
  car.nodes <- standards$car$car.analytics
  car.nodes$label <- car.nodes$id
  car.nodes$group <- rep("car", nrow(car.nodes))
  car.nodes$type <- rep("analytic", nrow(car.nodes))
  car.nodes$value <- rep(1, nrow(car.nodes))
  car.nodes$title <- car.nodes$title
  car.nodes$standard <- car.nodes$label
  car.nodes$shape <- rep("triangle", nrow(car.nodes))
  car.nodes$color <- rep("green", nrow(car.nodes))
  car.nodes$hidden <- rep(FALSE, nrow(car.nodes))
  car.nodes$mass <- car.nodes$value
  car.nodes$description <- car.nodes$description
  car.nodes$id <- rep(NA, nrow(car.nodes))
  car.nodes <- car.nodes[, names(nodes)]

  nodes <- dplyr::bind_rows(nodes, car.nodes)

  ### Data Model
  if (verbose) print(paste0("[NET][CAR] extracting data model nodes ..."))
  car.nodes <- standards$car$car.model
  car.nodes$label <- car.nodes$model.id
  car.nodes$group <- rep("car", nrow(car.nodes))
  car.nodes$type <- rep("data_model", nrow(car.nodes))
  car.nodes$value <- rep(1, nrow(car.nodes))
  car.nodes$title <- car.nodes$label
  car.nodes$standard <- car.nodes$label
  car.nodes$shape <- rep("circle", nrow(car.nodes))
  car.nodes$color <- rep("green", nrow(car.nodes))
  car.nodes$hidden <- rep(FALSE, nrow(car.nodes))
  car.nodes$mass <- car.nodes$value
  car.nodes <- dplyr::mutate(car.nodes,
                             description = paste(.data$description, .data$action.description,
                                                 "Example: ", .data$field.example, sep = "\n\n"))
  car.nodes$id <- rep(NA, nrow(car.nodes))
  car.nodes <- car.nodes[, names(nodes)]

  nodes <- dplyr::bind_rows(nodes, car.nodes)

  ### Sensors
  if (verbose) print(paste0("[NET][CAR] extracting sensor nodes ..."))
  car.nodes <- standards$car$car.sensors
  car.nodes <- dplyr::mutate(car.nodes, label = paste(.data$sensor_name, .data$sensor_version, sep = "_"))
  car.nodes$group <- rep("car", nrow(car.nodes))
  car.nodes$type <- rep("sensor", nrow(car.nodes))
  car.nodes$value <- rep(1, nrow(car.nodes))
  car.nodes$title <- car.nodes$label
  car.nodes$standard <- car.nodes$label
  car.nodes$shape <- rep("circle", nrow(car.nodes))
  car.nodes$color <- rep("green", nrow(car.nodes))
  car.nodes$hidden <- rep(FALSE, nrow(car.nodes))
  car.nodes$mass <- car.nodes$value
  car.nodes$description = car.nodes$sensor_description
  car.nodes$id <- rep(NA, nrow(car.nodes))
  car.nodes <- car.nodes[, names(nodes)]

  nodes <- dplyr::bind_rows(nodes, car.nodes)
  nodes$id <- 1:nrow(nodes)

  nodes$standard <- gsub('"',"",nodes$standard)
  nodes$label <- gsub('"',"",nodes$label)
  nodes$group <- gsub('-',"_",nodes$group)
  nodes$type <- gsub('-',"_",nodes$type)

  return(nodes)
}

#' Extract relationships between standards as edges in a data frame.
#'
#' \code{from} : node id of edge start
#' \code{to} : node id of edge end
#' \code{from_std} : standard id of edge start
#' \code{to_std} : standard id of edge end
#' \code{value} : When a value is set, the nodes will be scaled using the options in the scaling object defined above.
#' \code{title} : The title is shown in a pop-up when the mouse moves over the edge.
#' \code{arrows} : To draw an arrow with default settings a string can be supplied. For example: 'to, from,middle' or 'to;from', any combination with any separating symbol is fine. If you want to control the size of the arrowheads, you can supply an object.
#' \code{dashes} : When true, the edge will be drawn as a dashed line.
#' \code{color} : Color for the node.
#' \code{hidden} : When true, the node will not be shown. It will still be part of the physics simulation though!
#'
#' @param standards list of data.frames
#' @param verbose logical, FALSE by default. Change it to see the process messages.
#'
#' @return data.frame
build_edges <- function(standards = standards, verbose = FALSE) {
  edges <- newEdge()

  ### CPE -> CVE
  if (verbose) print(paste0("[NET] Adding relationships CPE -> CVE ..."))
  cpe.edges <- lapply(standards$cpe$cpe.nist$refs, function(x) stringr::str_extract_all(x, "CVE-\\d+-\\d+"))
  cpe.edges <- sapply(cpe.edges, function(x) ifelse(identical(x[[1]], character(0)), NA, x[[1]]))
  cpe.edges <- data.frame(from_std = standards$cpe$cpe.nist$cpe.23, to_std = cpe.edges, stringsAsFactors = FALSE)
  cpe.edges <- cpe.edges[stats::complete.cases(cpe.edges), ]
  cpe.edges$to_std <- as.character(cpe.edges$to_std)
  cpe.edges$to_std <- stringr::str_replace_all(cpe.edges$to_std,"'","")
  cpe.edges$from <- as.character(rep(NA, nrow(cpe.edges)))
  cpe.edges$to <- as.character(rep(NA, nrow(cpe.edges)))
  cpe.edges$title <- rep("is_vulnerable", nrow(cpe.edges))
  cpe.edges$value <- rep(1, nrow(cpe.edges))
  cpe.edges$label <- rep("is_vulnerable", nrow(cpe.edges))
  cpe.edges$arrows <- rep("to", nrow(cpe.edges))
  cpe.edges$dashes <- rep(FALSE, nrow(cpe.edges))
  cpe.edges$hidden <- rep(FALSE, nrow(cpe.edges))
  cpe.edges$color <- rep("red", nrow(cpe.edges))

  cpe.edges <- cpe.edges[, names(edges)]
  edges <- dplyr::bind_rows(edges, cpe.edges)

  ### CVE -> CWE
  if (verbose) print(paste0("[NET] Adding relationships CVE -> CWE ..."))
  cve.edges <- dplyr::select(standards$cve$cve.nist, c("cve.id", "problem.type"))
  cve.edges[cve.edges$problem.type == "{}", "problem.type"] <- "[\"NVD-CWE-noinfo\"]"
  cve.edges$problem.type <- lapply(cve.edges$problem.type, jsonlite::fromJSON)
  cve.edges <- tidyr::unnest(cve.edges, cols = c("problem.type"))
  names(cve.edges) <- c("from_std", "to_std")
  cve.edges$from <- as.character(rep(NA, nrow(cve.edges)))
  cve.edges$to <- as.character(rep(NA, nrow(cve.edges)))
  cve.edges$title <- rep("takes_advantage_of", nrow(cve.edges))
  cve.edges$value <- rep(1, nrow(cve.edges))
  cve.edges$label <- rep("problem_type", nrow(cve.edges))
  cve.edges$arrows <- rep("to", nrow(cve.edges))
  cve.edges$dashes <- rep(FALSE, nrow(cve.edges))
  cve.edges$hidden <- rep(FALSE, nrow(cve.edges))
  cve.edges$color <- rep("orange", nrow(cve.edges))

  cve.edges <- cve.edges[-which(cve.edges$to_std == "NVD-CWE-noinfo"), ]
  cve.edges <- cve.edges[-which(cve.edges$to_std == "NVD-CWE-Other"), ]

  cve.edges <- cve.edges[, names(edges)]
  edges <- dplyr::bind_rows(edges, cve.edges)

  ### CVE -> CPE
  if (verbose) print(paste0("[NET] Adding relationships CVE -> CPE ..."))
  cve.edges <- dplyr::select(standards$cve$cve.nist, c("cve.id", "vulnerable.configuration"))
  cpematch <- lapply(cve.edges$vulnerable.configuration,
                     function(x)
                       unique(jsonlite::fromJSON(x)$cpe_match[[1]]$cpe23Uri))
  cpematch <- unlist(lapply(cpematch, function(x) ifelse(is.null(x), NA, x)))
  cpechild <- lapply(cve.edges$vulnerable.configuration,
                     function(x)
                       unique(unlist(sapply(jsonlite::fromJSON(x)$children,
                                            function(y)
                                              unlist(sapply(y$cpe_match,
                                                            function(z) z$cpe23Uri))))))
  cpechild <- unlist(lapply(cpechild, function(x) ifelse(is.null(x), NA, x)))
  tocpes <- data.frame(cpematch = cpematch,
                     cpechild = cpechild,
                     stringsAsFactors = FALSE)
  cve.edges$to_std <- apply(tocpes, 1,
                       function(x)
                         as.character(stats::na.exclude(unique(c(x[["cpematch"]], x[["cpechild"]])))))
  cve.edges <- tidyr::unnest(cve.edges, cols = c("to_std"))
  cve.edges$vulnerable.configuration <- NULL
  cve.edges$from_std <- cve.edges$cve.id
  cve.edges$from <- as.character(rep(NA, nrow(cve.edges)))
  cve.edges$to <- as.character(rep(NA, nrow(cve.edges)))
  cve.edges$title <- rep("vulnerable_configuration", nrow(cve.edges))
  cve.edges$value <- rep(1, nrow(cve.edges))
  cve.edges$label <- rep("is_vulnerable", nrow(cve.edges))
  cve.edges$arrows <- rep("to", nrow(cve.edges))
  cve.edges$dashes <- rep(FALSE, nrow(cve.edges))
  cve.edges$hidden <- rep(FALSE, nrow(cve.edges))
  cve.edges$color <- rep("red", nrow(cve.edges))
  rm(cpematch, cpechild, tocpes)

  cve.edges <- cve.edges[, names(edges)]
  edges <- dplyr::bind_rows(edges, cve.edges)

  ### CWE -> CVE
  if (verbose) print(paste0("[NET] Adding relationships CWE -> CVE ..."))
  cwe.edges <- lapply(standards$cwe$cwe.weaknesses$Observed_Examples,
                      function(x) {
                        cves <- stringr::str_extract_all(x, "CVE-\\d+-\\d+")[[1]]
                        data.frame(to_std = cves, stringsAsFactors = FALSE)
                      })
  names(cwe.edges) <- standards$cwe$cwe.weaknesses$Code_Standard
  cwe.edges <- plyr::ldply(cwe.edges, rbind)
  names(cwe.edges) <- c("from_std", "to_std")
  cwe.edges <- cwe.edges[stats::complete.cases(cwe.edges), ]
  cwe.edges$from <- as.character(rep(NA, nrow(cwe.edges)))
  cwe.edges$to <- as.character(rep(NA, nrow(cwe.edges)))
  cwe.edges$title <- rep("vulnerability_example", nrow(cwe.edges))
  cwe.edges$value <- rep(1, nrow(cwe.edges))
  cwe.edges$label <- rep("example", nrow(cwe.edges))
  cwe.edges$arrows <- rep("to", nrow(cwe.edges))
  cwe.edges$dashes <- rep(FALSE, nrow(cwe.edges))
  cwe.edges$hidden <- rep(FALSE, nrow(cwe.edges))
  cwe.edges$color <- rep("orange", nrow(cwe.edges))

  cwe.edges <- cwe.edges[, names(edges)]
  edges <- dplyr::bind_rows(edges, cwe.edges)

  ### CWE -> CAPEC
  if (verbose) print(paste0("[NET] Adding relationships CWE -> CAPEC ..."))
  cwe.edges <- standards$cwe$cwe.weaknesses[, c("Code_Standard", "Related_Attack_Patterns")]
  cwe.edges <- cwe.edges[stats::complete.cases(cwe.edges), ]
  cwe2capec <- lapply(cwe.edges$Related_Attack_Patterns,
                      function(x)
                        data.frame(to_std = paste0("CAPEC-", RJSONIO::fromJSON(x)),
                                   stringsAsFactors = FALSE))
  names(cwe2capec) <- cwe.edges$Code_Standard
  cwe.edges <- dplyr::bind_rows(cwe2capec, .id = "from_std")
  cwe.edges <- cwe.edges[stats::complete.cases(cwe.edges), ]
  cwe.edges$from <- as.character(rep(NA, nrow(cwe.edges)))
  cwe.edges$to <- as.character(rep(NA, nrow(cwe.edges)))
  cwe.edges$title <- rep("leverage_attack", nrow(cwe.edges))
  cwe.edges$value <- rep(1, nrow(cwe.edges))
  cwe.edges$label <- rep("leverage", nrow(cwe.edges))
  cwe.edges$arrows <- rep("to", nrow(cwe.edges))
  cwe.edges$dashes <- rep(FALSE, nrow(cwe.edges))
  cwe.edges$hidden <- rep(FALSE, nrow(cwe.edges))
  cwe.edges$color <- rep("red", nrow(cwe.edges))

  cwe.edges <- cwe.edges[, names(edges)]
  edges <- dplyr::bind_rows(edges, cwe.edges)

  ### CWE -> CWE
  if (verbose) print(paste0("[NET] Adding relationships CWE -> CWE ..."))
  cwe.edges <- dplyr::bind_rows(standards$cwe$cwe.views[, c("Code_Standard", "Related_Weakness")],
                                standards$cwe$cwe.categories[, c("Code_Standard", "Related_Weakness")],
                                standards$cwe$cwe.weaknesses[, c("Code_Standard", "Related_Weakness")])
  cwe.edges <- cwe.edges[stats::complete.cases(cwe.edges), ]
  cwe2cwe <- lapply(cwe.edges$Related_Weakness,
                    function(x) {
                      k <- RJSONIO::fromJSON(x)
                      if (length(k) == 1) {
                        k <- as.data.frame.array(t(k[[1]]))
                      } else {
                        k <- dplyr::bind_rows(lapply(k, function(x) as.data.frame(t(x))))
                        # names(k) <- c("nature", "cwe_id", "view_id", "ordinal")
                      }
                      k
                    })
  names(cwe2cwe) <- cwe.edges$Code_Standard
  cwe.edges <- dplyr::bind_rows(cwe2cwe, .id = "from_std")
  cwe.edges$to_std <- paste0("CWE-", cwe.edges$cwe_id)
  cwe.edges$from <- as.character(rep(NA, nrow(cwe.edges)))
  cwe.edges$to <- as.character(rep(NA, nrow(cwe.edges)))
  cwe.edges$nature[is.na(cwe.edges$nature)] <- "include"
  cwe.edges$title <- cwe.edges$nature
  cwe.edges$value <- rep(1, nrow(cwe.edges))
  cwe.edges$label <- rep("include", nrow(cwe.edges))
  cwe.edges$arrows <- rep("to", nrow(cwe.edges))
  cwe.edges$dashes <- rep(FALSE, nrow(cwe.edges))
  cwe.edges$hidden <- rep(FALSE, nrow(cwe.edges))
  cwe.edges$color <- rep("blue", nrow(cwe.edges))

  cwe.edges <- cwe.edges[, names(edges)]
  edges <- dplyr::bind_rows(edges, cwe.edges)
  rm(cwe2capec, cwe2cwe)

  ### CAPEC multiple relations
  if (verbose) print(paste0("[NET] Adding relationships CAPEC -> ANY ..."))
  capec.edges <- standards$capec$capec.relations
  # XXX: Workaround for empty relations to CWEs
  capec.edges <- capec.edges[!grepl(pattern = "^CWE-$", x = capec.edges$to), ]
  names(capec.edges) <- c("from_std", "label", "to_std", "title")
  capec.edges$from <- as.character(rep(NA, nrow(capec.edges)))
  capec.edges$to <- as.character(rep(NA, nrow(capec.edges)))
  capec.edges$value <- rep(1, nrow(capec.edges))
  capec.edges$arrows <- rep("to", nrow(capec.edges))
  capec.edges$dashes <- rep(FALSE, nrow(capec.edges))
  capec.edges$hidden <- rep(FALSE, nrow(capec.edges))
  capec.edges$color <- rep("orange", nrow(capec.edges))

  capec.edges <- capec.edges[, names(edges)]
  edges <- dplyr::bind_rows(edges, capec.edges)

  ### ATTCK multiple relations
  if (verbose) print(paste0("[NET] Adding relationships ATTCK -> ANY ..."))
  attck.edges <- standards$attck$attck.relations
  attck.edges <- attck.edges[, c("from", "to", "description", "relationship_type")]
  names(attck.edges) <- c("from_std", "to_std", "title", "label")
  attck.edges$from <- as.character(rep(NA, nrow(attck.edges)))
  attck.edges$to <- as.character(rep(NA, nrow(attck.edges)))
  attck.edges$value <- rep(1, nrow(attck.edges))
  attck.edges$arrows <- rep("to", nrow(attck.edges))
  attck.edges$dashes <- rep(FALSE, nrow(attck.edges))
  attck.edges$hidden <- rep(FALSE, nrow(attck.edges))
  attck.edges$color <- rep("red", nrow(attck.edges))

  attck.edges <- attck.edges[, names(edges)]
  edges <- dplyr::bind_rows(edges, attck.edges)

  if (verbose) print(paste0("[NET] Adding relationships ATTCK -> Data component ..."))
  attck.edges <- standards$attck$attck.data_relations
  attck.edges$title <- attck.edges$label
  attck.edges$from <- as.character(rep(NA, nrow(attck.edges)))
  attck.edges$to <- as.character(rep(NA, nrow(attck.edges)))
  attck.edges$value <- rep(1, nrow(attck.edges))
  attck.edges$arrows <- rep("to", nrow(attck.edges))
  attck.edges$dashes <- rep(FALSE, nrow(attck.edges))
  attck.edges$hidden <- rep(FALSE, nrow(attck.edges))
  attck.edges$color <- rep("red", nrow(attck.edges))

  attck.edges <- attck.edges[, names(edges)]
  edges <- dplyr::bind_rows(edges, attck.edges)

  ### SHIELD multiple relations
  if (verbose) print(paste0("[NET] Adding relationships SHIELD -> ANY ..."))
  shield.edges <- standards$shield$shield.relations
  names(shield.edges) <- c("from_std", "to_std", "label")
  shield.edges$title <- shield.edges$label
  shield.edges$from <- as.character(rep(NA, nrow(shield.edges)))
  shield.edges$to <- as.character(rep(NA, nrow(shield.edges)))
  shield.edges$value <- rep(1, nrow(shield.edges))
  shield.edges$arrows <- rep("to", nrow(shield.edges))
  shield.edges$dashes <- rep(FALSE, nrow(shield.edges))
  shield.edges$hidden <- rep(FALSE, nrow(shield.edges))
  shield.edges$color <- rep("blue", nrow(shield.edges))

  shield.edges <- shield.edges[, names(edges)]
  edges <- dplyr::bind_rows(edges, shield.edges)

  ### ENGAGE multiple relations
  if (verbose) print(paste0("[NET] Adding relationships ENGAGE -> ANY ..."))
  engage.edges <- standards$engage$engage.relations
  names(engage.edges) <- c("from_std", "to_std", "from_type", "to_type", "label")
  engage.edges$title <- engage.edges$label
  engage.edges$from <- as.character(rep(NA, nrow(engage.edges)))
  engage.edges$to <- as.character(rep(NA, nrow(engage.edges)))
  engage.edges$value <- rep(1, nrow(engage.edges))
  engage.edges$arrows <- rep("to", nrow(engage.edges))
  engage.edges$dashes <- rep(FALSE, nrow(engage.edges))
  engage.edges$hidden <- rep(FALSE, nrow(engage.edges))
  engage.edges$color <- rep("blue", nrow(engage.edges))

  engage.edges <- engage.edges[, names(edges)]
  edges <- dplyr::bind_rows(edges, engage.edges)

  ### CAR multiple relations
  #### CAR -> ATTCK
  if (verbose) print(paste0("[NET] Adding relationships CAR -> ATTCK ..."))
  car.edges <- standards$car$car.coverage
  names(car.edges) <- c("from_std", "to_std", "title", "value")
  car.edges$label <- rep("cover", nrow(car.edges))
  car.edges$from <- as.character(rep(NA, nrow(car.edges)))
  car.edges$to <- as.character(rep(NA, nrow(car.edges)))
  car.edges$arrows <- rep("to", nrow(car.edges))
  car.edges$dashes <- rep(FALSE, nrow(car.edges))
  car.edges$hidden <- rep(FALSE, nrow(car.edges))
  car.edges$color <- rep("blue", nrow(car.edges))

  car.edges <- car.edges[, names(edges)]
  edges <- dplyr::bind_rows(edges, car.edges)

  #### CAR -> CAR Data Model
  if (verbose) print(paste0("[NET] Adding relationships CAR -> CAR ..."))
  #### CAR Data Model -> CAR Sensor
  car.edges <- standards$car$car.relations
  names(car.edges) <- c("from_std", "to_std")
  car.edges$label <- rep("implement", nrow(car.edges))
  car.edges$title <- car.edges$label
  car.edges$from <- as.character(rep(NA, nrow(car.edges)))
  car.edges$to <- as.character(rep(NA, nrow(car.edges)))
  car.edges$value <- rep(1, nrow(car.edges))
  car.edges$arrows <- rep("to", nrow(car.edges))
  car.edges$dashes <- rep(FALSE, nrow(car.edges))
  car.edges$hidden <- rep(FALSE, nrow(car.edges))
  car.edges$color <- rep("blue", nrow(car.edges))

  car.edges <- car.edges[, names(edges)]
  edges <- dplyr::bind_rows(edges, car.edges)

  edges$label <- gsub(pattern = "(\\s|-)", "_", edges$label)
  edges$title <- gsub(pattern = "(\\s|-)", "_", edges$title)
  edges$title <- stringr::str_replace_all(edges$title,"'","")
  edges$title <- stringr::str_replace_all(edges$title,'"',"")
  edges$title[is.na(edges$title)] <- "unknown"

  return(edges)
}

#' Create an empty node
#'
#' \code{id} : The id of the node unique value for all standard elements.
#' \code{label} : The label is the piece of text shown in or under the node, depending on the shape.
#' \code{group} : When not undefined, the group of node(s)
#' \code{type} : Used as subgroup to classify different object from
#' \code{value} : When a value is set, the nodes will be scaled using the options in the scaling object defined above.
#' \code{title} : Title to be displayed when the user hovers over the node. The title can be an HTML element or a string containing plain text or HTML.
#' \code{standard} : The id of the standard
#' \code{shape} : The shape defines what the node looks like. The types with the label inside of it are: ellipse, circle, database, box, text. The ones with the label outside of it are: image, circularImage, diamond, dot, star, triangle, triangleDown, square and icon.
#' \code{color} : Color for the node.
#' \code{hidden} : When true, the node will not be shown. It will still be part of the physics simulation though!
#' \code{mass} : Default to 1. The "barnesHut" physics model (which is enabled by default) is based on an inverted gravity model. By increasing the mass of a node, you increase it's repulsion. Values lower than 1 are not recommended.
#' \code{description} : Description could include extra information or nested data which include other columns from original data frame observation.
#'
#' @return data.frame
newNode <- function() {
  node <- data.frame(
    id = character(),
    label = character(),
    group = character(),
    type = character(),
    value = numeric(),
    title = character(),
    standard = character(),
    shape = character(),
    color = character(),
    hidden = logical(),
    mass = numeric(),
    description = character(),
    stringsAsFactors = FALSE
  )

  return(node)
}


#' Create an empty node
#'
#' \code{from} : node id of edge start
#' \code{to} : node id of edge end
#' \code{from_std} : standard id of edge start
#' \code{to_std} : standard id of edge end
#' \code{title} : The title is shown in a pop-up when the mouse moves over the edge.
#' \code{value} : When a value is set, the nodes will be scaled using the options in the scaling object defined above.
#' \code{label} : The label of the edge. HTML does not work in here because the network uses HTML5 Canvas.
#' \code{arrows} : To draw an arrow with default settings a string can be supplied. For example: 'to, from,middle' or 'to;from', any combination with any separating symbol is fine. If you want to control the size of the arrowheads, you can supply an object.
#' \code{dashes} : When true, the edge will be drawn as a dashed line.
#' \code{hidden} : When true, the node will not be shown. It will still be part of the physics simulation though!
#' \code{color} : Color for the node.
#' \code{hidden} : When true, the node will not be shown. It will still be part of the physics simulation though!
#'
#' @return data.frame
newEdge <- function() {
  edge <- data.frame(
    from = character(),
    to = character(),
    from_std = character(),
    to_std = character(),
    title = character(),
    value = numeric(),
    label = character(),
    arrows = character(),
    dashes = logical(),
    hidden = logical(),
    color = character(),
    stringsAsFactors = FALSE
  )

  return(edge)
}
