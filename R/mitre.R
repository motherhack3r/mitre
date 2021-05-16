#' Create a list of nodes and edges related to all standards in data folder.
#'
#' @return list, containing nodes and edges as data frames
#' @export
build_network <- function() {
  nodes <- build_nodes()
  edges <- build_edges()

  return(list(nodes = nodes, edges = edges))
}

#' Transform all standards as nodes in a data frame.
#'
#' \code{id} : The id of the node unique value for all standard elements.
#' \code{label} : The label is the piece of text shown in or under the node, depending on the shape.
#' \code{group} : When not undefined, the group of node(s)
#' \code{type} : Used as subgroup to classify diferent object from
#' \code{value} : When a value is set, the nodes will be scaled using the options in the scaling object defined above.
#' \code{title} : Title to be displayed when the user hovers over the node. The title can be an HTML element or a string containing plain text or HTML.
#' \code{standard} : The id of the standard
#' \code{shape} : The shape defines what the node looks like. The types with the label inside of it are: ellipse, circle, database, box, text. The ones with the label outside of it are: image, circularImage, diamond, dot, star, triangle, triangleDown, square and icon.
#' \code{color} : Color for the node.
#' \code{hidden} : When true, the node will not be shown. It will still be part of the physics simulation though!
#' \code{mass} : Default to 1. The barnesHut physics model (which is enabled by default) is based on an inverted gravity model. By increasing the mass of a node, you increase it's repulsion. Values lower than 1 are not recommended.
#' \code{description} : Description could include extra information or nested data which include other columns from original data frame observation.
#'
#' @return data.frame
build_nodes <- function() {
  nodes <- newNode()

  ## IT SYSTEMS (MITRE & NIST)
  ### CPE
  cpe.nodes <- cpe.nist[, c("title", "cpe.23", "deprecated")]
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

  nodes <- dplyr::bind_rows(nodes, cpe.nodes)

  ### CVE
  cve.nodes <- cve.nist[, c("cve.id", "description", "cvss3.score", "cvss2.score", "references")]
  cve.nodes$id <- rep(NA, nrow(cve.nodes))
  cve.nodes$label <- cve.nodes$cve.id
  cve.nodes$group <- rep("cve", nrow(cve.nodes))
  cve.nodes$type <- rep("cve", nrow(cve.nodes))
  cve.nodes$cvss3.score[is.na(cve.nodes$cvss3.score)] <- 0
  cve.nodes$cvss2.score[is.na(cve.nodes$cvss2.score)] <- 0
  cve.nodes$value <- as.numeric(apply(cve.nodes, 1, function(x) max(x["cvss2.score"], x["cvss3.score"])))
  cve.nodes$title <- cve.nodes$cve.id
  cve.nodes$standard <- cve.nodes$cve.id
  cve.nodes$shape <- rep("triangle", nrow(cve.nodes))
  cve.nodes$color <- rep("blue", nrow(cve.nodes))
  cve.nodes$hidden <- rep(FALSE, nrow(cve.nodes))
  cve.nodes$mass <- cve.nodes$value
  cve.nodes$description <- cve.nodes$references
  cve.nodes <- cve.nodes[, names(nodes)]

  nodes <- dplyr::bind_rows(nodes, cve.nodes)

  ### CWE
  cwe.nodes <- dplyr::bind_rows(cwe.weaknesses, cwe.categories, cwe.views)
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
  attck.nodes <- attck.tactics
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
  attck.nodes <- attck.techniques
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
  attck.nodes <- attck.mitigations
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
  attck.nodes <- attck.software
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
  attck.nodes <- attck.groups
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

  ## CAPEC MITRE
  capec.p <- capec.patterns
  capec.p$type <- rep("pattern", nrow(capec.p))
  capec.c <- capec.categories
  capec.c$type <- rep("category", nrow(capec.c))
  capec.v <- capec.views
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
  shield.nodes <- shield.tactics
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
  shield.nodes <- shield.techniques
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
  shield.nodes <- shield.use_cases
  shield.nodes$label <- shield.nodes$id
  shield.nodes$id <- rep(NA, nrow(shield.nodes))
  shield.nodes$group <- rep("shield", nrow(shield.nodes))
  shield.nodes$type <- rep("use_case", nrow(shield.nodes))
  shield.nodes$value <- rep(1, nrow(shield.nodes))
  shield.nodes$title <- shield.nodes$name
  shield.nodes$standard <- shield.nodes$label
  shield.nodes$shape <- rep("circle", nrow(shield.nodes))
  shield.nodes$color <- rep("grey", nrow(shield.nodes))
  shield.nodes$hidden <- rep(FALSE, nrow(shield.nodes))
  shield.nodes$mass <- shield.nodes$value
  shield.nodes$description <- shield.nodes$description
  shield.nodes <- shield.nodes[, names(nodes)]

  nodes <- dplyr::bind_rows(nodes, shield.nodes)

  ### Opportunities
  shield.nodes <- shield.opportunities
  shield.nodes$label <- shield.nodes$id
  shield.nodes$id <- rep(NA, nrow(shield.nodes))
  shield.nodes$group <- rep("shield", nrow(shield.nodes))
  shield.nodes$type <- rep("opportunity", nrow(shield.nodes))
  shield.nodes$value <- rep(1, nrow(shield.nodes))
  shield.nodes$title <- shield.nodes$name
  shield.nodes$standard <- shield.nodes$label
  shield.nodes$shape <- rep("square", nrow(shield.nodes))
  shield.nodes$color <- rep("grey", nrow(shield.nodes))
  shield.nodes$hidden <- rep(FALSE, nrow(shield.nodes))
  shield.nodes$mass <- shield.nodes$value
  shield.nodes$description <- shield.nodes$description
  shield.nodes <- shield.nodes[, names(nodes)]

  nodes <- dplyr::bind_rows(nodes, shield.nodes)

  ### Procedures
  shield.nodes <- shield.procedures
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

  ## CAR MITRE
  ### Analytics
  car.nodes <- car.analytics
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
  car.nodes <- car.model
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
                             description = paste(description, action.description,
                                                 "Example: ", field.example, sep = "\n\n"))
  car.nodes$id <- rep(NA, nrow(car.nodes))
  car.nodes <- car.nodes[, names(nodes)]

  nodes <- dplyr::bind_rows(nodes, car.nodes)

  ### Sensors
  car.nodes <- car.sensors
  car.nodes <- dplyr::mutate(car.nodes, label = paste(sensor_name, sensor_version, sep = "_"))
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

  return(nodes)
}

#' Extract relationships between standards as edges in a data frame.
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
build_edges <- function() {

}

#' Create an empty node
#'
#' \code{id} : The id of the node unique value for all standard elements.
#' \code{label} : The label is the piece of text shown in or under the node, depending on the shape.
#' \code{group} : When not undefined, the group of node(s)
#' \code{type} : Used as subgroup to classify diferent object from
#' \code{value} : When a value is set, the nodes will be scaled using the options in the scaling object defined above.
#' \code{title} : Title to be displayed when the user hovers over the node. The title can be an HTML element or a string containing plain text or HTML.
#' \code{standard} : The id of the standard
#' \code{shape} : The shape defines what the node looks like. The types with the label inside of it are: ellipse, circle, database, box, text. The ones with the label outside of it are: image, circularImage, diamond, dot, star, triangle, triangleDown, square and icon.
#' \code{color} : Color for the node.
#' \code{hidden} : When true, the node will not be shown. It will still be part of the physics simulation though!
#' \code{mass} : Default to 1. The barnesHut physics model (which is enabled by default) is based on an inverted gravity model. By increasing the mass of a node, you increase it's repulsion. Values lower than 1 are not recommended.
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
