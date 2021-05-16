#' Create a list of nodes and edges related to all standards in data folder.
#'
#' @return list, containing nodes and edges as data frames
#' @export
build_network <- function() {

  # NODES
  nodes <- newNode()

  ## IT SYSTEMS (MITRE & NIST)
  ### CPE
  cpe.nodes <- cpe.nist[, c("title", "cpe.23", "deprecated")]
  names(cpe.nodes) <- c("label", "title", "hidden")
  cpe.nodes$group <- rep("cpe", nrow(cpe.nodes))
  cpe.nodes$value <- rep(1, nrow(cpe.nodes))
  cpe.nodes$standard <- cpe.nodes$title
  cpe.nodes$shape <- rep("circle", nrow(cpe.nodes))
  cpe.nodes$color <- rep("blue", nrow(cpe.nodes))
  cpe.nodes$mass <- rep(1, nrow(cpe.nodes))
  cpe.nodes$description <- cpe.nodes$label

  nodes <- dplyr::bind_rows(nodes, cpe.nodes)

  ### CVE
  cve.nodes <- cve.nist[, c("cve.id", "description", "cvss3.score", "cvss2.score", "references")]
  cve.nodes$label <- cve.nodes$cve.id
  cve.nodes$group <- rep("cve", nrow(cve.nodes))

  cve.nodes$cvss3.score[is.na(cve.nodes$cvss3.score)] <- 0
  cve.nodes$cvss2.score[is.na(cve.nodes$cvss2.score)] <- 0
  cve.nodes$value <- apply(cve.nodes, 1,
                           function(x) as.numeric(ifelse(is.na(x["cvss3.score"]),
                                                         yes = ifelse(is.na(x["cvss2.score"]),
                                                                      yes = 0,
                                                                      no = x["cvss2.score"]),
                                                         no = x["cvss3.score"])))
  cve.nodes$title <- cve.nodes$cve.id
  cve.nodes$standard <- cve.nodes$cve.id
  cve.nodes$shape <- rep("triangle", nrow(cve.nodes))
  cve.nodes$color <- rep("blue", nrow(cve.nodes))
  cve.nodes$mass <- cve.nodes$value
  cve.nodes$description <- cve.nodes$references
  cve.nodes <- cve.nodes[, names(nodes)]
  cve.nodes$id <- rep(NA, nrow(cve.nodes))

  nodes <- dplyr::bind_rows(nodes, cve.nodes)

  ### CWE
  cwe.nodes <- cwe.weaknesses

  ## ATT&CK MITRE

  ## CAPEC MITRE

  ## SHIELD MITRE

  ## CAR MITRE


}



#' Create an empty node
#'
#' \code{id} : The id of the node unique value for all standard elements.
#' \code{label} : The label is the piece of text shown in or under the node, depending on the shape.
#' \code{group} : When not undefined, the group of node(s)
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
#' \code{from} : node id of begin of the edge
#' \code{to} : node id of end of the edge
#' \code{title} : The title is shown in a pop-up when the mouse moves over the edge.
#' \code{value} : When a value is set, the nodes will be scaled using the options in the scaling object defined above.
#' \code{label} : The label of the edge. HTML does not work in here because the network uses HTML5 Canvas.
#' \code{arrows} : To draw an arrow with default settings a string can be supplied. For example: 'to, from,middle' or 'to;from', any combination with any seperating symbol is fine. If you want to control the size of the arrowheads, you can supply an object.
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
