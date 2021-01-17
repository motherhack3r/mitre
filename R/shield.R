#' ETL process that read source data from \url{https://github.com/MITRECND/mitrecnd.github.io/tree/master/_data} .
#'
#' @return MITRE Shield Tactics data frame
#'   The data frame columns are: id, name, description, long_description.
#' @keywords internal
getShieldTactics <- function() {
  tactics <- jsonlite::fromJSON("data-raw/shield-tactics.json")

  return(tactics)
}

#' ETL process that read source data from \url{https://github.com/MITRECND/mitrecnd.github.io/tree/master/_data} .
#'
#' @return MITRE Shield Techniques data frame
#'   The data frame columns are: id, name, description, long_description.
#' @keywords internal
getShieldTechniques <- function() {
  tech <- jsonlite::fromJSON("data-raw/shield-techniques.json")

  return(tech)
}

#' ETL process that read source data from \url{https://github.com/MITRECND/mitrecnd.github.io/tree/master/_data} .
#'
#' @return MITRE Shield Opportunities data frame
#'   The data frame columns are: id, description.
#' @keywords internal
getShieldOpportunities <- function() {
  opport <- jsonlite::fromJSON("data-raw/shield-opportunities.json")

  return(opport)
}

#' ETL process that read source data from \url{https://github.com/MITRECND/mitrecnd.github.io/tree/master/_data} .
#'
#' @return MITRE Shield Procedures data frame
#'   The data frame columns are: id, description.
#' @keywords internal
getShieldProcedures <- function() {
  proced <- jsonlite::fromJSON("data-raw/shield-procedures.json")

  return(proced)
}

#' ETL process that read source data from \url{https://github.com/MITRECND/mitrecnd.github.io/tree/master/_data} .
#'
#' @return MITRE Shield Use Cases data frame
#'   The data frame columns are: id, description.
#' @keywords internal
getShieldUseCases <- function() {
  usecase <- jsonlite::fromJSON("data-raw/shield-use_cases.json")
  usecase <- usecase[, 1:2]

  return(usecase)
}

#' MITRE Shield Tactics detailed data frame
#'
#' @return data.frame
#' @keywords internal
getShieldTactictDetail <- function() {
  tact_det <- jsonlite::fromJSON("data-raw/shield-tactic_details.json")
  tact_det <- plyr::ldply(tact_det, function(x) x[["techniques"]])
  names(tact_det)[1:2] <- c("tact_id", "tech_id")

  return(tact_det)
}

#' MITRE Shield Techniques detailed data frame
#'
#' @return data.frame
#' @keywords internal
getShieldTechniquesDetail <- function() {
  tech_det_url <- "data-raw/shield-technique_details.json"
  tech_det <- jsonlite::fromJSON(tech_det_url)

  return(tech_det)
}

#' MITRE Shield objects relations data frame
#'
#' @return data.frame
#' @keywords internal
getShieldRelations <- function() {
  tact_det <- getShieldTactictDetail()
  tech_det <- getShieldTechniquesDetail()

  ## SHIELD RELATIONS
  relations <- data.frame(
    from = character(0),
    to = character(0),
    team = character(0),
    stringsAsFactors = FALSE
  )

  ## Shield Tactic --> Shield Technique
  df <- tact_det[, c("tact_id", "tech_id")]
  df$from <- df$tact_id
  df$to <- df$tech_id
  df$tact_id <- NULL
  df$tech_id <- NULL
  df$team <- rep("BLUE", nrow(df))
  relations <- dplyr::bind_rows(relations, df)

  ## Shield Technique --> ATT&CK Tactic
  df <- plyr::ldply(
    tech_det,
    function(x) {
      d <- data.frame(to = x[["attack_tactics"]][["id"]])
      d$team <- rep("BLUE", nrow(d))
      d
    }
  )
  df$from <- df$.id
  df$.id <- NULL
  relations <- dplyr::bind_rows(relations, df)

  ## Shield Technique --> ATT&CK Techniques
  df <- plyr::ldply(
    tech_det,
    function(x) {
      d <- data.frame(to = x[["attack_techniques"]][["id"]])
      d$team <- rep("BLUE", nrow(d))
      d
    }
  )
  df$from <- df$.id
  df$.id <- NULL
  relations <- dplyr::bind_rows(relations, df)

  ## Shield Technique --> Use Cases
  df <- plyr::ldply(
    tech_det,
    function(x) {
      d <- data.frame(to = x[["use_cases"]][["id"]])
      d$team <- rep("BLUE", nrow(d))
      d
    }
  )
  df$from <- df$.id
  df$.id <- NULL
  relations <- dplyr::bind_rows(relations, df)

  ## Shield Technique --> Opportunities
  df <- plyr::ldply(
    tech_det,
    function(x) {
      d <- data.frame(to = x[["opportunities"]][["id"]])
      d$team <- rep("BLUE", nrow(d))
      d
    }
  )
  df$from <- df$.id
  df$.id <- NULL
  relations <- dplyr::bind_rows(relations, df)

  ## Shield Technique --> Procedures
  df <- plyr::ldply(
    tech_det,
    function(x) {
      d <- data.frame(to = x[["procedures"]][["id"]])
      d$team <- rep("BLUE", nrow(d))
      d
    }
  )
  df$from <- df$.id
  df$.id <- NULL
  relations <- dplyr::bind_rows(relations, df)

  relations$label <- rep("uses", nrow(relations))
  relations$arrows <- rep("to", nrow(relations))
  relations$title <- rep("uses", nrow(relations))
  relations$dashes <- rep(FALSE, nrow(relations))

  return(relations)
}

#' MITRE Shield objects as nodes in a data frame
#'
#' @return data.frame
#' @keywords internal
getShieldNodes <- function() {
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
    shadow = logical(0),
    team = character(0)
  )
  shield_nodes <- nodes

  ### Tactics nodes
  df <- getShieldTactics()
  df$label <- df$id
  df$group <- rep("tactic", nrow(df))
  df$value <- rep(5, nrow(df))
  df$shape <- rep("triangle", nrow(df))
  df$title <- paste0("<p><b>", df$name, "</b><br>", df$description, "</p>")
  df$color <- rep("gold", nrow(df))
  df$description <- NULL
  df$name <- NULL
  df$long_description <- NULL
  df$team <- rep("BLUE", nrow(df))

  shield_nodes <- rbind(shield_nodes, df)

  ### Techniques nodes
  df <- getShieldTechniques()
  df$label <- df$id
  df$group <- rep("technique", nrow(df))
  df$value <- rep(4, nrow(df))
  df$shape <- rep("square", nrow(df))
  df$title <- paste0("<p><b>", df$name, "</b><br>", df$description, "</p>")
  df$color <- rep("orange", nrow(df))
  df$description <- NULL
  df$name <- NULL
  df$long_description <- NULL
  df$team <- rep("BLUE", nrow(df))

  shield_nodes <- rbind(shield_nodes, df)

  ### Opportunities nodes
  df <- getShieldOpportunities()
  df$label <- df$id
  df$group <- rep("opportunity", nrow(df))
  df$value <- rep(2, nrow(df))
  df$shape <- rep("star", nrow(df))
  df$title <- paste0("<p><b>", df$description, "</b></p>")
  df$color <- rep("grey", nrow(df))
  df$description <- NULL
  df$team <- rep("BLUE", nrow(df))

  shield_nodes <- rbind(shield_nodes, df)

  ### Procedures nodes
  df <- getShieldProcedures()
  df$label <- df$id
  df$group <- rep("procedure", nrow(df))
  df$value <- rep(4, nrow(df))
  df$shape <- rep("box", nrow(df))
  df$title <- paste0("<p><b>", df$description, "</b></p>")
  df$color <- rep("purple", nrow(df))
  df$description <- NULL
  df$team <- rep("BLUE", nrow(df))

  shield_nodes <- rbind(shield_nodes, df)

  ### Use Cases nodes
  df <- getShieldUseCases()
  df$label <- df$id
  df$group <- rep("usecase", nrow(df))
  df$value <- rep(4, nrow(df))
  df$shape <- rep("ellipse", nrow(df))
  df$title <- paste0("<p><b>", df$description, "</b></p>")
  df$color <- rep("yellow", nrow(df))
  df$description <- NULL
  df$team <- rep("BLUE", nrow(df))

  shield_nodes <- rbind(shield_nodes, df)
  shield_nodes$shadow <- rep(FALSE, nrow(shield_nodes))

  return(shield_nodes)
}

#' MITRE  ETL process that read source data from \url{https://github.com/MITRECND/mitrecnd.github.io/tree/master/_data} .
#' It returns a visNetwork object ready for analyze and plot.
#'
#' @return list of nodes and edges
#' @keywords internal
getShieldNetwork <- function() {
  # MITRE Shield Network as igraph
  relations <- getShieldRelations()
  shield_nodes <- getShieldNodes()

  shieldnet <- list(nodes = shield_nodes,
                    edges = relations)

  return(shieldnet)
}

#' ETL process that download current shield definitions and return a list of
#' data frames for each object. The list also contains a visNetwork object with
#' SHIELD objects as nodes and all relations as edges.
#'
#' @param verbose Default set as FALSE
#'
#' @return list of data frames
#'
#' @examples
#' \donttest{
#' shield <- mitre::getShieldData()
#' }
getShieldData <- function(verbose = FALSE) {
  if (verbose) print(paste("[*][SHIELD] Building output ..."))
  shield <- list(tactics = getShieldTactics(),
                 techniques = getShieldTechniques(),
                 opportunities = getShieldOpportunities(),
                 procedures = getShieldProcedures(),
                 usecases = getShieldUseCases(),
                 shieldnet = getShieldNetwork())
  return(shield)
}
