#' MITRE Shield Tactics data frame
#'
#' @return data.frame
#' @export
getShieldTactics <- function() {
  tactics_url <- "https://raw.githubusercontent.com/MITRECND/mitrecnd.github.io/master/_data/tactics.json"
  tactics <- jsonlite::fromJSON(tactics_url)

  return(tactics)
}

#' MITRE Shield Techniques data frame
#'
#' @return data.frame
#' @export
getShieldTechniques <- function() {
  tech_url <- "https://raw.githubusercontent.com/MITRECND/mitrecnd.github.io/master/_data/techniques.json"
  tech <- jsonlite::fromJSON(tech_url)

  return(tech)
}

#' MITRE Shield Opportunities data frame
#'
#' @return data.frame
#' @export
getShieldOpportunities <- function() {
  opport_url <- "https://raw.githubusercontent.com/MITRECND/mitrecnd.github.io/master/_data/opportunities.json"
  opport <- jsonlite::fromJSON(opport_url)

  return(opport)
}

#' MITRE Shield Procedures data frame
#'
#' @return data.frame
#' @export
getShieldProcedures <- function() {
  proced_url <- "https://raw.githubusercontent.com/MITRECND/mitrecnd.github.io/master/_data/procedures.json"
  proced <- jsonlite::fromJSON(proced_url)

  return(proced)
}

#' MITRE Shield Use Cases data frame
#'
#' @return data.frame
#' @export
getShieldUseCases <- function() {
  usecase_url <- "https://raw.githubusercontent.com/MITRECND/mitrecnd.github.io/master/_data/use_cases.json"
  usecase <- jsonlite::fromJSON(usecase_url)
  usecase <- usecase[,1:2]

  return(usecase)
}

#' MITRE Shield Tactics detailed data frame
#'
#' @return data.frame
getShieldTactictDetail <- function() {
  tact_det_url <- "https://raw.githubusercontent.com/MITRECND/mitrecnd.github.io/master/_data/tactic_details.json"
  tact_det <- jsonlite::fromJSON(tact_det_url)
  tact_det <- plyr::ldply(tact_det, function(x) x[["techniques"]])
  names(tact_det)[1:2] <- c("tact_id", "tech_id")

  return(tact_det)
}

#' MITRE Shield Techniques detailed data frame
#'
#' @return data.frame
getShieldTechniquesDetail <- function() {
  tech_det_url <- "https://raw.githubusercontent.com/MITRECND/mitrecnd.github.io/master/_data/technique_details.json"
  tech_det <- jsonlite::fromJSON(tech_det_url)

  return(tech_det)
}

#' MITRE Shield objects relations data frame
#'
#' @return data.frame
getShieldRelations <- function() {
  tact_det <- getShieldTactictDetail()
  tech_det <- getShieldTechniquesDetail()

  ## SHIELD RELATIONS
  relations <- data.frame(from = character(0),
                          to = character(0),
                          team = character(0),
                          stringsAsFactors = FALSE)

  ## Shield Tactic --> Shield Technique
  df <- tact_det[, c("tact_id", "tech_id")]
  df$from <- df$tact_id
  df$to <- df$tech_id
  df$tact_id <- NULL
  df$tech_id <- NULL
  df$team <- rep("BLUE", nrow(df))
  relations <- dplyr::bind_rows(relations, df)

  ## Shield Technique --> ATT&CK Tactic
  df <- plyr::ldply(tech_det,
                    function(x) {
                      d = data.frame(to = x[["attack_tactics"]][["id"]])
                      d$team = rep("BLUE", nrow(d))
                      d
                    })
  df$from <- df$.id
  df$.id <- NULL
  relations <- dplyr::bind_rows(relations, df)

  ## Shield Technique --> ATT&CK Techniques
  df <- plyr::ldply(tech_det,
                    function(x) {
                      d = data.frame(to = x[["attack_techniques"]][["id"]])
                      d$team = rep("BLUE", nrow(d))
                      d
                    })
  df$from <- df$.id
  df$.id <- NULL
  relations <- dplyr::bind_rows(relations, df)

  ## Shield Technique --> Use Cases
  df <- plyr::ldply(tech_det,
                    function(x) {
                      d = data.frame(to = x[["use_cases"]][["id"]])
                      d$team = rep("BLUE", nrow(d))
                      d
                    })
  df$from <- df$.id
  df$.id <- NULL
  relations <- dplyr::bind_rows(relations, df)

  ## Shield Technique --> Opportunities
  df <- plyr::ldply(tech_det,
                    function(x) {
                      d = data.frame(to = x[["opportunities"]][["id"]])
                      d$team = rep("BLUE", nrow(d))
                      d
                    })
  df$from <- df$.id
  df$.id <- NULL
  relations <- dplyr::bind_rows(relations, df)

  ## Shield Technique --> Procedures
  df <- plyr::ldply(tech_det,
                    function(x) {
                      d = data.frame(to = x[["procedures"]][["id"]])
                      d$team = rep("BLUE", nrow(d))
                      d
                    })
  df$from <- df$.id
  df$.id <- NULL
  relations <- dplyr::bind_rows(relations, df)

  return(relations)
}

#' MITRE Shield as visNetwork
#'
#' @return data.frame
#' @export
getShieldNetwork <- function() {
  # MITRE Shield Network as igraph
  relations <- getShieldRelations()

  ## NODES
  # Ref: https://datastorm-open.github.io/visNetwork/nodes.html
  nodes <- data.frame(id = character(0),
                      label = character(0),
                      group = character(0),
                      value = numeric(0),
                      shape = character(0),
                      title = character(0),
                      color = character(0),
                      shadow = logical(0))
  shield_nodes <- nodes

  ### Tactics nodes
  df <- getShieldTactics()
  df$label <- df$id
  df$group <- rep("tactic", nrow(df))
  df$value <- rep(5, nrow(df))
  df$shape <- rep("triangle", nrow(df))
  df$title <- paste0("<p><b>", df$name,"</b><br>", df$description,"</p>")
  df$color <- rep("darkred", nrow(df))
  df$description <- NULL
  df$name <- NULL
  df$long_description <- NULL

  shield_nodes <- rbind(shield_nodes, df)

  ### Techniques nodes
  df <- getShieldTechniques()
  df$label <- df$id
  df$group <- rep("technique", nrow(df))
  df$value <- rep(4, nrow(df))
  df$shape <- rep("square", nrow(df))
  df$title <- paste0("<p><b>", df$name,"</b><br>", df$description,"</p>")
  df$color <- rep("orange", nrow(df))
  df$description <- NULL
  df$name <- NULL
  df$long_description <- NULL

  shield_nodes <- rbind(shield_nodes, df)

  ### Opportunities nodes
  df <- getShieldOpportunities()
  df$label <- df$id
  df$group <- rep("opportunity", nrow(df))
  df$value <- rep(2, nrow(df))
  df$shape <- rep("star", nrow(df))
  df$title <- paste0("<p><b>", df$description,"</b></p>")
  df$color <- rep("grey", nrow(df))
  df$description <- NULL

  shield_nodes <- rbind(shield_nodes, df)

  ### Procedures nodes
  df <- getShieldProcedures()
  df$label <- df$id
  df$group <- rep("procedure", nrow(df))
  df$value <- rep(4, nrow(df))
  df$shape <- rep("box", nrow(df))
  df$title <- paste0("<p><b>", df$description,"</b></p>")
  df$color <- rep("purple", nrow(df))
  df$description <- NULL

  shield_nodes <- rbind(shield_nodes, df)

  ### Use Cases nodes
  df <- getShieldUseCases()
  df$label <- df$id
  df$group <- rep("usecase", nrow(df))
  df$value <- rep(4, nrow(df))
  df$shape <- rep("ellipse", nrow(df))
  df$title <- paste0("<p><b>", df$description,"</b></p>")
  df$color <- rep("yellow", nrow(df))
  df$description <- NULL

  shield_nodes <- rbind(shield_nodes, df)

  shieldnet <- visNetwork::visNetwork(shield_nodes, relations)

  return(shieldnet)
}
