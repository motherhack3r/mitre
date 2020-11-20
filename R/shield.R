#' Title
#'
#' @return
#' @export
#'
#' @examples
getShieldTactics <- function() {
  tactics_url <- "https://raw.githubusercontent.com/MITRECND/mitrecnd.github.io/master/_data/tactics.json"
  tactics <- jsonlite::fromJSON(tactics_url)

  return(tactics)
}

#' Title
#'
#' @return
#' @export
#'
#' @examples
getShieldTechniques <- function() {
  tech_url <- "https://raw.githubusercontent.com/MITRECND/mitrecnd.github.io/master/_data/techniques.json"
  tech <- jsonlite::fromJSON(tech_url)

  return(tech)
}

#' Title
#'
#' @return
#' @export
#'
#' @examples
getShieldOpportunities <- function() {
  opport_url <- "https://raw.githubusercontent.com/MITRECND/mitrecnd.github.io/master/_data/opportunities.json"
  opport <- jsonlite::fromJSON(opport_url)

  return(opport)
}

#' Title
#'
#' @return
#' @export
#'
#' @examples
getShieldProcedures <- function() {
  proced_url <- "https://raw.githubusercontent.com/MITRECND/mitrecnd.github.io/master/_data/procedures.json"
  proced <- jsonlite::fromJSON(proced_url)

  return(proced)
}

#' Title
#'
#' @return
#' @export
#'
#' @examples
getShieldUseCases <- function() {
  usecase_url <- "https://raw.githubusercontent.com/MITRECND/mitrecnd.github.io/master/_data/use_cases.json"
  usecase <- jsonlite::fromJSON(usecase_url)
  usecase <- usecase[,1:2]

  return(usecase)
}

#' Title
#'
#' @return
#'
#' @examples
getShieldTactictDetail <- function() {
  tact_det_url <- "https://raw.githubusercontent.com/MITRECND/mitrecnd.github.io/master/_data/tactic_details.json"
  tact_det <- jsonlite::fromJSON(tact_det_url)
  tact_det <- plyr::ldply(tact_det, function(x) x[["techniques"]])
  names(tact_det)[1:2] <- c("tact_id", "tech_id")

  return(tact_det)
}

#' Title
#'
#' @return
#'
#' @examples
getShieldTechniquesDetail <- function() {
  tech_det_url <- "https://raw.githubusercontent.com/MITRECND/mitrecnd.github.io/master/_data/technique_details.json"
  tech_det <- jsonlite::fromJSON(tech_det_url)

  return(tech_det)
}

#' Title
#'
#' @return
#'
#' @examples
getShieldRelations <- function() {
  tact_det <- getShieldTactictDetail()
  tech_det <- getShieldTechniquesDetail()

  ## SHIELD RELATIONS
  suppressPackageStartupMessages(library(dplyr))
  relations <- data.frame(from = character(0),
                          to = character(0),
                          team = character(0),
                          stringsAsFactors = FALSE)

  ## Shield Tactic --> Shield Technique
  # df <- tact_det[, c("tact_id", "tech_id")]
  df <- tact_det %>%
    select(tact_id, tech_id) %>%
    rename(from = tact_id, to = tech_id)
  df$team <- rep("BLUE", nrow(df))
  relations <- bind_rows(relations, df)

  ## Shield Technique --> ATT&CK Tactic
  df <- plyr::ldply(tech_det,
                    function(x) {
                      d = data.frame(to = x[["attack_tactics"]][["id"]])
                      d$team = rep("BLUE", nrow(d))
                      d
                    })
  df <- df %>% rename(from = .id)
  relations <- bind_rows(relations, df)

  ## Shield Technique --> ATT&CK Techniques
  df <- plyr::ldply(tech_det,
                    function(x) {
                      d = data.frame(to = x[["attack_techniques"]][["id"]])
                      d$team = rep("BLUE", nrow(d))
                      d
                    })
  df <- df %>% rename(from = .id)
  relations <- bind_rows(relations, df)

  ## Shield Technique --> Use Cases
  df <- plyr::ldply(tech_det,
                    function(x) {
                      d = data.frame(to = x[["use_cases"]][["id"]])
                      d$team = rep("BLUE", nrow(d))
                      d
                    })
  df <- df %>% rename(from = .id)
  relations <- bind_rows(relations, df)

  ## Shield Technique --> Opportunities
  df <- plyr::ldply(tech_det,
                    function(x) {
                      d = data.frame(to = x[["opportunities"]][["id"]])
                      d$team = rep("BLUE", nrow(d))
                      d
                    })
  df <- df %>% rename(from = .id)
  relations <- bind_rows(relations, df)

  ## Shield Technique --> Procedures
  df <- plyr::ldply(tech_det,
                    function(x) {
                      d = data.frame(to = x[["procedures"]][["id"]])
                      d$team = rep("BLUE", nrow(d))
                      d
                    })
  df <- df %>% rename(from = .id)
  relations <- bind_rows(relations, df)

  return(relations)
}

#' Title
#'
#' @return
#' @export
#'
#' @examples
getShieldNetwork <- function() {
  # MITRE Shield Network as igraph
  relations <- mitre::getShieldRelations()

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
  df <- mitre::getShieldTactics()
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
  df <- mitre::getShieldTechniques()
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
  df <- mitre::getShieldOpportunities()
  df$label <- df$id
  df$group <- rep("opportunity", nrow(df))
  df$value <- rep(2, nrow(df))
  df$shape <- rep("star", nrow(df))
  df$title <- paste0("<p><b>", df$description,"</b></p>")
  df$color <- rep("grey", nrow(df))
  df$description <- NULL

  shield_nodes <- rbind(shield_nodes, df)

  ### Procedures nodes
  df <- mitre::getShieldProcedures()
  df$label <- df$id
  df$group <- rep("procedure", nrow(df))
  df$value <- rep(4, nrow(df))
  df$shape <- rep("box", nrow(df))
  df$title <- paste0("<p><b>", df$description,"</b></p>")
  df$color <- rep("purple", nrow(df))
  df$description <- NULL

  shield_nodes <- rbind(shield_nodes, df)

  ### Use Cases nodes
  df <- mitre::getShieldUseCases()
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
