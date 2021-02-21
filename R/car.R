#' ETL process that download current CAR definitions and return a list with a
#' data frame for CAR objects. The list also contains a visNetwork object with
#' CAR objects as nodes and all relations as edges.
#'
#' @param verbose Default set as FALSE
#'
#' @return list of data frames
getCARData <- function(verbose = FALSE) {

  # https://github.com/mitre-attack/car/tree/master/analytics
  # https://github.com/mitre-attack/car/tree/master/data_model
  # https://github.com/mitre-attack/car/tree/master/sensors
  #
  # https://github.com/mitre-attack/car/blob/master/docs/data/analytics.json
  # https://github.com/mitre-attack/car/blob/master/docs/data/data_model.json
  # https://github.com/mitre-attack/car/blob/master/docs/data/sensors.json

  if (verbose) print(paste("[-][CAR] List YAML source files from mitre github repo ..."))
  req <- httr::GET("https://api.github.com/repos/mitre-attack/car/git/trees/master?recursive=1")
  httr::stop_for_status(req)
  filelist <- unlist(lapply(httr::content(req)$tree, "[", "path"), use.names = F)

  if (verbose) print(paste("[-][CAR] Reading analytics YAML files ..."))
  files.analytics <- grep("analytics/CAR.*yaml", filelist, value = TRUE, perl = TRUE)
  raw.analytics <- suppressWarnings(lapply(paste0("https://raw.githubusercontent.com/mitre-attack/car/master/",
                                                  files.analytics), yaml::read_yaml))

  if (verbose) print(paste("[-][CAR] Reading model YAML files ..."))
  files.model <- grep("data_model/.*yaml", filelist, value = TRUE, perl = TRUE)
  raw.model <- suppressWarnings(lapply(paste0("https://raw.githubusercontent.com/mitre-attack/car/master/",
                                              files.model), yaml::read_yaml))

  if (verbose) print(paste("[-][CAR] Reading sensors YAML files ..."))
  files.sensors <- grep("^sensors/.*yaml$", filelist, value = TRUE, perl = TRUE)
  raw.sensors <- suppressWarnings(lapply(paste0("https://raw.githubusercontent.com/mitre-attack/car/master/",
                                                files.sensors), yaml::read_yaml))

  # Analytics
  if (verbose) print(paste("[-][CAR] Extracting Analytics coverage relationships ..."))
  coverage.a <- lapply(raw.analytics, function(x) x[["coverage"]])
  if (verbose) print(paste("[-][CAR] Parsing Analytics data ..."))
  analytics <- lapply(raw.analytics,
                      function(x) {
                        x[["implementations"]] <- jsonlite::toJSON(x[["implementations"]])
                        x[["platforms"]] <- jsonlite::toJSON(x[["platforms"]])
                        x[["subtypes"]] <- jsonlite::toJSON(x[["subtypes"]])
                        x[["contributors"]] <- jsonlite::toJSON(x[["contributors"]])
                        x[["analytic_types"]] <- jsonlite::toJSON(x[["analytic_types"]])
                        x[["unit_tests"]] <- jsonlite::toJSON(x[["unit_tests"]])
                        x[["coverage"]] <- NULL
                        x[["references"]] <- jsonlite::toJSON(x[["references"]])
                        x[["true_positives"]] <- jsonlite::toJSON(x[["true_positives"]])
                        x[["data_model_references"]] <- jsonlite::toJSON(x[["data_model_references"]])
                        x
                      })
  analytics <- dplyr::bind_rows(analytics)
  analytics$Coverage <- NULL

  if (verbose) print(paste("[-][CAR] Analytics platform expanse ..."))
  analytics$platform_windows <- grepl("Windows", analytics$platforms)
  analytics$platform_linux <- grepl("Linux", analytics$platforms)
  analytics$platform_macos <- grepl("macOS", analytics$platforms)
  analytics$platforms <- NULL

  if (verbose) print(paste("[-][CAR] Analytics type expanse ..."))
  analytics$type_ttp <- grepl("TTP", analytics$analytic_types)
  analytics$type_detection <- grepl("Detection", analytics$analytic_types)
  analytics$type_anomaly <- grepl("Anomaly", analytics$analytic_types)
  analytics$type_awareness <- grepl("Situational Awareness", analytics$analytic_types)
  analytics$analytic_types <- NULL
  names(coverage.a) <- analytics$id

  # CAR -> ATTCK Techniques
  if (verbose) print(paste("[-][CAR] Looking for relations between CAR and ATT&CK Techniques..."))
  tech <- lapply(coverage.a,
                 function(x) {
                   data.frame(to = unlist(sapply(x, function(k) k[["technique"]])), stringsAsFactors = F)
                 })
  car2tech <- dplyr::bind_rows(tech, .id = "from")

  # CAR -> ATTCK SubTechniques
  if (verbose) print(paste("[-][CAR] Parsing CAR and ATT&CK Sub-Techniques relatinship ..."))
  stech <- lapply(coverage.a,
                  function(x) {
                    data.frame(to = unlist(sapply(x, function(k) k[["subtechniques"]])), stringsAsFactors = F)
                  })
  car2stech <- dplyr::bind_rows(stech, .id = "from")


  # CAR -> ATTCK Tactics
  if (verbose) print(paste("[-][CAR] Correlating relations from CAR to ATT&CK Tactics ..."))
  tact <- lapply(coverage.a,
                 function(x) {
                   data.frame(to = unlist(sapply(x, function(k) k[["tactics"]])), stringsAsFactors = F)
                 })
  car2tact <- dplyr::bind_rows(tact, .id = "from")

  caredges <- dplyr::bind_rows(car2tact, car2tech, car2stech)
  caredges <- unique(caredges)

  # Model
  if (verbose) print(paste("[-][CAR] Parsing Data Model objects ..."))
  model <- plyr::ldply(raw.model,
                       function(x) {
                         actions <- plyr::ldply(x$actions, as.data.frame)
                         fields <- plyr::ldply(x$fields, function(x) as.data.frame(x))
                         x$actions <- NULL
                         x$fields <- NULL
                         x <- dplyr::full_join(as.data.frame(x), actions, by = character())
                         x <- dplyr::full_join(x, fields, by = character())
                         names(x) <- c("name", "description", "action.name",
                                       "action.description", "field.name",
                                       "field.description", "field.example")
                         x
                       })

  # Sensor

  # Network
  if (verbose) print(paste("[-][CAR] Building network graph from relationships ..."))
  carnodes <- data.frame(id = character(), label = character(), group = character(),
                         value = numeric(), shape = character(), title = character(),
                         color = character(), shadow = logical(), stringsAsFactors = FALSE)

  nodes.a <- analytics[,c("id", "title")]
  nodes.a$label <- nodes.a$title
  nodes.a$group <- rep("car", nrow(nodes.a))
  nodes.a$value <- rep(4, nrow(nodes.a))
  nodes.a$shape <- rep("circle", nrow(nodes.a))
  nodes.a$color <- rep("aliceblue", nrow(nodes.a))
  nodes.a$shadow <- rep(FALSE, nrow(nodes.a))
  nodes.a$team <- rep("BLUE", nrow(nodes.a))

  carnodes <- dplyr::bind_rows(carnodes, nodes.a)

  caredges$team <- rep("BLUE", nrow(caredges))
  caredges$label <- rep("detects", nrow(caredges))
  caredges$arrows <- rep("to", nrow(caredges))
  caredges$title <- rep("detects", nrow(caredges))
  caredges$dashes <- rep(FALSE, nrow(caredges))

  car <- list(analytics = analytics,
              model = model,
              carnet = list(nodes = carnodes,
                            edges = caredges))

  return(car)
}
