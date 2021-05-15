# library(jsonlite)
library(usethis)
library(tidyr, warn.conflicts = FALSE)
library(dplyr, warn.conflicts = FALSE)

if (!dir.exists("data")) dir.create("data")

# Download
if (length(list.files(path = "data-raw", pattern = "^car-.*\\.yaml$")) != 3) {
  req <- httr::GET("https://api.github.com/repos/mitre-attack/car/git/trees/master?recursive=1")
  httr::stop_for_status(req)
  filelist <- unlist(lapply(httr::content(req)$tree, "[", "path"), use.names = F)
  rm(req)

  raw.analytics <- suppressWarnings(lapply(paste0("https://raw.githubusercontent.com/mitre-attack/car/master/",
                                                  grep("analytics/CAR.*yaml", filelist,
                                                       value = TRUE, perl = TRUE)),
                                           yaml::read_yaml))
  raw.model <- suppressWarnings(lapply(paste0("https://raw.githubusercontent.com/mitre-attack/car/master/",
                                              grep("data_model/.*yaml", filelist,
                                                   value = TRUE, perl = TRUE)),
                                       yaml::read_yaml))
  raw.sensors <- suppressWarnings(lapply(paste0("https://raw.githubusercontent.com/mitre-attack/car/master/",
                                                grep("^sensors/.*yaml$", filelist,
                                                     value = TRUE, perl = TRUE)),
                                         yaml::read_yaml))

}

#####
## Analytics, Implementations and Coverage
analytics <- lapply(raw.analytics,
                    function(x) {
                      x[["implementations"]] <- jsonlite::toJSON(x[["implementations"]])
                      x[["platforms"]] <- jsonlite::toJSON(x[["platforms"]])
                      x[["subtypes"]] <- jsonlite::toJSON(x[["subtypes"]])
                      x[["contributors"]] <- jsonlite::toJSON(x[["contributors"]])
                      x[["analytic_types"]] <- jsonlite::toJSON(x[["analytic_types"]])
                      x[["unit_tests"]] <- jsonlite::toJSON(x[["unit_tests"]])
                      x[["coverage"]] <- NULL
                      x[["Coverage"]] <- NULL
                      x[["references"]] <- jsonlite::toJSON(x[["references"]])
                      x[["true_positives"]] <- jsonlite::toJSON(x[["true_positives"]])
                      x[["data_model_references"]] <- NULL
                      x
                    })
analytics <- dplyr::bind_rows(analytics)
analytics$submission_date <- as.Date(analytics$submission_date)
analytics$subtypes <- as.character(analytics$subtypes)
analytics$contributors <- as.character(analytics$contributors)
analytics$unit_tests <- as.character(analytics$unit_tests)

# Issue: https://github.com/mitre-attack/car/pull/120
analytics$information_domain[analytics$id == "CAR-2013-07-005"] <- "Host"
analytics$platforms[analytics$id == "CAR-2013-07-005"] <- '["Windows", "Linux", "macOS"]'

# Remove near empty columns or nested
analytics$references <- NULL
analytics$true_positives <- NULL

# uniq.infodom <- unique(stringr::str_trim(unlist(strsplit(analytics$information_domain, ","))))
analytics$domain_analytic <- grepl("Analytic", analytics$information_domain)
analytics$domain_host <- grepl("Host", analytics$information_domain)
analytics$domain_network <- grepl("Network", analytics$information_domain)
analytics$information_domain <- NULL

# uniq.platforms <- unique(unlist(sapply(analytics$platforms, function(x) jsonlite::fromJSON(x))))
analytics$platform_windows <- grepl("Windows", analytics$platforms)
analytics$platform_linux <- grepl("Linux", analytics$platforms)
analytics$platform_macos <- grepl("macOS", analytics$platforms)
analytics$platforms <- NULL

# uniq.types <- unique(unlist(sapply(analytics$analytic_types, function(x) jsonlite::fromJSON(x))))
analytics$type_ttp <- grepl("TTP", analytics$analytic_types)
analytics$type_detection <- grepl("Detection", analytics$analytic_types)
analytics$type_anomaly <- grepl("Anomaly", analytics$analytic_types)
analytics$type_awareness <- grepl("Situational Awareness", analytics$analytic_types)
analytics$analytic_types <- NULL

uniq.subtypes <- unique(unlist(sapply(analytics$subtypes, function(x) jsonlite::fromJSON(x))))

# Analytics Implementations
implementations <- lapply(analytics$implementations, function(x) jsonlite::fromJSON(x))
for (ca in which(sapply(implementations, length) == 0)) implementations[[ca]] <- data.frame(stringsAsFactors = F)
names(implementations) <- analytics$id
implementations <- dplyr::bind_rows(implementations, .id = "column_label")
rm(ca)

implementations$description[sapply(implementations$description, is.null)] <- ""
implementations$code[sapply(implementations$code, is.null)] <- ""
implementations$type[sapply(implementations$type, is.null)] <- ""
implementations$data_model[sapply(implementations$data_model, is.null)] <- ""
implementations$name[sapply(implementations$name, is.null)] <- ""
implementations$data_mode[sapply(implementations$data_mode, is.null)] <- ""

implementations$description <- unlist(implementations$description)
implementations$code <- unlist(implementations$code)
implementations$type <- unlist(implementations$type)
implementations$data_model <- unlist(implementations$data_model)
implementations$name <- unlist(implementations$name)
implementations$data_mode <- unlist(implementations$data_mode)

implementations <- dplyr::mutate(implementations, data_model = paste0(data_model, data_mode))
implementations$data_mode <- NULL
analytics$implementations <- NULL

# Coverage relations
coverage <- lapply(raw.analytics, function(x) x[["coverage"]])
names(coverage) <- analytics$id
# Issue: https://github.com/mitre-attack/car/pull/119
# coverage2 <- lapply(raw.analytics, function(x) x[["Coverage"]])
# names(coverage2) <- analytics$id

covnet <- data.frame(from = character(), to = character(), coverage = character(),
                     force = integer(), stringsAsFactors = F)
for (i in 1:length(coverage)) {
  cx <- coverage[[i]]
  cx.id <- names(coverage)[i]
  if (is.null(cx)) next

  kk <- plyr::ldply(cx, function(y) {
    z <- data.frame(to = character(), coverage = character(), stringsAsFactors = F)
    if ("tactics" %in% names(y)) {
      k <- crossing(y[["tactics"]], y$coverage)
      names(k) <- c("to", "coverage")
      z <- z %>% bind_rows(z, k)
    }
    if ("technique" %in% names(y)) {
      k <- crossing(y[["technique"]], y$coverage)
      names(k) <- c("to", "coverage")
      z <- z %>% bind_rows(z, k)
    }
    if ("subtechniques" %in% names(y)) {
      k <- crossing(y[["subtechniques"]], y$coverage)
      names(k) <- c("to", "coverage")
      z <- z %>% bind_rows(z, k)
    }
    z
  })
  kk$from <- rep(cx.id, nrow(kk))
  kk <- kk %>% group_by(to, from, coverage) %>% summarise(.groups = "keep", force = n())
  covnet <- dplyr::bind_rows(covnet, as.data.frame(kk))
  rm(kk, cx, cx.id)
}
coverage <- covnet
rm(covnet, i)

#####
## Data Model
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
model$model.id <- apply(model, 1, function(x) paste0(x["name"], "/", x["action.name"], "/", x["field.name"]))
model$model.id <- tolower(model$model.id)

modelnet <- lapply(raw.analytics, function(x) x[["data_model_references"]])
names(modelnet) <- analytics$id

modelnet <- plyr::ldply(modelnet, as.data.frame)
names(modelnet) <- c("from", "to")

#####
## Sensors
sensors <- plyr::ldply(raw.sensors,
                       function(x) {
                         x$mappings <- NULL
                         x$other_coverage <- NULL
                         as.data.frame(x)
                       })

sensnet <- lapply(raw.sensors, function(x) x[["mappings"]])
names(sensnet) <- sensors$sensor_name

sensnet <- plyr::ldply(sensnet,
                     function(x) {
                       mappings <- plyr::ldply(x, as.data.frame)
                       mappings$notes <- NULL
                       mappings$model.id <- apply(mappings, 1,
                                                  function(x)
                                                    paste0(x["object"], "/", x["action"], "/", x["fields"]))
                       mappings$object <- NULL
                       mappings$action <- NULL
                       mappings$fields <- NULL
                       mappings
                     })
names(sensnet) <- c("to", "from")

relations <- dplyr::bind_rows(modelnet, sensnet)
rm(modelnet, sensnet)

#####
## Save data sets
car.analytics <- analytics
usethis::use_data(car.analytics, compress = "xz", overwrite = TRUE)
jsonlite::write_json(car.analytics, "data-raw/car-analytics.json")

car.model <- model
usethis::use_data(car.model, compress = "xz", overwrite = TRUE)
jsonlite::write_json(model, "data-raw/car-model.json")

car.sensors <- sensors
usethis::use_data(car.sensors, compress = "xz", overwrite = TRUE)
jsonlite::write_json(sensors, "data-raw/car-sensors.json")

car.implementations <- implementations
usethis::use_data(car.implementations, compress = "xz", overwrite = TRUE)
jsonlite::write_json(implementations, "data-raw/car-implementations.json")

car.coverage <- coverage
usethis::use_data(car.coverage, compress = "xz", overwrite = TRUE)
jsonlite::write_json(coverage, "data-raw/car-coverage.json")

car.relations <- relations
usethis::use_data(car.relations, compress = "xz", overwrite = TRUE)
jsonlite::write_json(relations, "data-raw/car-relations.json")

