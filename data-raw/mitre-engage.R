library(jsonlite, warn.conflicts = F)
library(plyr, warn.conflicts = F)
library(dplyr, warn.conflicts = F)

if (!dir.exists("data")) dir.create("data")
if (!dir.exists("data-raw/engage")) dir.create("data-raw/engage")

# Download and apply simple parser to raw data
req <- httr::GET("https://api.github.com/repos/mitre/engage/git/trees/main?recursive=1")
httr::stop_for_status(req)
filelist <- unlist(lapply(httr::content(req)$tree, "[", "path"), use.names = F)
filepath <- sapply(sapply(strsplit(grep("Data/json/.*.json", filelist, value = T), "/"),
                          function(x) x[3]),
                   function(x)
                     paste0("https://raw.githubusercontent.com/mitre/engage/main/Data/json/",x))
filelist <- names(filepath)

engage <- list()
for (i in 1:length(filelist)) {
  n <- filelist[i]
  if (!file.exists(paste0("data-raw/engage", "/", n))) {
    download.file(url = filepath[i],
                  destfile = paste0("data-raw/engage", "/", n))
  }
  engage[[n]] <- fromJSON(paste0("data-raw/engage", "/", n))
}

# Approaches
engage.approaches <- plyr::ldply(engage$approach_details.json,
                          function(x)
                            as.data.frame(x[c("name", "type",
                                              "description", "long_description")]))


# Goals
engage.goals <- plyr::ldply(engage$goal_details.json,
                          function(x)
                            as.data.frame(x[c("name", "type",
                                              "description", "long_description")]))

# Activities
engage.activities <- plyr::ldply(engage$activity_details.json,
                     function(x)
                       as.data.frame(x[c("name", "type",
                                         "description", "long_description")]))


relations <- engage$approach_activity_mappings.json
names(relations) <- c("from", "to")

g2ap <- engage$goal_approach_mappings.json
names(g2ap) <- c("from", "to")
relations <- dplyr::bind_rows(relations, g2ap)

atm <- engage$attack_mapping.json
a2e <- atm[, c("attack_id", "eav_id")]
names(a2e) <- c("from", "to")
relations <- dplyr::bind_rows(relations, a2e)

a2eac <- atm[, c("attack_id", "eac_id")]
names(a2eac) <- c("from", "to")
relations <- dplyr::bind_rows(relations, a2eac)
engage.relations <- unique(relations)

usethis::use_data(engage.activities, compress = "xz", overwrite = TRUE)
usethis::use_data(engage.approaches, compress = "xz", overwrite = TRUE)
usethis::use_data(engage.goals, compress = "xz", overwrite = TRUE)
usethis::use_data(engage.relations, compress = "xz", overwrite = TRUE)

rm(req, filelist, filepath, i, n, atm, g2ap, a2e, a2eac, relations, engage)
