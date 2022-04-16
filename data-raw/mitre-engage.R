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

# Adversary vulnerabilities
engage.av <- engage$attack_mapping.json %>% select(eav_id, eav) %>% unique()

relations <- engage$approach_activity_mappings.json
names(relations) <- c("from", "to")
relations$from_type <- rep("engage approach", nrow(relations))
relations$to_type <- rep("engage activity", nrow(relations))
relations$label <- rep("can_include", nrow(relations))

g2ap <- engage$goal_approach_mappings.json
names(g2ap) <- c("from", "to")
g2ap$from_type <- rep("engage goal", nrow(g2ap))
g2ap$to_type <- rep("engage approach", nrow(g2ap))
g2ap$label <- rep("can_try", nrow(g2ap))
relations <- dplyr::bind_rows(relations, g2ap)

atm <- engage$attack_mapping.json
a2e <- atm[, c("attack_id", "eav_id")] %>% unique()
names(a2e) <- c("from", "to")
a2e$from_type <- rep("attack technique", nrow(a2e))
a2e$to_type <- rep("engage adversary_vulnerability", nrow(a2e))
a2e$label <- rep("may_show", nrow(a2e))
relations <- dplyr::bind_rows(relations, a2e)

eav2eac <- atm[, c("eav_id", "eac_id")] %>% unique()
names(eav2eac) <- c("from", "to")
eav2eac$from_type <- rep("engage adversary_vulnerability", nrow(eav2eac))
eav2eac$to_type <- rep("engage activity", nrow(eav2eac))
eav2eac$label <- rep("defend_with", nrow(eav2eac))
relations <- dplyr::bind_rows(relations, eav2eac)
engage.relations <- unique(relations)

usethis::use_data(engage.activities, compress = "xz", overwrite = TRUE)
usethis::use_data(engage.approaches, compress = "xz", overwrite = TRUE)
usethis::use_data(engage.goals, compress = "xz", overwrite = TRUE)
usethis::use_data(engage.av, compress = "xz", overwrite = TRUE)
usethis::use_data(engage.relations, compress = "xz", overwrite = TRUE)

rm(req, filelist, filepath, i, n, atm, g2ap, a2e, eav2eac, relations, engage)
