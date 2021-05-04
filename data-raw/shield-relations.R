library(jsonlite)
library(usethis)
library(plyr)
library(dplyr, warn.conflicts = FALSE)

tact_det_url <- "https://raw.githubusercontent.com/MITRECND/mitrecnd.github.io/master/_data/tactic_details.json"
tact_det <- fromJSON(tact_det_url)
tact_det <- ldply(tact_det, function(x) x[["techniques"]])
names(tact_det)[1:2] <- c("tact_id", "tech_id")

tech_det_url <- "https://raw.githubusercontent.com/MITRECND/mitrecnd.github.io/master/_data/technique_details.json"
tech_det <- jsonlite::fromJSON(tech_det_url)

## Shield Tactic --> Shield Technique
relations <- tact_det %>% select(tact_id, tech_id)
names(relations) <- c("from", "to")

## Shield Technique --> ATT&CK Tactic
df <- ldply(tech_det, function(x) data.frame(to = x[["attack_tactics"]][["id"]]))
names(df) <- c("from", "to")
relations <- bind_rows(relations, df)

## Shield Technique --> ATT&CK Techniques
df <- ldply(tech_det, function(x) data.frame(to = x[["attack_techniques"]][["id"]]))
names(df) <- c("from", "to")
relations <- bind_rows(relations, df)

## Shield Technique --> Use Cases
df <- ldply(tech_det, function(x) data.frame(to = x[["use_cases"]][["id"]]))
names(df) <- c("from", "to")
relations <- bind_rows(relations, df)

## Shield Technique --> Opportunities
df <- ldply(tech_det, function(x) data.frame(to = x[["opportunities"]][["id"]]))
names(df) <- c("from", "to")
relations <- bind_rows(relations, df)

## Shield Technique --> Procedures
df <- ldply(tech_det, function(x) data.frame(to = x[["procedures"]][["id"]]))
names(df) <- c("from", "to")
shield.relations <- bind_rows(relations, df)

write_json(tact_det, "data-raw/shield-tactic_details.json")
write_json(tech_det, "data-raw/shield-technique_details.json")
usethis::use_data(shield.relations, compress = "xz", overwrite = TRUE)
