if(any(grepl("package:RJSONIO", search()))) detach("package:RJSONIO")
library(jsonlite)
library(usethis)
if(any(grepl("package:dplyr", search()))) detach("package:dplyr") else message("dplyr not loaded")
library(plyr, warn.conflicts = FALSE)
library(dplyr, warn.conflicts = FALSE)

if (!dir.exists("data")) dir.create("data")

# Tactics
if (!file.exists("data-raw/shield-tactics.json")) {
  download.file(url = "https://raw.githubusercontent.com/MITRECND/mitrecnd.github.io/master/_data/tactics.json",
                destfile = "data-raw/shield-tactics.json")
}
shield.tactics <- fromJSON("data-raw/shield-tactics.json")

# Techniques
if (!file.exists("data-raw/shield-techniques.json")) {
  download.file(url = "https://raw.githubusercontent.com/MITRECND/mitrecnd.github.io/master/_data/techniques.json",
                destfile = "data-raw/shield-techniques.json")
}
shield.techniques <- fromJSON("data-raw/shield-techniques.json")

# Procedures
if (!file.exists("data-raw/shield-procedures.json")) {
  download.file(url = "https://raw.githubusercontent.com/MITRECND/mitrecnd.github.io/master/_data/procedures.json",
                destfile = "data-raw/shield-procedures.json")
}
shield.procedures <- fromJSON("data-raw/shield-procedures.json")

# Use Cases
if (!file.exists("data-raw/shield-use_cases.json")) {
  download.file(url = "https://raw.githubusercontent.com/MITRECND/mitrecnd.github.io/master/_data/use_cases.json",
                destfile = "data-raw/shield-use_cases.json")
}

shield.use_cases <- fromJSON("data-raw/shield-use_cases.json")

# Opportunities
if (!file.exists("data-raw/shield-opportunities.json")) {
  download.file(url = "https://raw.githubusercontent.com/MITRECND/mitrecnd.github.io/master/_data/opportunities.json",
                destfile = "data-raw/shield-opportunities.json")
}
shield.opportunities <- fromJSON("data-raw/shield-opportunities.json")

# Relations
if (!file.exists("data-raw/shield-tactic_details.json")) {
  download.file(url = "https://raw.githubusercontent.com/MITRECND/mitrecnd.github.io/master/_data/tactic_details.json",
                destfile = "data-raw/shield-tactic_details.json")
}

if (!file.exists("data-raw/shield-technique_details.json")) {
  download.file(url = "https://raw.githubusercontent.com/MITRECND/mitrecnd.github.io/master/_data/technique_details.json",
                destfile = "data-raw/shield-technique_details.json")
}


tact_det <- fromJSON("data-raw/shield-tactic_details.json")
tact_det <- ldply(tact_det, function(x) x[["techniques"]])
names(tact_det)[1:2] <- c("tact_id", "tech_id")
tech_det <- fromJSON("data-raw/shield-technique_details.json")

## Shield Tactic --> Shield Technique
relations <- tact_det %>% select(tact_id, tech_id)
names(relations) <- c("from", "to")
relations$name <- rep("leverage", nrow(relations))

## Shield Technique --> ATT&CK Tactic
df <- ldply(tech_det, function(x) data.frame(to = x[["attack_tactics"]][["id"]]))
names(df) <- c("from", "to")
df$name <- rep("cover", nrow(df))
relations <- bind_rows(relations, df)

## Shield Technique --> ATT&CK Techniques
df <- ldply(tech_det, function(x) data.frame(to = x[["attack_techniques"]][["id"]]))
names(df) <- c("from", "to")
df$name <- rep("defend", nrow(df))
relations <- bind_rows(relations, df)

## Shield Technique --> Use Cases
df <- ldply(tech_det, function(x) data.frame(to = x[["use_cases"]][["id"]]))
names(df) <- c("from", "to")
df$name <- rep("use", nrow(df))
relations <- bind_rows(relations, df)

## Shield Technique --> Opportunities
df <- ldply(tech_det, function(x) data.frame(to = x[["opportunities"]][["id"]]))
names(df) <- c("from", "to")
df$name <- rep("has", nrow(df))
relations <- bind_rows(relations, df)

## Shield Technique --> Procedures
df <- ldply(tech_det, function(x) data.frame(to = x[["procedures"]][["id"]]))
names(df) <- c("from", "to")
df$name <- rep("implement", nrow(df))
shield.relations <- bind_rows(relations, df)

# Save raw files and data sets
usethis::use_data(shield.tactics, compress = "xz", overwrite = TRUE)
usethis::use_data(shield.techniques, compress = "xz", overwrite = TRUE)
usethis::use_data(shield.procedures, compress = "xz", overwrite = TRUE)
usethis::use_data(shield.use_cases, compress = "xz", overwrite = T)
usethis::use_data(shield.opportunities, compress = "xz", overwrite = TRUE)
usethis::use_data(shield.relations, compress = "xz", overwrite = TRUE)

rm(df, relations, tact_det, tech_det)
