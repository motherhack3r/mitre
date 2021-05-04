library(jsonlite)
library(usethis)

raw.url <- "https://raw.githubusercontent.com/MITRECND/mitrecnd.github.io/master/_data/procedures.json"
shield.procedures <- fromJSON(raw.url)

write_json(shield.procedures, "data-raw/shield-procedures.json")
usethis::use_data(shield.procedures, compress = "xz", overwrite = TRUE)
