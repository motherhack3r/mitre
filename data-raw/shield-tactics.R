library(jsonlite)
library(usethis)

raw.url <- "https://raw.githubusercontent.com/MITRECND/mitrecnd.github.io/master/_data/tactics.json"
shield.tactics <- fromJSON(raw.url)

write_json(shield.tactics, "data-raw/shield-tactics.json")
usethis::use_data(shield.tactics, compress = "xz", overwrite = TRUE)
