library(jsonlite)
library(usethis)

raw.url <- "https://raw.githubusercontent.com/MITRECND/mitrecnd.github.io/master/_data/techniques.json"
shield.techniques <- fromJSON(raw.url)

write_json(shield.techniques, "data-raw/shield-techniques.json")
usethis::use_data(shield.techniques, compress = "xz", overwrite = TRUE)
