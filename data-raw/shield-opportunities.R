library(jsonlite)
library(usethis)

raw.url <- "https://raw.githubusercontent.com/MITRECND/mitrecnd.github.io/master/_data/opportunities.json"
shield.opportunities <- fromJSON(raw.url)

write_json(shield.opportunities, "data-raw/shield-opportunities.json")
usethis::use_data(shield.opportunities, compress = "xz", overwrite = TRUE)
