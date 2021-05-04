library(jsonlite)
library(usethis)

raw.url <- "https://raw.githubusercontent.com/MITRECND/mitrecnd.github.io/master/_data/use_cases.json"
shield.use_cases <- fromJSON(raw.url)

write_json(shield.use_cases, "data-raw/shield-use_cases.json")
usethis::use_data(shield.use_cases, compress = "xz", overwrite = T)
