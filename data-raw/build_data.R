# for(f in list.files(path = "data", pattern = "*.rda", full.names = T)) load(f)
# rm(f)

source("data-raw/mitre-cwe.R")
source("data-raw/mitre-attck.R")
source("data-raw/mitre-capec.R")
source("data-raw/mitre-car.R")
source("data-raw/mitre-shield.R")
source("data-raw/nist-cpe.R")
source("data-raw/nist-cve.R")

# Sample data... it's so huge
set.seed(42)
cpe.nist <- dplyr::sample_n(cpe.nist, 500)
cve.nist <- dplyr::sample_n(cve.nist, 1000)
attck.relations <- dplyr::sample_n(attck.relations, 4000)

usethis::use_data(cve.nist, cpe.nist, capec.views, capec.categories, capec.patterns,
                  capec.relations, car.analytics, car.model, car.sensors, car.implementations,
                  car.coverage, car.relations, shield.tactics, shield.techniques, shield.procedures,
                  shield.use_cases, shield.opportunities, shield.relations, attck.tactics,
                  attck.techniques, attck.mitigations, attck.groups,
                  attck.software, attck.relations,
                  compress = "xz", overwrite = TRUE, internal = TRUE)
