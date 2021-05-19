# for(f in list.files(path = "data", pattern = "*.rda", full.names = T)) load(f)
# rm(f)

source("data-raw/mitre-cwe.R")
source("data-raw/mitre-attck.R")
source("data-raw/mitre-capec.R")
source("data-raw/mitre-car.R")
source("data-raw/mitre-shield.R")
source("data-raw/nist-cpe.R")
source("data-raw/nist-cve.R")

# Build sample data sets
# set.seed(42)
# cpe.nist <- dplyr::sample_frac(cpe.nist, 0.0002)
# cve.nist <- dplyr::sample_frac(cve.nist, 0.005)
# cwe.weaknesses <- cwe.weaknesses[cwe.weaknesses$Status != "Deprecated", ]
# cwe.weaknesses <- dplyr::sample_frac(cwe.weaknesses, 0.6)
# cwe.categories <- cwe.weaknesses[cwe.categories$Status != "Deprecated", ]
# cwe.categories <- dplyr::sample_frac(cwe.categories, 0.6)
# capec.patterns <- dplyr::sample_frac(capec.patterns, 0.6)
# attck.relations <- dplyr::sample_frac(attck.relations, 0.6)
# attck.techniques <- dplyr::sample_frac(attck.techniques, 0.6)
# attck.software <- dplyr::sample_frac(attck.software, 0.6)
# car.analytics <- dplyr::sample_frac(car.analytics, 0.6)
# car.model <- dplyr::sample_frac(car.model, 0.6)

usethis::use_data(cve.nist, cpe.nist, cwe.views, cwe.categories, cwe.weaknesses,
                  capec.views, capec.categories, capec.patterns, capec.relations,
                  car.analytics, car.model, car.sensors, car.implementations,
                  car.coverage, car.relations, shield.tactics, shield.techniques,
                  shield.procedures, shield.use_cases, shield.opportunities,
                  shield.relations, attck.tactics, attck.techniques, attck.mitigations,
                  attck.groups, attck.software, attck.relations,
                  compress = "xz", overwrite = TRUE, internal = TRUE)
