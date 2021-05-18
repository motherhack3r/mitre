source("data-raw/mitre-cwe.R")
source("data-raw/mitre-attck.R")
source("data-raw/mitre-capec.R")
source("data-raw/mitre-car.R")
source("data-raw/mitre-shield.R")
source("data-raw/nist-cpe.R")
source("data-raw/nist-cve.R")


# Sample data... it's so huge
nodes <- mitre::build_network(as_igraph = F)[["nodes"]]
cpe.nist <- cpe.nist[cpe.nist$cpe.23 %in% nodes$standard,]
cpe.nist <- dplyr::sample_n(cpe.nist, 1000)
cpe.nist <- dplyr::select(cpe.nist, cpe.23, title, part, vendor, product, version, refs, deprecated)

usethis::use_data(cpe.nist, compress = "xz", overwrite = TRUE, internal = TRUE)

cve.nist <- dplyr::sample_n(cve.nist, 1000)

usethis::use_data(cve.nist, cpe.nist, capec.views, capec.categories, capec.patterns,
                  capec.relations, car.analytics, car.model, car.sensors, car.implementations,
                  car.coverage, car.relations, shield.tactics, shield.techniques, shield.procedures,
                  shield.use_cases, shield.opportunities, shield.relations, attck.tactics,
                  attck.techniques, attck.mitigations, attck.groups,
                  attck.software, attck.relations,
                  compress = "xz", overwrite = TRUE, internal = TRUE)
