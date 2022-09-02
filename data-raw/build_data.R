source("data-raw/mitre-cwe.R")
source("data-raw/mitre-attck.R")
source("data-raw/mitre-capec.R")
source("data-raw/mitre-car.R")
source("data-raw/mitre-shield.R")
source("data-raw/mitre-engage.R")
source("data-raw/nist-cpe.R")
source("data-raw/nist-cve.R")

standards <- list(cpe = list(cpe.nist = cpe.nist),
                  cve = list(cve.nist = cve.nist),
                  cwe = list(cwe.views = cwe.views,
                             cwe.categories = cwe.categories,
                             cwe.weaknesses = cwe.weaknesses),
                  capec = list(capec.views = capec.views,
                               capec.categories = capec.categories,
                               capec.patterns = capec.patterns,
                               capec.relations = capec.relations),
                  attck = list(attck.tactics = attck.tactics,
                               attck.techniques = attck.techniques,
                               attck.mitigations = attck.mitigations,
                               attck.groups = attck.groups,
                               attck.software = attck.software,
                               attck.data_component = attck.data_component,
                               attck.data_relations = attck.data_relations,
                               attck.relations = attck.relations),
                  shield = list(shield.tactics = shield.tactics,
                                shield.techniques = shield.techniques,
                                shield.procedures = shield.procedures,
                                shield.use_cases = shield.use_cases,
                                shield.opportunities = shield.opportunities,
                                shield.relations = shield.relations),
                  engage = list(engage.goals = engage.goals,
                                engage.approaches = engage.approaches,
                                engage.activities = engage.activities,
                                engage.av = engage.av,
                                engage.relations = engage.relations),
                  car = list(car.analytics = car.analytics,
                             car.model = car.model,
                             car.sensors = car.sensors,
                             car.implementations = car.implementations,
                             car.coverage = car.coverage,
                             car.relations = car.relations)
                  )

rm(cve.nist, cpe.nist, cwe.views, cwe.categories, cwe.weaknesses,
   capec.views, capec.categories, capec.patterns, capec.relations,
   car.analytics, car.model, car.sensors, car.implementations,
   car.coverage, car.relations, shield.tactics, shield.techniques,
   shield.procedures, shield.use_cases, shield.opportunities,
   shield.relations, attck.tactics, attck.techniques, attck.mitigations,
   attck.groups, attck.software, attck.data_component,
   attck.data_relations, attck.relations, engage.activities,
   engage.approaches, engage.goals, engage.relations, engage.av)

usethis::use_data(standards, compress = "xz", overwrite = TRUE)
usethis::use_data(standards, compress = "xz", overwrite = TRUE, internal = TRUE)
