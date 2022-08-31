NULL

#' Cybersecurity Standards Framework provided by MITRE and NIST
#'
#' List of standards:
#'  - cpe: cpe.nist
#'  - cve: cve.nist
#'  - cwe: cwe.views, cwe.categories, cwe.weaknesses
#'  - capec: capec.views, capec.categories, capec.patterns, capec.relations
#'  - attck: attck.tactics, attck.techniques, attck.mitigations, attck.groups,
#'           attck.software, attck.data_component, attck.data_relations, attck.relations
#'  - shield: shield.tactics, shield.techniques, shield.procedures, shield.use_cases,
#'            shield.opportunities, shield.relations
#'  - engage: engage.goals, engage.approaches, engage.activities, engage.av, engage.relations
#'  - car: car.analytics, car.model, car.sensors, car.implementations, car.coverage, car.relations
#'
#' Each standard is a list of data frames that represents the entities of the
#' standard and its relationships.
#'
#' @export
#' @format A list of standards, were every standard is a list of data frames
"standards"
