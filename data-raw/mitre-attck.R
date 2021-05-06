library(jsonlite)
library(usethis)
library(dplyr, warn.conflicts = FALSE)
library(tidyr, warn.conflicts = FALSE)

if (!dir.exists("data")) dir.create("data")

# ATTACK ENTERPRISE
# Ref: https://github.com/mitre/cti/blob/master/USAGE.md#the-attck-data-model

# Tactics
if (!file.exists("data-raw/attack-enterprise.json"))
  download.file(url = "https://github.com/mitre/cti/raw/master/enterprise-attack/enterprise-attack.json",
                destfile = "data-raw/attack-enterprise.json")
attck.ent <- fromJSON("data-raw/attack-enterprise.json")


## Tactics (x-mitre-tactic)
tact <- attck.ent$objects[attck.ent$objects$type == "x-mitre-tactic", ]
tact <- tact[, apply(tact, 2, function(x) !all(is.na(x)))]
tact <- tact[, apply(tact, 2, function(x) !is.null(unlist(x)))]
attck.tactics <- bind_cols(bind_rows(tact$external_references),
                  tact %>% select(-object_marking_refs, -external_references))

## Techniques (attack-pattern)
tech <- attck.ent$objects[attck.ent$objects$type == "attack-pattern", ]
tech <- tech[, apply(tech, 2, function(x) !all(is.na(x)))]
tech <- tech[, apply(tech, 2, function(x) !is.null(unlist(x)))]
tech <- bind_cols(bind_rows(tech$external_references) %>%
                    filter(source_name == "mitre-attack") %>%
                    select(-description),
                  tech %>%
                    select(-kill_chain_phases, -x_mitre_platforms,
                           -x_mitre_data_sources,
                           -x_mitre_permissions_required,
                           -x_mitre_effective_permissions,
                           -x_mitre_defense_bypassed, -x_mitre_impact_type,
                           -object_marking_refs, -external_references,
                           -x_mitre_system_requirements,
                           -x_mitre_contributors))

tech$revoked[is.na(tech$revoked)] <- FALSE
tech$x_mitre_deprecated[is.na(tech$x_mitre_deprecated)] <- FALSE

attck.techniques <- tech %>%
  filter(!revoked, !x_mitre_deprecated) %>%
  select(-revoked, -x_mitre_deprecated)

## Mitigation (course-of-action)
miti <- attck.ent$objects[attck.ent$objects$type == "course-of-action", ]
miti <- miti[, apply(miti, 2, function(x) !all(is.na(x)))]
miti <- miti[, apply(miti, 2, function(x) !is.null(unlist(x)))]
miti$x_mitre_deprecated[is.na(miti$x_mitre_deprecated)] <- FALSE
miti <- miti %>% filter(!x_mitre_deprecated) %>% select(-x_mitre_deprecated)

attck.mitigations <- bind_cols(bind_rows(miti$external_references),
                           miti %>% select(-object_marking_refs, -external_references))

## Group (intrusion-set)
grup <- attck.ent$objects[attck.ent$objects$type == "intrusion-set", ]
grup <- grup[, apply(grup, 2, function(x) !all(is.na(x)))]
grup <- grup[, apply(grup, 2, function(x) !is.null(unlist(x)))]
grup$revoked[is.na(grup$revoked)] <- FALSE
grup <- grup %>% filter(!revoked) %>% select(-revoked)

attck.groups <- bind_cols(bind_rows(grup$external_references) %>%
                            filter(source_name == "mitre-attack") %>%
                            select(-description),
                          grup %>% select(-object_marking_refs, -external_references,
                                          -x_mitre_contributors, -aliases))


## Software (malware, tool)
soft <- attck.ent$objects[attck.ent$objects$type %in% c("malware", "tool"), ]
soft <- soft[, apply(soft, 2, function(x) !all(is.na(x)))]
soft <- soft[, apply(soft, 2, function(x) !is.null(unlist(x)))]
soft$revoked[is.na(soft$revoked)] <- FALSE
soft <- soft %>% filter(!revoked) %>% select(-revoked)
soft$labels <- unlist(soft$labels)

attck.software <- bind_cols(bind_rows(soft$external_references) %>%
                              filter(source_name == "mitre-attack") %>%
                              select(-description),
                            soft %>% select(-x_mitre_platforms, -x_mitre_contributors,
                                            -object_marking_refs, -x_mitre_aliases,
                                            -external_references))


## Relationship (relationship)
rels <- attck.ent$objects[attck.ent$objects$type == "relationship", ]
rels <- rels[, apply(rels, 2, function(x) !all(is.na(x)))]
rels <- rels[, apply(rels, 2, function(x) !is.null(unlist(x)))]
rels <- rels %>%
  separate(source_ref, c("source_type", "source_uuid"), "--", remove = F) %>%
  separate(target_ref, c("target_type", "target_uuid"), "--", remove = F) %>%
  select(-object_marking_refs, -external_references) %>%
  filter(relationship_type != "revoked-by")

df <- bind_rows(attck.tactics %>% select(external_id, id),
                attck.techniques %>% select(external_id, id),
                attck.mitigations %>% select(external_id, id),
                attck.software %>% select(external_id, id),
                attck.groups %>% select(external_id, id))

rels <- dplyr::rename(left_join(rels, df,
                                by = c("source_ref" = "id"), keep = F) ,
                      from = external_id)
rels <- dplyr::rename(left_join(rels, df,
                                by = c("target_ref" = "id"), keep = F) ,
                      to = external_id)

attck.relations <- rels %>%
  filter(!is.na(from) & !is.na(to)) %>%
  select(-source_uuid, -target_uuid)


usethis::use_data(attck.tactics, compress = "xz", overwrite = TRUE)
usethis::use_data(attck.techniques, compress = "xz", overwrite = TRUE)
usethis::use_data(attck.mitigations, compress = "xz", overwrite = TRUE)
usethis::use_data(attck.groups, compress = "xz", overwrite = TRUE)
usethis::use_data(attck.software, compress = "xz", overwrite = TRUE)
usethis::use_data(attck.relations, compress = "xz", overwrite = TRUE)


# ATTACK MOBILE

# mob.url <- "https://github.com/mitre/cti/raw/master/mobile-attack/mobile-attack.json"


# write_json(tact, "data-raw/attck-tactics.json")
# usethis::use_data(attck.tactics, compress = "xz", overwrite = TRUE)
