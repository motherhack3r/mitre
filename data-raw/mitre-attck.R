if(any(grepl("package:RJSONIO", search()))) detach("package:RJSONIO")
library(jsonlite)
library(usethis)
library(tidyr, warn.conflicts = FALSE)
library(dplyr, warn.conflicts = FALSE)

if (!dir.exists("data")) dir.create("data")
if (!dir.exists("data-raw/attack")) dir.create("data-raw/attack")

# ATTACK ENTERPRISE
# Ref: https://github.com/mitre/cti/blob/master/USAGE.md#the-attck-data-model

# Download data
if (!file.exists("data-raw/attack/attack-enterprise.json")) {
  download.file(url = "https://github.com/mitre/cti/raw/master/enterprise-attack/enterprise-attack.json",
                destfile = "data-raw/attack/attack-enterprise.json")
}
attck.ent <- fromJSON("data-raw/attack/attack-enterprise.json")


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

### Data sources
dsrc <- attck.ent$objects[attck.ent$objects$type == "x-mitre-data-source", ]
dsrc <- dsrc[, apply(dsrc, 2, function(x) !all(is.na(x)))]
dsrc <- dsrc[, apply(dsrc, 2, function(x) !is.null(unlist(x)))]

dsrc <- bind_cols(dsrc %>% select(-external_references, -x_mitre_contributors),
                bind_rows(dsrc$external_references) %>%
                  filter(source_name == "mitre-attack") %>%
                  select(-description))

dsrc <- unnest(unnest(dsrc, x_mitre_platforms), x_mitre_collection_layers)
dsrc$x_mitre_platforms <- tolower(dsrc$x_mitre_platforms)
dsrc$x_mitre_platforms <- gsub(" ", "_", dsrc$x_mitre_platforms)
dsrc$x_mitre_platforms <- sapply(dsrc$x_mitre_platforms, function(x) paste0("platform_", x))
dsrc$x_mitre_collection_layers <- tolower(dsrc$x_mitre_collection_layers)
dsrc$x_mitre_collection_layers <- gsub(" ", "_", dsrc$x_mitre_collection_layers)
dsrc$x_mitre_collection_layers <- sapply(dsrc$x_mitre_collection_layers, function(x) paste0("collect_", x))

dsrc$object_marking_refs <- as.character(dsrc$object_marking_refs)
dsrc$val <- rep(T, nrow(dsrc))
dsrc <- spread(dsrc, key = x_mitre_platforms, value = val, fill = F)
dsrc$val <- rep(T, nrow(dsrc))
dsrc <- spread(dsrc, key = x_mitre_collection_layers, value = val, fill = F)


### Data Sources relations
tech <- attck.ent$objects[attck.ent$objects$type == "attack-pattern", ]
dsrl <- tech %>%
  select(id, x_mitre_data_sources) %>%
  unnest(x_mitre_data_sources) %>%
  separate(col = x_mitre_data_sources,
           into=c("data_component", "data_source"), sep = ": ")
dsrl$data_source <- stringr::str_trim(
  stringr::str_remove_all(string = dsrl$data_source,
                          pattern = paste0(sort(unique(dsrl$data_component),
                                                decreasing = T),
                                           collapse = "|")))
dscomp <- dsrl %>%
  select(data_component, data_source) %>%
  unique() %>%
  group_by(data_component) %>%
  nest() %>%
  mutate(data_source = paste0(unlist(data), collapse = ", ")) %>%
  select(-data) %>%
  ungroup()

attck.data_component <- dplyr::left_join(dsrc, dscomp, by = c("name"="data_component"))

attck.data_relations <- dsrl %>%
  left_join(y = attck.techniques %>%
              select(id, external_id),
            by = c("id"="id")) %>%
  left_join(y = attck.data_component %>%
              select(name, external_id),
            by = c("data_component"="name"))
names(attck.data_relations) <- c("source_ref", "target_ref", "label", "from_std", "to_std")
attck.data_relations <- attck.data_relations[, c("label", "from_std", "to_std")]

## Mitigation (course-of-action)
miti <- attck.ent$objects[attck.ent$objects$type == "course-of-action", ]
miti <- miti[, apply(miti, 2, function(x) !all(is.na(x)))]
miti <- miti[, apply(miti, 2, function(x) !is.null(unlist(x)))]
miti$x_mitre_deprecated[is.na(miti$x_mitre_deprecated)] <- FALSE
miti <- miti %>% filter(!x_mitre_deprecated) %>% select(-x_mitre_deprecated)

## TODO: Remove head and improve nested references
attck.mitigations <- bind_cols(bind_rows(sapply(miti$external_references, function(x) head(x,1))) %>% select(-description),
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

rm(attck.ent, grup, miti, rels, tact, tech, soft, df, dscomp, dsrl, dsrc)
