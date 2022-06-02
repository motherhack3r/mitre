library(tidyr)
library(dplyr)
library(neo2R)

verbose <- TRUE

# Load mitre network
mitrenet <- readRDS("inst/temp/mitrenet.rds")

# remove cpe nodes and related edges
mnodes <- mitrenet$nodes %>% filter(group != "cpe") # %>% filter(group != "cve")
medges <- mitrenet$edges %>%
  inner_join(mnodes[,c("id", "type")], by = c("from" = "id")) %>%
  inner_join(mnodes[,c("id", "type")], by = c("to" = "id"))
mnodes <- mnodes[mnodes$id %in% unique(c(medges$from, medges$to)), ]
medges <- medges[, c("from", "to", "title", "label", "color", "dashes")]

# Replace invalid characters
mnodes$title <- gsub('"', "", mnodes$title)
mnodes$label <- gsub('"', "", mnodes$label)
mnodes$group <- gsub('-',"_",mnodes$group)
# medges$title <- gsub('(\\|\"|\\s|-|\\[|\\])', "_", medges$title)
medges$label <- gsub(pattern = "(\\s|-)", "_", medges$label)

# Connect to Neo4j
importPath = "C:/DEVEL/datasets/neo4j/relate-data/dbmss/dbms-824eff48-c555-449e-8a0e-f605791a2aa6/import"
graph <- startGraph(url = "localhost:7474", importPath = importPath,
                    username = "neo4j", password = "devel4free")

# Add nodes
ntype <- sort(unique(mnodes$group))
for (g in ntype) {
  dfimport <- mnodes[mnodes$group == g, c("id", "label","group", "type", "title",
                                          "hidden", "mass", "standard", "value", "color")]
  if (verbose) print(paste("Adding", nrow(dfimport), tolower(g), "nodes"))
  scql <- paste0("MERGE (n:mitre:", tolower(g), " {id:row.id, label:row.label, ",
                 "group:row.group, type:row.type, title:row.title, old:row.hidden, ",
                 "mass:row.mass, name:row.standard, value:row.value, color:row.color})")
  import_from_df(graph = graph, cql = scql, toImport = dfimport)
}

try(neo2R::cypher(graph, 'CREATE INDEX ON :mitre(id)'))
try(neo2R::cypher(graph, 'CREATE INDEX ON :mitre(type)'))
try(neo2R::cypher(graph, 'CREATE INDEX ON :mitre(group)'))


# Add edges
etype <- sort(unique(medges$label))
for (e in etype) {
  dfimport <- medges[medges$label == e, c("from", "to", "color", "dashes")]
  if (verbose) print(paste("Adding", nrow(dfimport), toupper(e), "edges"))
  import_from_df(graph = graph,
                 cql = prepCql("MATCH (f:mitre {id:row.from}) ",
                               "MATCH (t:mitre {id:row.to}) ",
                               paste0("MERGE (f)-[r:", toupper(e) ," {color:row.color, dashes:row.dashes}]->(t)")),
                 toImport = dfimport)
}



#
#
#
#
#
#
#
#
#
# # mnet <- mitre::build_network(T, T)
# # saveRDS(mnet, "inst/temp/mnet.rds")
# mnet <- readRDS("inst/temp/mnet.rds")
#
#
# # A partir de la deteccion de ataques (ATTCK) identificar aquellos assets
# # más susceptibles y proporcionar mejores formas de defensa y prevención.
#
# t <- tempfile()
# download.file(url = "https://github.com/hrbrmstr/attckr/raw/master/inst/extdat/more-incidents.rds", destfile = t)
# incidents <- readRDS(t)
# incidents$inc_id <- 1:nrow(incidents)
# incidents <- incidents %>% unnest("mitre_attack")
#
# # Select sample org = 01aa9757
# df <- incidents %>%
#   filter(org == "01aa9757") %>%
#   select(first_event_ts, technique, id) %>%
#   arrange(first_event_ts)
#
# unique(df$id)
#
#
# ## Neo4j
#
# # remove cpe from graph
# mnodes <- mitrenet$nodes %>% filter(group != "cpe")
# medges <- mitrenet$edges %>%
#   inner_join(mnodes[,c("id", "type")], by = c("from" = "id")) %>%
#   inner_join(mnodes[,c("id", "type")], by = c("to" = "id"))
# mnodes <- mnodes[mnodes$id %in% unique(c(medges$from, medges$to)), ]
# medges <- medges[, c("from", "to", "title", "label", "dashes")]
#
# # mnodes$description <- gsub(pattern = "(\\|\")", "_", mnodes$description)
# # mnodes$description <- gsub('"', "", mnodes$description)
# mnodes$title <- gsub('"', "", mnodes$title)
# mnodes$label <- gsub('"', "", mnodes$label)
# mnodes$group <- gsub('-',"_",mnodes$group)
# medges$label <- gsub(pattern = "(\\s|-)", "_", medges$label)
# medges$title <- gsub(pattern = "(\\s|-)", "_", medges$title)
#
#
#
# importPath = "C:/DEVEL/datasets/neo4j/relate-data/dbmss/dbms-c26455eb-9623-4cb7-8183-4bbfe013ece5/import"
# url = "localhost:7474"
# username = "neo4j"
# password = "devel4free"
#
# graph <- neo2R::startGraph(url = url,
#                            username = username,
#                            password = password, importPath = importPath)
#
# # try(neo2R::cypher(graph, 'CREATE INDEX ON :MITRE(type)'))
# # try(neo2R::cypher(graph, 'CREATE INDEX ON :MITRE(group)'))
# # # try(neo2R::cypher(graph, 'CREATE INDEX ON :MITRE(standard)'))
#
# # Add nodes
# neo2R::import_from_df(graph = graph,
#                       cql = paste0("MERGE (n:MITRE ",
#                                    "{id:row.id, label:row.label, group:row.group,
#                                    type:row.type, title:row.title, old:row.hidden,
#                                    mass:row.mass, name:row.standard, value:row.value})"),
#                       toImport = mnodes[, c("id","label","group", "type", "title",
#                                             "hidden", "mass", "standard", "value")])
#
# # Add edges
# etype <- unique(medges$label)
# for (e in etype) {
#   if (verbose) print(paste("Adding", e, "edges"))
#   neo2R::import_from_df(graph = graph,
#                         cql=neo2R::prepCql('MATCH (f:MITRE {id:row.from})',
#                                            'MATCH (t:MITRE {id:row.to})',
#                                            paste0('MERGE (f)-[r:', e,' {name:row.title, dashes:row.dashes}]->(t)')),
#                         toImport = medges[medges$label == e, c("title","from", "to", "dashes")])
# }
#
#
#
#
#
#
#
#
# #################
# to_neo4j <- function(mitrenet = getLatestDataSet()[["mitrenet"]],
#                      url = "localhost:7474",
#                      username = "neo4j",
#                      password = "devel4free",
#                      importPath = "~/neo4j/import",
#                      verbose = FALSE) {
#
#   mitrenet$nodes$name <- gsub('"',"",mitrenet$nodes$name)
#   mitrenet$nodes$label <- gsub('"',"",mitrenet$nodes$label)
#   mitrenet$nodes$group <- gsub('-',"_",mitrenet$nodes$group)
#   mitrenet$edges$label <- gsub(pattern = "(\\s|-)", "_", mitrenet$edges$label)
#   mitrenet$edges$title <- gsub(pattern = "(\\s|-)", "_", mitrenet$edges$title)
#
#   if (verbose) print(paste0("Connecting to graph database..."))
#   graph <- neo2R::startGraph(url = url,
#                              username = username,
#                              password = password, importPath = importPath)
#
#   if (verbose) print(paste0("Creating index for id, name and group"))
#   # try(neo2R::cypher(graph, 'CREATE INDEX ON :MITRE(id)'))
#   # try(neo2R::cypher(graph, 'CREATE INDEX ON :MITRE(name)'))
#   # try(neo2R::cypher(graph, 'CREATE INDEX ON :MITRE(group)'))
#
#   ntype <- unique(mitrenet$nodes$group)
#   tdef <- grep(pattern = "^(shield_.*|car)$", ntype, value = T)
#   toff <- grep(pattern = "^(attck_.*|capec)$", ntype, value = T)
#   for (g in ntype) {
#     if (verbose) print(paste("Adding", g, "nodes"))
#     t <- ifelse(test = g %in% tdef,
#                 yes = "DEFENSE",
#                 no = ifelse(test = g %in% toff,
#                             yes = "OFFENSE",
#                             no = "IT"))
#     neo2R::import_from_df(graph = graph,
#                           cql = paste0("MERGE (n:MITRE:", g, ":", t," {id:row.id, name:row.name, group:row.group, old:row.shadow })"),
#                           toImport = mitrenet$nodes[mitrenet$nodes$group == g, c("id","name","group", "shadow", "label")])
#   }
#
#   etype <- unique(mitrenet$edges$label)
#   for (e in etype) {
#     if (verbose) print(paste("Adding", e, "edges"))
#     neo2R::import_from_df(graph = graph,
#                           cql=neo2R::prepCql('MATCH (f:MITRE {id:row.from})',
#                                              'MATCH (t:MITRE {id:row.to})',
#                                              paste0('MERGE (f)-[r:', e,' {type:row.title, dashes:row.dashes}]->(t)')),
#                           toImport = mitrenet$edges[mitrenet$edges$label == e, c("title","from", "to", "dashes")])
#   }
# }
#
# getContextNet <- function(attck.tech = c("T1420", "T1204"),
#                           url = "localhost:7474",
#                           username = "neo4j",
#                           password = "devel4free",
#                           importPath = "~/neo4j/import",
#                           mitrenet = as_igraph()) {
#
#   graph <- neo2R::startGraph(url = "localhost:7474",
#                              username = "neo4j",
#                              password = "devel4free", importPath = importPath)
#
#
#   query <- paste0('MATCH p=(s:MITRE)-[*1..2]-(n:attck_techniques) WHERE n.name IN ["',
#                   paste0(attck.tech, collapse = "\", \""),
#                   '"] AND s.group STARTS WITH "shield" RETURN p')
#
#   df <- neo2R::cypher(graph, neo2R::prepCql(query),
#                       result="graph")
#
#   nnames <- sapply(df$nodes, function(x) x$properties$name)
#   names(nnames) <- sapply(df$nodes, function(x) x$properties$id)
#   ncntxt <- unique(c(as.numeric(unlist(df$paths)),
#                      sapply(df$nodes,
#                             function(x)
#                               as.numeric(x$properties$id))))
#   ig <- igraph::induced.subgraph(mitrenet, ncntxt)
#   Isolated = which(igraph::degree(ig)==0)
#   ig = igraph::delete.vertices(ig, Isolated)
#   g <- visNetwork::visIgraph(ig)
#
#   return(g)
# }
