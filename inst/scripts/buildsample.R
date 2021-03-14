# Schemas:
#  - CVE: https://csrc.nist.gov/schema/nvd/feed/1.1/nvd_cve_feed_json_1.1.schema
#  - CWE: https://cwe.mitre.org/data/xsd/cwe_schema_latest.xsd
#  - CPE: https://csrc.nist.gov/schema/cpe/2.3/cpe-dictionary_2.3.xsd
#         https://csrc.nist.gov/schema/cpe/2.3/cpe-dictionary-extension_2.3.xsd
#         https://csrc.nist.gov/schema/cpe/2.2/cpe-dictionary_2.2.xsd
#         https://csrc.nist.gov/schema/cpe/2.1/cpe-dictionary-metadata_0.2.xsd
#  - CAPEC: https://capec.mitre.org/data/xsd/ap_schema_latest.xsd
#           https://github.com/mitre/cti/blob/master/USAGE-CAPEC.md
#  - ATTCK: https://github.com/mitre/cti/blob/master/USAGE.md


# library(mitre)
# # library(dplyr)
#
# today <- as.character(Sys.Date())
# file.current <- paste0(today, "_sample_v", packageVersion("mitre"), ".rds")
# file.latest <- "sample.rds"
#
# print(paste0("START at ", as.character(Sys.time())))
#
# mitredata <- mitre::parseRawData(verbose = T)
#
# print(paste0("Parsed at ", as.character(Sys.time())))
# print(paste0("NODES: ", nrow(mitredata$mitrenet$nodes)))
# print(paste0("EDGES: ", nrow(mitredata$mitrenet$edges)))
#
#
# print("Removing deprecated nodes from network")
# mitrenet <- mitre::omitDeprecated(mitredata$mitrenet)
# print(paste0("NODES: ", nrow(mitrenet$nodes)))
# print(paste0("EDGES: ", nrow(mitrenet$edges)))
#
# edges <- mitrenet$edges
#
# # Select the most related CVEs with CWEs
# rels <- edges[grepl("^CVE-\\d+-\\d+$", edges$from),]
# rels <- rels[grepl("^CWE-\\d+$", rels$to),]
# top.rels <- names(tail(sort(table(rels$from)), 5))
#
# # Select the most related CAPEC with CVEs
# rels <- edges[grepl("^CAPEC-\\d+$", edges$from),]
# rels <- rels[grepl("^CVE-\\d+-\\d+$", rels$to),]
# top.rels <- c(top.rels, names(tail(sort(table(rels$from)), 5)))
#
# # Select the most related CPEs with CVEs
# rels <- edges[grepl("^cpe:2.+$", edges$from),]
# rels <- rels[grepl("^CVE-\\d+-\\d+$", rels$to),]
# top.rels <- c(top.rels, names(tail(sort(table(rels$from)), 5)))
#
# # Select the most related CWEs with CAPECs
# rels <- edges[grepl("^CWE-\\d+$", edges$from),]
# rels <- rels[grepl("^CAPEC-\\d+$", rels$to),]
# top.rels <- c(top.rels, names(tail(sort(table(rels$from)), 5)))
#
# # Select the most related ATTCK Techniques with CAPECs
# rels <- edges[grepl("^T\\d+$", edges$from),]
# rels <- rels[grepl("^CAPEC-\\d+$", rels$to),]
# top.rels <- c(top.rels, names(tail(sort(table(rels$from)), 5)))
#
# # Select the most related ATTCK Techniques with CVEs
# rels <- edges[grepl("^T\\d+$", edges$from),]
# rels <- rels[grepl("^CVE-\\d+-\\d+$", rels$to),]
# top.rels <- c(top.rels, names(tail(sort(table(rels$from)), 5)))
#
# # Select the most related SHIELD Techniques with ATTCK Techniques
# rels <- edges[grepl("^DTE\\d+$", edges$from),]
# rels <- rels[grepl("^T\\d+$", rels$to),]
# top.rels <- c(top.rels, names(tail(sort(table(rels$from)), 5)))
#
# # Select the most related SHIELD Techniques with ATTCK Tactics
# rels <- edges[grepl("^DTE\\d+$", edges$from),]
# rels <- rels[grepl("^TA\\d+$", rels$to),]
# top.rels <- c(top.rels, names(tail(sort(table(rels$from)), 5)))
#
#
# ed <- mitre::getNodeNeighbors(nodes = top.rels, mitrenet = mitrenet)
#
#
#
#
#
# ## A simple example with a couple of actors
# ## The typical case is that these tables are read in from files....
# actors <- data.frame(name=c("Alice", "Bob", "Cecil", "David",
#                             "Esmeralda"),
#                      age=c(48,33,45,34,21),
#                      gender=c("F","M","F","M","F"))
# relations <- data.frame(from=c("Bob", "Cecil", "Cecil", "David",
#                                "David", "Esmeralda"),
#                         to=c("Alice", "Bob", "Alice", "Alice", "Bob", "Alice"),
#                         same.dept=c(FALSE,FALSE,TRUE,FALSE,FALSE,TRUE),
#                         friendship=c(4,5,5,2,1,1), advice=c(4,5,5,4,2,3))
# g <- graph_from_data_frame(relations, directed=TRUE, vertices=actors)
# print(g, e=TRUE, v=TRUE)
#
# ## The opposite operation
# as_data_frame(g, what="vertices")
# as_data_frame(g, what="edges")
#
#
#
#
#
# library(igraph)
# mynodes <- mitrenet$nodes
# myedges <- mitrenet$edges
#
#
# myedges <- dplyr::left_join(myedges, mynodes[, c("id", "name")], c("from"="id"))
# myedges$from.id <- myedges$from
# myedges$from <- myedges$name
# myedges$name <- NULL
# myedges <- dplyr::left_join(myedges, mynodes[, c("id", "name")], c("to"="id"))
# myedges$to.id <- myedges$to
# myedges$to <- myedges$name
# myedges$name <- NULL
#
# myedges <- myedges[-unique(which(is.na(myedges$from) | is.na(myedges$to))), ]
#
# myedges <- myedges[((myedges$from.id %in% mynodes$id) & (myedges$to.id %in% mynodes$id)), ]
#
#
# mynodes <- mynodes[which(mynodes$id %in% unique(c(myedges$from.id, myedges$to.id))),]
# # mynodes <- mynodes[mynodes$id %in% unique(c(myedges$from.id, myedges$to.id)),]
#
#
#
# vertices <- mynodes[which(mynodes$id %in% unique(c(myedges$from.id, myedges$to.id))), ]
#
# g <- graph_from_data_frame(myedges, directed = T, vertices = mynodes)
# print(g, e=T, v=T)
#
#
#
# nodes$name <- nodes$id
# nodes$id <- seq_len(nrow(nodes))
# edges <- dplyr::left_join(edges, nodes[, c("id", "name")], c("from"="name"))
# edges$from <- edges$id
# edges$id <- NULL
# edges <- dplyr::left_join(edges, nodes[, c("id", "name")], c("to"="name"))
# edges$to <- edges$id
# edges$id <- NULL
#
#
#
# library(networkD3)
#
# data(MisLinks)
# data(MisNodes)
#
# networkD3::forceNetwork(Links = ed$edges, Nodes = ed$nodes,
#              Source = "from", Target = "to",
#              Value = "value", NodeID = "id",
#              Group = "group", opacity = 0.8)
#
#
#
#
#
# g <- visNetwork::visNetwork(nodes = ed$nodes, edges = ed$edges)
#
# ig <- visNetwork::ex
#
#
# # print(paste0("Saving in mitre-datasets/beta/", file.current))
# # saveRDS(object = mitredata, file = paste0("../../../datasets/mitre-datasets/beta/", file.current))
# # print(paste0("Saving in mitre-datasets/latest/", file.latest))
# # saveRDS(object = mitredata, file = paste0("../../../datasets/mitre-datasets/latest/", file.latest))
#
# print(paste0("END at ", as.character(Sys.time())))
