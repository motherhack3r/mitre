# mitredata <- readRDS("C:/DEVEL/datasets/mitre-datasets/latest/mitre_latest.rds")
mitredata <- mitre::getLatestDataSet(T)

mitrenet <- mitre::omitDeprecated(mitredata$mitrenet)
nodes <- mitrenet$nodes
edges <- mitrenet$edges

edges <- dplyr::left_join(edges, nodes[, c("id", "name")], by = c("from"="id"))
edges$src <- edges$name
edges$name <- NULL
edges <- dplyr::left_join(edges, nodes[, c("id", "name")], by = c("to"="id"))
edges$dst <- edges$name
edges$name <- NULL

kk <- nodes[!(nodes$id %in% unique(c(edges$from, edges$to))), ]
kk <- edges[is.na(edges$to) | is.na(edges$from),]
kk <- edges[is.na(edges$src) | is.na(edges$dst),]
kk <- nodes[which(nodes$shadow),]

library(igraph)
library(networkD3)
# library(visNetwork)

ig <- graph_from_data_frame(edges, directed = T, vertices = nodes)

# Ref: https://stackoverflow.com/questions/37665651/r-igraph-matching-edges-between-two-graphs


# Select the most related CVEs with CWEs
rels <- edges[grepl("^CVE-\\d+-\\d+$", edges$src),]
rels <- rels[grepl("^CWE-\\d+$", rels$dst),]
top.rels <- names(tail(sort(table(rels$src)), 5))

# Select the most related CAPEC with CVEs
rels <- edges[grepl("^CAPEC-\\d+$", edges$src),]
rels <- rels[grepl("^CVE-\\d+-\\d+$", rels$dst),]
top.rels <- c(top.rels, names(tail(sort(table(rels$src)), 5)))

# Select the most related CPEs with CVEs
rels <- edges[grepl("^cpe:2.+$", edges$src),]
rels <- rels[grepl("^CVE-\\d+-\\d+$", rels$dst),]
top.rels <- c(top.rels, names(tail(sort(table(rels$src)), 5)))

# Select the most related CWEs with CAPECs
rels <- edges[grepl("^CWE-\\d+$", edges$src),]
rels <- rels[grepl("^CAPEC-\\d+$", rels$dst),]
top.rels <- c(top.rels, names(tail(sort(table(rels$src)), 5)))

# Select the most related ATTCK Techniques with CAPECs
rels <- edges[grepl("^T\\d+$", edges$src),]
rels <- rels[grepl("^CAPEC-\\d+$", rels$dst),]
top.rels <- c(top.rels, names(tail(sort(table(rels$src)), 5)))

# Select the most related ATTCK Techniques with CVEs
rels <- edges[grepl("^T\\d+$", edges$src),]
rels <- rels[grepl("^CVE-\\d+-\\d+$", rels$dst),]
top.rels <- c(top.rels, names(tail(sort(table(rels$src)), 5)))

# Select the most related SHIELD Techniques with ATTCK Techniques
rels <- edges[grepl("^DTE\\d+$", edges$src),]
rels <- rels[grepl("^T\\d+$", rels$dst),]
top.rels <- c(top.rels, names(tail(sort(table(rels$src)), 5)))

# Select the most related SHIELD Techniques with ATTCK Tactics
rels <- edges[grepl("^DTE\\d+$", edges$src),]
rels <- rels[grepl("^TA\\d+$", rels$dst),]
top.rels <- c(top.rels, names(tail(sort(table(rels$src)), 5)))


###########
#### CREATE SAMPLE

igv <- largest_cliques(ig)
igs <- unique(as.character(sapply(igv, as_ids)))

needNodes <- unique(c(igs, top.rels))
subGr <- induced_subgraph(ig, vids=needNodes)

# Convert to object suitable for networkD3
mitre_d3 <- igraph_to_networkD3(subGr)
mitre_d3$nodes <- dplyr::left_join(mitre_d3$nodes, nodes[, c("name", "group")], "name")

# Create force directed network plot
f <- forceNetwork(Links = mitre_d3$links, Nodes = mitre_d3$nodes,
                  Source = 'source', Target = 'target',
                  NodeID = 'name', Group = 'group', zoom = TRUE)
f
