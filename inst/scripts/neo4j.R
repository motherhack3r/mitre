to_neo4j <- function(mitrenet = getLatestDataSet()[["mitrenet"]],
                     url = "localhost:7474",
                     username = "neo4j",
                     password = "bullshit",
                     importPath = "~/neo4j/import",
                     verbose = FALSE) {

  mitrenet$nodes$description <- gsub('"',"",mitrenet$nodes$description)
  mitrenet$nodes$description <- gsub("'","",mitrenet$nodes$description)

  if (verbose) print(paste0("Connecting to graph database..."))
  graph <- neo2R::startGraph(url = url,
                             username = username,
                             password = password, importPath = importPath)

  if (verbose) print(paste0("Creating index for id, name and group"))
  # tryCatch(neo2R::cypher(graph, 'CREATE INDEX ON :MITRE(id)'), print("Index on id"))
  # tryCatch(neo2R::cypher(graph, 'CREATE INDEX ON :MITRE(name)'), print("Index on name"))
  # tryCatch(neo2R::cypher(graph, 'CREATE INDEX ON :MITRE(group)'), print("Index on group"))
  # tryCatch(neo2R::cypher(graph, 'CREATE INDEX ON :MITRE(type)'), print("Index on type"))

  # try(neo2R::cypher(graph, 'CREATE INDEX ON :MITRE(id)'), silent = T)
  # try(neo2R::cypher(graph, 'CREATE INDEX ON :MITRE(name)'), silent = T)
  # try(neo2R::cypher(graph, 'CREATE INDEX ON :MITRE(group)'), silent = T)
  # try(neo2R::cypher(graph, 'CREATE INDEX ON :MITRE(type)'), silent = T)

  try(neo2R::cypher(graph, 'CREATE INDEX IF NOT EXISTS FOR (m:MITRE) ON (m.id)'))
  try(neo2R::cypher(graph, 'CREATE INDEX IF NOT EXISTS FOR (m:MITRE) ON (m.name)'))
  try(neo2R::cypher(graph, 'CREATE INDEX IF NOT EXISTS FOR (m:MITRE) ON (m.group)'))
  try(neo2R::cypher(graph, 'CREATE INDEX IF NOT EXISTS FOR (m:MITRE) ON (m.type)'))

  ntype <- unique(mitrenet$nodes$group)
  tdef <- grep(pattern = "^(engage|shield|car)$", ntype, value = T)
  toff <- grep(pattern = "^(attck|capec)$", ntype, value = T)
  for (g in ntype) {
    if (verbose) print(paste("Adding", g, "nodes"))
    t <- ifelse(test = g %in% tdef,
                yes = "DEFENSE",
                no = ifelse(test = g %in% toff,
                            yes = "OFFENSE",
                            no = "IT"))
    subg <- mitrenet$nodes[mitrenet$nodes$group == g, c("id","standard", "title", "group", "type", "description", "mass", "hidden", "label")]
    for (tp in unique(subg$type)) {
      if (verbose) print(paste("Adding", tp, "nodes"))
      neo2R::import_from_df(graph = graph,
                            cql = paste0("MERGE (n:MITRE:", t, ":", g, ":", tp," {id:row.id, name:row.standard, group:row.group, type:row.type, old:row.hidden, title:row.title, descr:row.description })"),
                            toImport = subg[subg$type == tp, ])
    }
  }

  etype <- unique(mitrenet$edges$label)
  for (e in etype) {
    if (verbose) print(paste("Adding", e, "edges"))
    neo2R::import_from_df(graph = graph,
                          cql=neo2R::prepCql('MATCH (f:MITRE {id:row.from})',
                                             'MATCH (t:MITRE {id:row.to})',
                                             paste0('MERGE (f)-[r:', e,' {type:row.title, dashes:row.dashes}]->(t)')),
                          toImport = mitrenet$edges[mitrenet$edges$label == e, c("title","from", "to", "dashes")])
  }
}

getContextNet <- function(attck.tech = c("T1420", "T1204"),
                          url = "localhost:7474",
                          username = "neo4j",
                          password = "bullshit",
                          importPath = "~/neo4j/import",
                          mitrenet = as_igraph()) {

  graph <- neo2R::startGraph(url = "localhost:7474",
                             username = "neo4j",
                             password = "devel4free", importPath = importPath)


  query <- paste0('MATCH p=(s:MITRE)-[*1..2]-(n:attck_techniques) WHERE n.name IN ["',
                  paste0(attck.tech, collapse = "\", \""),
                  '"] AND s.group STARTS WITH "shield" RETURN p')

  df <- neo2R::cypher(graph, neo2R::prepCql(query),
                      result="graph")

  nnames <- sapply(df$nodes, function(x) x$properties$name)
  names(nnames) <- sapply(df$nodes, function(x) x$properties$id)
  ncntxt <- unique(c(as.numeric(unlist(df$paths)),
    sapply(df$nodes,
           function(x)
             as.numeric(x$properties$id))))
  ig <- igraph::induced.subgraph(mitrenet, ncntxt)
  Isolated = which(igraph::degree(ig)==0)
  ig = igraph::delete.vertices(ig, Isolated)
  g <- visNetwork::visIgraph(ig)

  return(g)
}
