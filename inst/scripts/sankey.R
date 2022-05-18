
URL <- paste0(
  "https://cdn.rawgit.com/christophergandrud/networkD3/",
  "master/JSONdata/energy.json")
Energy <- jsonlite::fromJSON(URL)

library(dplyr)

kk <- left_join(x = cygraph$edges,
                y = cygraph$nodes %>% select(id, group),
                by = c("from"="id"))
kk$from_group <- kk$group
kk$group <- NULL
kk <- left_join(x = kk,
                y = cygraph$nodes %>% select(id, group),
                by = c("to"="id"))
kk$to_group <- kk$group
kk$group <- NULL

kkk <- kk %>%
  select(from_group, to_group) %>%
  group_by(from_group, to_group) %>%
  summarise(value = n())
kkk$value <- as.numeric(kkk$value)

lite_nodes = data.frame(name = unique(c(kkk$from_group, kkk$to_group)))
lite_nodes$id = as.integer(row.names(lite_nodes)) - 1

kkk <- kkk %>% left_join(lite_nodes, by = c("from_group" = "name"))
kkk$source <- kkk$id
kkk$id <- NULL

kkk <- kkk %>% left_join(lite_nodes, by = c("to_group" = "name"))
kkk$target <- kkk$id
kkk$id <- NULL

lite_links <-  kkk %>% ungroup() %>% select(source, target, value) %>% as.data.frame()
lite_nodes <- lite_nodes %>% select(name) %>% as.data.frame()

# remove self-relations
lite_links <- lite_links[-which(lite_links$source == lite_links$target),]

# join bidirectional
ll <- data.frame(head(lite_links, 0))
for(r in 1:nrow(lite_links)) {
  # if (any((lite_links$source == lite_links$target[r]) & (lite_links$target == lite_links$source[r]))) {
  #   lite_links$value <- lite_links$value + lite_links[(lite_links$source == lite_links$target[r]) & (lite_links$target == lite_links$source[r]), "value"]
  #   lite_links$value[((lite_links$source == lite_links$target[r]) & (lite_links$target == lite_links$source[r]))] <- 0
  # }
  e <- data.frame(source = 0, target = 0, value = 0)
  ll <- bind_rows(ll, e)
}

networkD3::sankeyNetwork(Links = lite_links, Nodes = lite_nodes,
                         Source = "source", Target = "target", Value = "value",
                         units = "TWh", fontSize = 12, nodeWidth = 30)

