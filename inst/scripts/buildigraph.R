mitredata <- readRDS("C:/DEVEL/datasets/mitre-datasets/latest/mitre_latest.rds")

mitrenet <- mitredata$mitrenet

nodes <- mitrenet$nodes

nodes <- nodes[which(!nodes$shadow),]
