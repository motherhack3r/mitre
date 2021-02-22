library(mitre)

today <- as.character(Sys.Date())
file.current <- paste0(today, "_mitre_v", packageVersion("mitre"), ".rds")
file.latest <- "mitre_latest.rds"

print(paste0("START at ", as.character(Sys.time())))

mitredata <- mitre::parseRawData(verbose = TRUE)

print(paste0("Parsed at ", as.character(Sys.time())))
print(paste0("NODES: ", nrow(mitredata$mitrenet$nodes)))
print(paste0("EDGES: ", nrow(mitredata$mitrenet$edges)))

print(paste0("Saving in mitre-datasets/beta/", file.current))
saveRDS(object = mitredata, file = paste0("../../../datasets/mitre-datasets/beta/", file.current))
print(paste0("Saving in mitre-datasets/latest/", file.latest))
saveRDS(object = mitredata, file = paste0("../../../datasets/mitre-datasets/latest/", file.latest))

print(paste0("END at ", as.character(Sys.time())))
