library(mitre)

today <- as.character(Sys.Date())
path.latest <- file.path("c:","DEVEL","datasets","mitre-datasets")
file.current <- paste0(today, "_mitre_v", packageVersion("mitre"), ".rds")
file.latest <- "mitre_latest.rds"
path.datasets <- "C:/DEVEL/datasets/mitre-datasets/"

print(paste0("START at ", as.character(Sys.time())))

mitredata <- mitre::parseRawData(verbose = TRUE)

print(paste0("Parsed at ", as.character(Sys.time())))
print(paste0("NODES: ", nrow(mitredata$mitrenet$nodes)))
print(paste0("EDGES: ", nrow(mitredata$mitrenet$edges)))

file.save <- file.path(path.latest, "beta", file.current)
print(paste0("Saving ", file.save))
saveRDS(object = mitredata, file = file.save)
file.save <- file.path(path.latest, "latest", file.latest)
print(paste0("Saving ", file.save))
saveRDS(object = mitredata, file = file.save)

print(paste0("END at ", as.character(Sys.time())))
