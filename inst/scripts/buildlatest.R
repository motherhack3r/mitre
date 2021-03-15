#!/usr/bin/env Rscript
list.of.packages <- c("optparse", "mitre")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, repos = "http://cran.us.r-project.org", verbose = FALSE)

suppressPackageStartupMessages(library("optparse"))

option_list <- list(
  optparse::make_option(c("-v", "--verbose"), action="store_true", default=FALSE,
                        help="Print extra output [default FALSE]"),
  optparse::make_option(c("-o", "--output"), default="./mitre-datasets",
                        help = "Path where files will be saved [default \"%default\"]")
)

today <- as.character(Sys.Date())

opt <- optparse::parse_args(optparse::OptionParser(option_list=option_list))

if (dir.exists(opt$output)) {
  if (opt$verbose) print(paste0("START at ", as.character(Sys.time())))
  path.latest <- file.path(opt$output, "latest")
  path.beta <- file.path(opt$output, "beta")
  file.latest <- "mitre_latest.rds"
  file.beta <- paste0(today, "_mitre_v", packageVersion("mitre"), ".rds")

  if (!dir.exists(path.latest)) dir.create(path.latest)
  if (!dir.exists(path.beta)) dir.create(path.beta)

  library(mitre)

  mitredata <- mitre::parseRawData(verbose = opt$verbose)
  if (opt$verbose) print(paste0("Parsed at ", as.character(Sys.time())))
  if (opt$verbose) print(paste0("NODES: ", nrow(mitredata$mitrenet$nodes)))
  if (opt$verbose) print(paste0("EDGES: ", nrow(mitredata$mitrenet$edges)))

  file.save <- file.path(path.beta, file.beta)
  if (opt$verbose) print(paste0("Saving ", file.save))
  saveRDS(object = mitredata, file = file.save)

  file.save <- file.path(path.latest, file.latest)
  if (opt$verbose) print(paste0("Saving ", file.save))
  saveRDS(object = mitredata, file = file.save)

  if (opt$verbose) print(paste0("END at ", as.character(Sys.time())))

} else {
  warning(paste0("Path: ", opt$output, " does NOT exist."))
}

