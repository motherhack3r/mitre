if(any(grepl("package:RJSONIO", search()))) detach("package:RJSONIO")
library(jsonlite)
library(usethis)
library(dplyr, warn.conflicts = FALSE)

if (!dir.exists("data")) dir.create("data")
if (!dir.exists("data-raw/cve")) dir.create("data-raw/cve")

from_year <-  2002

# Download Raw
if (!as.logical(length(list.files(path = "data-raw/cve", pattern = "^cve-\\d+\\.json.gz$")))) {
  for (year in 2002:strftime(Sys.Date(), "%Y")) {
    utils::download.file(url = paste0("https://nvd.nist.gov/feeds/json/cve/1.1/nvdcve-1.1-", year,".json.gz"),
                         destfile = paste0("data-raw/cve/cve-", year,".json.gz"), quiet = T)
  }
}


#####
# PARSER FUNCTIONS
GetNISTvulnsByYear <- function(year = 2020, verbose = FALSE) {
  if (verbose) print(paste0("[PARSE][CVE] Parsing year ", year))
  raw.cves <- jsonlite::fromJSON(paste0("data-raw/cve/cve-", year,".json.gz"))

  raw.cves <- raw.cves$CVE_Items
  cves <- data.frame(cve.id = raw.cves$cve$CVE_data_meta$ID,
                     stringsAsFactors = F)
  cves$description <- unlist(lapply(raw.cves$cve$description$description_data,
                                    function(x) x[["value"]][1]))
  cves$problem.type <- unlist(lapply(raw.cves$cve$problemtype$problemtype_data,
                                     function(x)
                                       jsonlite::toJSON(x[[1]][[1]]$value)))
  cves$vulnerable.configuration <- unlist(lapply(raw.cves$configurations$nodes,
                                                 function(x)
                                                   jsonlite::toJSON(x)))
  cves$references <- unlist(lapply(raw.cves$cve$references$reference_data,
                                   function(x)
                                     jsonlite::toJSON(x)))

  cves$cvss3.vector <- raw.cves$impact$baseMetricV3$cvssV3$vectorString
  cves$cvss3.score <- raw.cves$impact$baseMetricV3$cvssV3$baseScore
  cves$cvss3.severity <- raw.cves$impact$baseMetricV3$cvssV3$baseSeverity
  cves$cvss2.vector <- raw.cves$impact$baseMetricV2$cvssV2$vectorString
  cves$cvss2.score <- raw.cves$impact$baseMetricV2$cvssV2$baseScore
  cves$published.date <- raw.cves$publishedDate
  cves$last.modified <- raw.cves$lastModifiedDate

  if (verbose) print(paste0("[PARSE][CVE] ", nrow(cves)," vulnerabilities from year ", year))
  return(cves)
}

NewNISTEntry <- function() {
  return(data.frame(cve.id = character(),
                    # affects = character(),
                    problem.type = character(),
                    references = character(),
                    description = character(),
                    vulnerable.configuration = character(),
                    cvss3.vector = character(),
                    cvss3.score = numeric(),
                    cvss3.severity = character(),
                    cvss2.vector = character(),
                    cvss2.score = numeric(),
                    published.date = character(),
                    last.modified = character(),
                    stringsAsFactors = FALSE)
  )
}

cves <- NewNISTEntry()
for (year in from_year:strftime(Sys.Date(), "%Y")) {
  cves <- dplyr::bind_rows(cves, GetNISTvulnsByYear(year, T))
}
cves$cvss3.severity <- as.factor(cves$cvss3.severity)
cves$published.date <- as.POSIXct.POSIXlt(strptime(cves$published.date, "%Y-%m-%dT%H:%MZ"))
cves$last.modified <- as.POSIXct.POSIXlt(strptime(cves$last.modified, "%Y-%m-%dT%H:%MZ"))

cve.nist <- cves

usethis::use_data(cve.nist, compress = "xz", overwrite = TRUE)

rm(cves, year, from_year, GetNISTvulnsByYear, NewNISTEntry)
