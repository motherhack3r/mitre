library(jsonlite)
library(usethis)
library(dplyr, warn.conflicts = FALSE)

if (!dir.exists("data")) dir.create("data")

# Download Raw
if (!as.logical(length(list.files(path = "data-raw", pattern = "^cve-\\d+\\.json.gz$")))) {
  for (year in 2002:strftime(Sys.Date(), "%Y")) {
    utils::download.file(url = paste0("https://nvd.nist.gov/feeds/json/cve/1.1/nvdcve-1.1-", year,".json.gz"),
                         destfile = paste0("data-raw/cve-", year,".json.gz"), quiet = T)
  }
}


#####
# PARSER FUNCTIONS
GetNISTvulnsByYear <- function(year) {
  raw.cves <- jsonlite::fromJSON(paste0("data-raw/cve-", year,".json.gz"))

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
  cves$cvss3.av <- raw.cves$impact$baseMetricV3$cvssV3$attackVector
  cves$cvss3.ac <- raw.cves$impact$baseMetricV3$cvssV3$attackComplexity
  cves$cvss3.pr <- raw.cves$impact$baseMetricV3$cvssV3$privilegesRequired
  cves$cvss3.ui <- raw.cves$impact$baseMetricV3$cvssV3$userInteraction
  cves$cvss3.s <- raw.cves$impact$baseMetricV3$cvssV3$scope
  cves$cvss3.c <- raw.cves$impact$baseMetricV3$cvssV3$confidentialityImpact
  cves$cvss3.i <- raw.cves$impact$baseMetricV3$cvssV3$integrityImpact
  cves$cvss3.a <- raw.cves$impact$baseMetricV3$cvssV3$availabilityImpact
  cves$cvss3.score <- raw.cves$impact$baseMetricV3$cvssV3$baseScore
  cves$cvss3.severity <- raw.cves$impact$baseMetricV3$cvssV3$baseSeverity
  cves$cvss3.score.exploit <- raw.cves$impact$baseMetricV3$exploitabilityScore
  cves$cvss3.score.impact <- raw.cves$impact$baseMetricV3$impactScore

  cves$cvss2.vector <- raw.cves$impact$baseMetricV2$cvssV2$vectorString
  cves$cvss2.av <- raw.cves$impact$baseMetricV2$cvssV2$accessVector
  cves$cvss2.ac <- raw.cves$impact$baseMetricV2$cvssV2$accessComplexity
  cves$cvss2.au <- raw.cves$impact$baseMetricV2$cvssV2$authentication
  cves$cvss2.c <- raw.cves$impact$baseMetricV2$cvssV2$confidentialityImpact
  cves$cvss2.i <- raw.cves$impact$baseMetricV2$cvssV2$integrityImpact
  cves$cvss2.a <- raw.cves$impact$baseMetricV2$cvssV2$availabilityImpact
  cves$cvss2.score <- raw.cves$impact$baseMetricV2$cvssV2$baseScore
  cves$cvss2.severity <- raw.cves$impact$baseMetricV2$cvssV2$baseSeverity
  cves$cvss2.score.exploit <- raw.cves$impact$baseMetricV2$exploitabilityScore
  cves$cvss2.score.impact <- raw.cves$impact$baseMetricV2$impactScore
  cves$cvss2.getallprivilege <- raw.cves$impact$baseMetricV2$obtainAllPrivilege
  cves$cvss2.getusrprivilege <- raw.cves$impact$baseMetricV2$obtainUserPrivilege
  cves$cvss2.getothprivilege <- raw.cves$impact$baseMetricV2$obtainOtherPrivilege
  cves$cvss2.requsrinter <- raw.cves$impact$baseMetricV2$userInteractionRequired
  cves$published.date <- raw.cves$publishedDate
  cves$last.modified <- raw.cves$lastModifiedDate

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
                    cvss3.av = character(),
                    cvss3.ac = character(),
                    cvss3.pr = character(),
                    cvss3.ui = character(),
                    cvss3.s = character(),
                    cvss3.c = character(),
                    cvss3.i = character(),
                    cvss3.a = character(),
                    cvss3.score = numeric(),
                    cvss3.severity = character(),
                    cvss3.score.exploit = numeric(),
                    cvss3.score.impact = numeric(),
                    cvss2.vector = character(),
                    cvss2.av = character(),
                    cvss2.ac = character(),
                    cvss2.au = character(),
                    cvss2.c = character(),
                    cvss2.i = character(),
                    cvss2.a = character(),
                    cvss2.score = numeric(),
                    cvss2.score.exploit = numeric(),
                    cvss2.score.impact = numeric(),
                    cvss2.getallprivilege = logical(),
                    cvss2.getusrprivilege = logical(),
                    cvss2.getothprivilege = logical(),
                    cvss2.requsrinter = logical(),
                    published.date = character(),
                    last.modified = character(),
                    stringsAsFactors = FALSE)
  )
}

cves <- NewNISTEntry()
for (year in 2002:strftime(Sys.Date(), "%Y")) {
  cves <- dplyr::bind_rows(cves, GetNISTvulnsByYear(year))
}
cves$cvss3.av <- as.factor(cves$cvss3.av)
cves$cvss3.ac <- as.factor(cves$cvss3.ac)
cves$cvss3.pr <- as.factor(cves$cvss3.pr)
cves$cvss3.ui <- as.factor(cves$cvss3.ui)
cves$cvss3.s <- as.factor(cves$cvss3.s)
cves$cvss3.c <- as.factor(cves$cvss3.c)
cves$cvss3.i <- as.factor(cves$cvss3.i)
cves$cvss3.a <- as.factor(cves$cvss3.a)
cves$cvss3.severity <- as.factor(cves$cvss3.severity)
cves$cvss2.av <- as.factor(cves$cvss2.av)
cves$cvss2.ac <- as.factor(cves$cvss2.ac)
cves$cvss2.au <- as.factor(cves$cvss2.au)
cves$cvss2.c <- as.factor(cves$cvss2.c)
cves$cvss2.i <- as.factor(cves$cvss2.i)
cves$cvss2.a <- as.factor(cves$cvss2.a)
cves$published.date <- as.POSIXct.POSIXlt(strptime(cves$published.date, "%Y-%m-%dT%H:%MZ"))
cves$last.modified <- as.POSIXct.POSIXlt(strptime(cves$last.modified, "%Y-%m-%dT%H:%MZ"))

cve.nist <- cves
usethis::use_data(cve.nist, compress = "xz", overwrite = TRUE)
