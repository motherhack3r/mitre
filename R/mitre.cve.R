

#' Load CVE data frame from local file or download latest
#'
#' @param local_path path to RDS file. NA value implies remote TRUE
#' @param remote logical
#' @param keep_deprecated logical
#' @param allcols logical
#' @param verbose logical
#'
#' @return data.frame
cve_latest_data <- function(local_path = NA, remote = F, allcols = F, verbose = F) {
  if (is.na(local_path) | remote) {
    local_path <- tempfile(fileext = ".rds")
    download.file(url = "https://github.com/motherhack3r/mitre-datasets/raw/master/latest/simple/cve.rds",
                  destfile = local_path, quiet = !verbose)
  }
  cves <- readRDS(local_path)

  if (!allcols) {
    cves <- cves[, c("cve.id", "description", "problem.type", "vulnerable.configuration", "cvss3.score", "published.date")]
  }

  return(cves)
}

