

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

cve_flatten_cpematch_or <- function(cpe_match = "", cve_id = "", numid = 1, parentid = 1) {
  df <- dplyr::bind_rows(lapply(cpe_match,
                                function(x) {
                                  y <- data.frame(vulnerable = x$vulnerable,
                                                  cpe23Uri = x$cpe23Uri,
                                                  versionStartExcluding = ifelse("versionStartExcluding" %in% names(x), x$versionStartExcluding, as.character(NA)),
                                                  versionStartIncluding = ifelse("versionStartIncluding" %in% names(x), x$versionStartIncluding, as.character(NA)),
                                                  versionEndExcluding = ifelse("versionEndExcluding" %in% names(x), x$versionEndExcluding, as.character(NA)),
                                                  versionEndIncluding = ifelse("versionEndIncluding" %in% names(x), x$versionEndIncluding, as.character(NA)),
                                                  cpe_name = ifelse("cpe_name" %in% names(x),
                                                                    ifelse(length(x$cpe_name) > 0,
                                                                           jsonlite::toJSON(x$cpe_name),
                                                                           as.character(NA)),
                                                                    as.character(NA)),
                                                  stringsAsFactors = FALSE)
                                  y$vc_id <- paste("vulnconf", cve_id, parentid, paste0("OR", numid), sep = ":")
                                  numid <<- numid + 1
                                  y
                                }))
  df$cve <- rep(cve_id, nrow(df))
  return(df)
}

cve_flatten_vulnconf <- function(vulnconf = "", cve_id = "", idnum = 1) {
  vcs <- RJSONIO::fromJSON(vulnconf)
  df <- dplyr::bind_rows(lapply(vcs,
                                function(vc) {
                                  vc.op <- vc$operator
                                  if (vc.op == "AND") {
                                    idand <- 1
                                    vc.cm <- lapply(vc$children,
                                                    function(x) {
                                                      y <- cve_flatten_cpematch_or(x$cpe_match, cve_id, parentid = idand)
                                                      idand <<- idand + 1
                                                      y
                                                    })
                                    vc.cm <- dplyr::bind_rows(vc.cm)
                                    vc.cm$vc_id <- paste(vc.cm$vc_id, paste0("AND", idnum), sep = ":")
                                  } else if (vc.op == "OR") {
                                    vc.cm <- cve_flatten_cpematch_or(vc$cpe_match, cve_id, parentid = idnum)
                                  } else {
                                    vc.cm <- data.frame()
                                  }
                                  idnum <<- idnum + 1
                                  vc.cm
                                }))
  return(df)
}

