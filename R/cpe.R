downloadCPEData <- function(verbose = FALSE, savepath = "data-raw/official-cpe-dictionary_v2.3.json") {
  baseURL <- "https://services.nvd.nist.gov/rest/json/cpes/1.0?addOns=cves"
  pageini <- 39000
  maxitem <- 3000

  rawdf <- data.frame(stringsAsFactors = FALSE)

  if (verbose) print("[.][CPE] Downloading CPE dictionary via NIST API...")
  repeat {
    currentURL <- paste0(baseURL,"&startIndex=", as.character(pageini),
                         "&resultsPerPage=", as.character(maxitem))
    resp <- jsonlite::fromJSON(currentURL)
    ntotal <- resp$totalResults
    pageini <- resp$startIndex + resp$resultsPerPage
    if (verbose) print(paste0("[.]     - ", as.character(pageini), "items..."))
    resp$result$cpes$refs <- sapply(resp$result$cpes$refs, jsonlite::toJSON)
    resp$result$cpes$titles <- sapply(resp$result$cpes$titles, jsonlite::toJSON)
    resp$result$cpes$vulnerabilities <- sapply(resp$result$cpes$vulnerabilities, jsonlite::toJSON)
    resp$result$cpes$deprecatedBy <- NULL
    rawdf <- dplyr::bind_rows(rawdf, resp$result$cpes)
    Sys.sleep(30)
    if (pageini >= ntotal) break
  }
  if (verbose) print(paste0("[.]     - ", as.character(ntotal), "items..."))
  if (verbose) print(paste0("[.][CPE] Saved as : ", savepath))
  jsonlite::write_json(rawdf, savepath)
}

#' ETL process that download current CPE definitions and return a list with a
#' data frame for CPE objects. The list also contains a list with
#' CPE objects as nodes and all relations as edges.
#'
#' @param verbose Default set as FALSE
#' @param savepath file path to store CPE dictionary
#'
#' @return list of data frames
getCPEData <- function(verbose = FALSE, savepath = "data-raw/official-cpe-dictionary_v2.3.json") {
  downloadCPEData(verbose)
  if (verbose) print("[.][CPE] Parsing dataset...")
  df <- jsonlite::read_json(savepath, simplifyVector = T)
  df$title <- as.character(sapply(df$titles, function(x) jsonlite::fromJSON(x)$title[1]))
  df$titles <- NULL
  df$lastModifiedDate <- as.POSIXct.POSIXlt(strptime(df$lastModifiedDate, "%Y-%m-%dT%H:%MZ"))

  new.cols <- c("std", "std.v", "part", "vendor", "product",
                "version", "update", "edition", "language", "sw_edition",
                "target_sw", "target_hw", "other")
  cpes <- df
  cpes$cpe23Uri <- stringr::str_replace_all(cpes$cpe23Uri, "\\\\:", "_")
  cpes <- tidyr::separate(data = cpes, col = "cpe23Uri", into = new.cols, sep = ":", remove = F)
  cpes <- dplyr::select(.data = cpes, -"std", -"std.v")
  cpes$vendor <- as.factor(cpes$vendor)
  cpes$product <- as.factor(cpes$product)
  cpes$language <- as.factor(cpes$language)
  cpes$sw_edition <- as.factor(cpes$sw_edition)
  cpes$target_sw <- as.factor(cpes$target_sw)
  cpes$target_hw <- as.factor(cpes$target_hw)
  cpes <- cpes[,c("title", "cpe23Uri", "part", "vendor", "product", "version",
                  "update", "edition", "language", "sw_edition", "target_sw", "target_hw", "other",
                  "refs", "deprecated", "lastModifiedDate", "vulnerabilities")]

  if (verbose) print("[.][CPE] Extracting CVE references...")


  cpe2cve <- lapply(cpes$vulnerabilities, function(x) stringr::str_extract_all(x, "CVE-\\d+-\\d+"))
  cpe2cve <- sapply(cpe2cve, function(x) ifelse(identical(x[[1]], character(0)), NA, x[[1]]))
  cpe2cve <- data.frame(from = cpes$cpe23Uri, to = cpe2cve, stringsAsFactors = FALSE)
  cpe2cve <- cpe2cve[stats::complete.cases(cpe2cve), ]

  if (verbose) print("[.][CPE] Building CPE network ...")
  cpe2cve$team <- rep("SYSADMIN", nrow(cpe2cve))
  cpe2cve$label <- rep("vulnerable", nrow(cpe2cve))
  cpe2cve$arrows <- rep("to", nrow(cpe2cve))
  cpe2cve$title <- rep("vulnerable", nrow(cpe2cve))
  cpe2cve$dashes <- rep(FALSE, nrow(cpe2cve))

  cpenodes <- cpes[, c("cpe23Uri", "title", "deprecated")]
  names(cpenodes) <- c("id", "title", "shadow")
  cpenodes$label <- cpenodes$title
  cpenodes$group <- rep("cpe", nrow(cpenodes))
  cpenodes$value <- rep(3, nrow(cpenodes))
  cpenodes$shape <- rep("box", nrow(cpenodes))
  cpenodes$color <- rep("honeydew", nrow(cpenodes))
  cpenodes$team <- rep("SYSADMIN", nrow(cpenodes))

  cpes <- list(cpes = cpes,
               cpenet = list(nodes = cpenodes,
                             edges = cpe2cve))

  return(cpes)
}

