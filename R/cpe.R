#' ETL process that download current CPE definitions and return a list with a
#' data frame for CPE objects. The list also contains a list with
#' CPE objects as nodes and all relations as edges.
#'
#' @param verbose Default set as FALSE
#' @param savepath file path to store CPE dictionary
#'
#' @return list of data frames
getCPEData <- function(verbose = FALSE, savepath = "data-raw/official-cpe-dictionary_v2.3.json") {
  if (!file.exists(savepath)) {
    savepath <- "data-raw/official-cpe-dictionary_v2.3.xml"
    if (!file.exists(savepath)) {
      # download from gitrepo
    } else {
      if (verbose) print("[.][CPE] Parsing XML raw data...")
      cpes <- parseCPExml(verbose, savepath)
    }
  } else {
    if (verbose) print("[.][CPE] Parsing JSON raw data...")
    cpes <- parseCPEjson(verbose, savepath)
  }
}

downloadCPE_API <- function(verbose = FALSE, savepath = "data-raw/official-cpe-dictionary_v2.3.json", pageini = 600000) {
  baseURL <- "https://services.nvd.nist.gov/rest/json/cpes/1.0?addOns=cves"
  seed <- sample(5:49, 1)
  maxitem <- seed * 100

  rawdf <- data.frame(stringsAsFactors = FALSE)

  if (verbose) print("[.][CPE] Get CPE dictionary via NIST API...")
  repeat {
    ts <- round(seed/12, 0) + 1
    if (verbose) print(paste0("[.]      - Downloading ", as.character(maxitem), " records..."))
    currentURL <- paste0(baseURL,"&startIndex=", as.character(pageini),
                         "&resultsPerPage=", as.character(maxitem))
    repeat {
      r <- httr::RETRY("GET", currentURL, quiet = T, times = 10,
                       pause_base = ts, pause_min = 10, pause_cap = 120)
      if (r$status_code == 200) break
      if (verbose) print(paste0("[x] HTTP ERROR ", r$status_code, ": "))
      ts <- round((ts*1.5), 0)
      if (verbose) print(paste0("[!] Waitting ", as.character(ts), " seconds before next try..."))
      Sys.sleep(ts)
    }
    resp <- jsonlite::fromJSON(httr::content(r, as = "text", encoding = "UTF-8"))
    resp$result$cpes$refs <- sapply(resp$result$cpes$refs, jsonlite::toJSON)
    resp$result$cpes$titles <- sapply(resp$result$cpes$titles, jsonlite::toJSON)
    resp$result$cpes$vulnerabilities <- sapply(resp$result$cpes$vulnerabilities, jsonlite::toJSON)
    resp$result$cpes$deprecatedBy <- NULL
    rawdf <- dplyr::bind_rows(rawdf, resp$result$cpes)

    ntotal <- resp$totalResults
    pageini <- resp$startIndex + resp$resultsPerPage
    ts <- seed * sample(3:7, 1)
    seed <- sample(5:40, 1)
    maxitem <- seed * 100
    if (verbose) print(paste0("[.]      - ", as.character(pageini), "/", ntotal,
                              " ok (", round(pageini/ntotal*100, 2), "%)"))
    # Sys.sleep(ts)
    if (pageini >= ntotal) break
  }
  if (verbose) print(paste0("[.]     - ", as.character(ntotal), "items..."))
  if (verbose) print(paste0("[.][CPE] Saved as : ", savepath))
  jsonlite::write_json(rawdf, savepath)
}

parseCPExml <- function(verbose = F, savepath = "data-raw/official-cpe-dictionary_v2.3.xml") {
  if (verbose) print("[.][CPE] Indexing XML and namespace schemas...")
  doc <- xml2::read_xml(savepath)

  if (verbose) print("[.][CPE] Parsing product title and cpe codes 2.x...")
  cpes <- data.frame(title = xml2::xml_text(xml2::xml_find_all(doc, "//*[cpe-23:cpe23-item]/*[@xml:lang='en-US'][1]")),
                     cpe.22 = xml2::xml_text(xml2::xml_find_all(doc, "//*[cpe-23:cpe23-item]/@name")),
                     cpe.23 = xml2::xml_text(xml2::xml_find_all(doc, "//cpe-23:cpe23-item/@name")),
                     stringsAsFactors = F)
  nodes <- xml2::xml_find_all(doc, ".//cpe-23:cpe23-item")

  if (verbose) print("[.][CPE] Flattening nested references. Be patient, it needs XPATH query optimization...")
  cpes$refs <- unlist(lapply(xml2::xml_find_first(nodes, ".//ancestor::d1:cpe-item/d1:references"),
                             function(x) {
                               z <- lapply(xml2::as_list(x), function(y) as.character(attributes(y)))
                               names(z) <- as.character(sapply(xml2::as_list(x), as.character))
                               jsonlite::toJSON(z, pretty = T)
                             }))

  if (verbose) print("[.][CPE] Searching for deprecated nodes...")
  cpes$deprecated <- !is.na(as.logical(xml2::xml_attr(xml2::xml_find_first(nodes,
                                                                           ".//ancestor::d1:cpe-item"),
                                                      "deprecated")))

  if (verbose) print("[.][CPE] Creating factor columns from CPE ids ...")
  new.cols <- c("std", "std.v", "part", "vendor", "product",
                "version", "update", "edition", "language", "sw_edition",
                "target_sw", "target_hw", "other")
  cpes$cpe.23 <- stringr::str_replace_all(cpes$cpe.23, "\\\\:", ";")
  cpes <- tidyr::separate(data = cpes, col = "cpe.23", into = new.cols, sep = ":", remove = F)
  cpes <- dplyr::select(.data = cpes, -"std", -"std.v")
  cpes$vendor <- as.factor(cpes$vendor)
  cpes$product <- as.factor(cpes$product)
  cpes$language <- as.factor(cpes$language)
  cpes$sw_edition <- as.factor(cpes$sw_edition)
  cpes$target_sw <- as.factor(cpes$target_sw)
  cpes$target_hw <- as.factor(cpes$target_hw)

  if (verbose) print("[.][CPE] Extracting CVE references...")
  cpe2cve <- lapply(cpes$refs, function(x) stringr::str_extract_all(x, "CVE-\\d+-\\d+"))
  cpe2cve <- sapply(cpe2cve, function(x) ifelse(identical(x[[1]], character(0)), NA, x[[1]]))
  cpe2cve <- data.frame(from = cpes$cpe.23, to = cpe2cve, stringsAsFactors = FALSE)
  cpe2cve <- cpe2cve[stats::complete.cases(cpe2cve), ]

  if (verbose) print("[.][CPE] Building CPE network ...")
  cpe2cve$team <- rep("SYSADMIN", nrow(cpe2cve))
  cpe2cve$label <- rep("is vulnerable", nrow(cpe2cve))
  cpe2cve$arrows <- rep("to", nrow(cpe2cve))
  cpe2cve$title <- rep("vulnerable", nrow(cpe2cve))
  cpe2cve$dashes <- rep(FALSE, nrow(cpe2cve))

  cpenodes <- cpes[, c("cpe.23", "title","cpe.22", "deprecated")]
  names(cpenodes) <- c("id", "label", "title", "shadow")
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

parseCPEjson <- function(verbose = F, savepath = "data-raw/official-cpe-dictionary_v2.3.json") {
  df <- jsonlite::read_json(savepath, simplifyVector = T)
  df$title <- as.character(sapply(df$titles,
                                  function(x) {
                                    y <- jsonlite::fromJSON(x)
                                    y[y$lang == "en_US", "title"]
                                  }))
  df$titles <- NULL
  df$lastModifiedDate <- as.POSIXct.POSIXlt(strptime(df$lastModifiedDate, "%Y-%m-%dT%H:%MZ"))

  new.cols <- c("std", "std.v", "part", "vendor", "product",
                "version", "update", "edition", "language", "sw_edition",
                "target_sw", "target_hw", "other")
  cpes <- df
  cpes$cpe23Uri <- stringr::str_replace_all(cpes$cpe23Uri, "\\\\:", "_")
  cpes <- tidyr::separate(data = cpes, col = "cpe23Uri", into = new.cols, sep = ":", remove = F)
  cpes <- dplyr::select(.data = cpes, -"std", -"std.v")
  cpes$part <- as.factor(cpes$part)
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

