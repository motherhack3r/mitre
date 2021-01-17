#' ETL process that download current CPE definitions and return a list with a
#' data frame for CPE objects. The list also contains a visNetwork object with
#' CPE objects as nodes and all relations as edges.
#'
#' @param verbose Default set as FALSE
#'
#' @return list of data frames
#'
#' @examples
#' \donttest{
#' cpes <- mitre::getCPEData()
#' }
getCPEData <- function(verbose = FALSE) {
  cpe.file <- "data-raw/official-cpe-dictionary_v2.3.xml"
  if (verbose) print("Indexing CPE XML and namespace schemas...")
  doc <- xml2::read_xml(cpe.file)

  if (verbose) print("Parsing product title and cpe codes 2.x...")
  cpes <- data.frame(title = xml2::xml_text(xml2::xml_find_all(doc, "//*[cpe-23:cpe23-item]/*[@xml:lang='en-US'][1]")),
                     cpe.22 = xml2::xml_text(xml2::xml_find_all(doc, "//*[cpe-23:cpe23-item]/@name")),
                     cpe.23 = xml2::xml_text(xml2::xml_find_all(doc, "//cpe-23:cpe23-item/@name")),
                     stringsAsFactors = F)
  nodes <- xml2::xml_find_all(doc, ".//cpe-23:cpe23-item")

  if (verbose) print("Flattening nested references. Be patient, it needs performance optimization...")
  cpes$refs <- unlist(lapply(xml2::xml_find_first(nodes, ".//ancestor::d1:cpe-item/d1:references"),
                             function(x) {
                               z <- lapply(xml2::as_list(x), function(y) as.character(attributes(y)))
                               names(z) <- as.character(sapply(xml2::as_list(x), as.character))
                               jsonlite::toJSON(z, pretty = T)
                             }))

  if (verbose) print("Searching for deprecated nodes...")
  cpes$deprecated <- !is.na(as.logical(xml2::xml_attr(xml2::xml_find_first(nodes,
                                                                           ".//ancestor::d1:cpe-item"),
                                                      "deprecated")))

  if (verbose) print("Creating factor columns from CPE ids ...")
  new.cols <- c("std", "std.v", "part", "vendor", "product",
                "version", "update", "edition", "language", "sw_edition",
                "target_sw", "target_hw", "other")
  cpes$cpe.23 <- stringr::str_replace_all(cpes$cpe.23, "\\\\:", ";")
  cpes <- tidyr::separate(data = cpes, col = cpe.23, into = new.cols, sep = ":", remove = F)
  cpes <- dplyr::select(.data = cpes, -std, -std.v)
  cpes$vendor <- as.factor(cpes$vendor)
  cpes$product <- as.factor(cpes$product)
  cpes$language <- as.factor(cpes$language)
  cpes$sw_edition <- as.factor(cpes$sw_edition)
  cpes$target_sw <- as.factor(cpes$target_sw)
  cpes$target_hw <- as.factor(cpes$target_hw)

  if (verbose) print("Extracting CVE references...")
  cpe2cve <- lapply(cpes$refs, function(x) stringr::str_extract_all(x, "CVE-\\d+-\\d+"))
  cpe2cve <- sapply(cpe2cve, function(x) ifelse(identical(x[[1]], character(0)), NA, x[[1]]))
  cpe2cve <- data.frame(from = cpes$cpe.23, to = cpe2cve, stringsAsFactors = FALSE)
  cpe2cve <- cpe2cve[complete.cases(cpe2cve), ]

  if (verbose) print("Building CPE network ...")
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
