#' ETL process that download current CPE definitions and return a list with a
#' data frame for CPE objects. The list also contains a visNetwork object with
#' CPE objects as nodes and all relations as edges.
#'
#' @param verbose Default set as FALSE
#'
#' @return list of data frames
#' @export
#'
#' @examples
#' \donttest{
#' cpes <- mitre::getCPEData()
#' }
getCPEData <- function(cpe.file = "data-raw/official-cpe-dictionary_v2.3.xml", verbose = FALSE) {
  if (verbose) print("Indexing CPE XML and namespace schemas...")
  doc <- xml2::read_xml(cpe.file)

  if (verbose) print("Parsing product title and cpe codes 2.x...")
  cpes <- data.frame(title = xml2::xml_text(xml2::xml_find_all(doc, "//*[cpe-23:cpe23-item]/*[@xml:lang='en-US'][1]")),
                     cpe.22 = xml2::xml_text(xml2::xml_find_all(doc, "//cpe-23:cpe23-item/@name")),
                     cpe.23 = xml2::xml_text(xml2::xml_find_all(doc, "//*[cpe-23:cpe23-item]/*/@name")),
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

  return(cpes)
}
