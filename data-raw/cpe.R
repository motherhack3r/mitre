library(usethis)

if (!dir.exists("data")) dir.create("data")

# Download
if (!file.exists("data-raw/nist-cpe.xml.zip"))
  download.file(url = "https://nvd.nist.gov/feeds/xml/cpe/dictionary/official-cpe-dictionary_v2.3.xml.zip",
                destfile = "data-raw/nist-cpe.xml.zip")


doc <- xml2::read_xml("data-raw/nist-cpe.xml.zip")

cpes <- data.frame(title = xml2::xml_text(xml2::xml_find_all(doc, "//*[cpe-23:cpe23-item]/*[@xml:lang='en-US'][1]")),
                   cpe.22 = xml2::xml_text(xml2::xml_find_all(doc, "//*[cpe-23:cpe23-item]/@name")),
                   cpe.23 = xml2::xml_text(xml2::xml_find_all(doc, "//cpe-23:cpe23-item/@name")),
                   stringsAsFactors = F)
nodes <- xml2::xml_find_all(doc, ".//cpe-23:cpe23-item")

cpes$refs <- unlist(lapply(xml2::xml_find_first(nodes, ".//ancestor::d1:cpe-item/d1:references"),
                           function(x) {
                             z <- lapply(xml2::as_list(x), function(y) as.character(attributes(y)))
                             names(z) <- as.character(sapply(xml2::as_list(x), as.character))
                             jsonlite::toJSON(z, pretty = T)
                           }))

cpes$deprecated <- !is.na(as.logical(xml2::xml_attr(xml2::xml_find_first(nodes,
                                                                         ".//ancestor::d1:cpe-item"),
                                                    "deprecated")))

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

cpe.nist <- cpes
usethis::use_data(cpe.nist, compress = "xz", overwrite = TRUE)

