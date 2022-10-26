if(any(grepl("package:RJSONIO", search()))) detach("package:RJSONIO")
library(jsonlite)
library(stringr)
library(usethis)
library(tidyr, warn.conflicts = FALSE)
library(dplyr, warn.conflicts = FALSE)
library(xml2)

if (!dir.exists("data")) dir.create("data")
if (!dir.exists("data-raw/cpe")) dir.create("data-raw/cpe")

# Download
if (!file.exists("data-raw/cpe/nist-cpe.xml.zip"))
  download.file(url = "https://nvd.nist.gov/feeds/xml/cpe/dictionary/official-cpe-dictionary_v2.3.xml.zip",
                destfile = "data-raw/cpe/cpe-nist.xml.zip")


doc <- xml2::read_xml("data-raw/cpe/cpe-nist.xml.zip")

cpes <- data.frame(title  = xml_text(xml_find_all(doc, "//d1:cpe-item/d1:title[1]")),
                   cpe.23 = xml_text(xml_find_all(doc, "//cpe-23:cpe23-item/@name")),
                   stringsAsFactors = F)

nodes <- xml2::xml_find_all(doc, "//cpe-23:cpe23-item")
cpes$refs <- unlist(lapply(xml_find_first(nodes, "ancestor::d1:cpe-item/d1:references"),
                    function(x) {
                      z <- lapply(as_list(x), function(y) as.character(attributes(y)))
                      names(z) <- as.character(sapply(as_list(x), as.character))
                      toJSON(z)
                    }))

cpes$deprecated <- !is.na(as.logical(xml_attr(xml_find_first(nodes,
                                                             ".//ancestor::d1:cpe-item"),
                                              "deprecated")))

rm(nodes)

new.cols <- c("std", "std.v", "part", "vendor", "product",
              "version", "update", "edition", "language", "sw_edition",
              "target_sw", "target_hw", "other")
cpes$cpe.23 <- stringr::str_replace_all(cpes$cpe.23, "\\\\:", ";")
cpes <- tidyr::separate(data = cpes, col = "cpe.23", into = new.cols, sep = ":", remove = F)
cpes$id <- 1:nrow(cpes)
cpe.nist <- dplyr::select(.data = cpes, "id", "title", "cpe.23", "part", "vendor", "product", "version", "refs", "deprecated")

rm(cpes, new.cols, doc)
usethis::use_data(cpe.nist, compress = "xz", overwrite = TRUE)


