library(usethis)
library(dplyr, warn.conflicts = FALSE)
library(rvest, warn.conflicts = FALSE)
library(xml2, warn.conflicts = FALSE)
if(any(grepl("package:jsonlite", search()))) detach("package:jsonlite") else message("jsonlite not loaded")
library(RJSONIO, warn.conflicts = FALSE)

if (!dir.exists("data")) dir.create("data")

# Latest XML definition
if (!as.logical(length(list.files(path = "data-raw", pattern = "^cwec_v\\d+\\..*\\.xml$")))) {
  download.file(url = "http://cwe.mitre.org/data/xml/cwec_latest.xml.zip",
                destfile = "data-raw/cwe-latest.xml.zip")
  utils::unzip(zipfile = paste0("data-raw/cwe-latest.xml.zip"),
               exdir = paste0("data-raw"),
               overwrite = T)
  unlink("data-raw/cwe-latest.xml.zip")
}

cwes.file <- file.path("data-raw",
                       list.files(path = "data-raw", pattern = "^cwec_v\\d+\\..*\\.xml$")[1])

doc <- read_html(cwes.file)

#####
# PARSE WEAKNESSES
#

raw.cwes <- html_nodes(doc, "weakness")
# Extract Weakness node attributes
cwes <- as.data.frame(t(sapply(raw.cwes, rvest::html_attrs)), stringsAsFactors = F)
names(cwes) <- c("ID", "Name", "Abstraction", "Structure", "Status")
cwes$CWE_Type <- rep("Weakness", nrow(cwes))
# Set factors (improve setting levels according to XSD)
cwes$Abstraction <- as.factor(cwes$Abstraction)
cwes$Structure <- as.factor(cwes$Structure)
# Add extra field with code standard
cwes$Code_Standard <- paste("CWE-", cwes$ID, sep = "")

cwes$Description <- sapply(rvest::html_nodes(doc, xpath = "//weakness/description"),
                           rvest::html_text)
ids <- xml2::xml_text(xml2::xml_find_all(doc, "//weakness/extended_description/parent::*/@id"))
vals <- sapply(xml2::xml_find_all(doc, "//weakness/extended_description"), xml2::xml_text)
df <- data.frame(ID = ids, Extended_Description = vals, stringsAsFactors = F)
cwes <- dplyr::left_join(cwes, df, by = c("ID"))

ids <- xml2::xml_text(xml2::xml_find_all(doc, "//weakness/related_weaknesses/parent::*/@id"))
vals <- sapply(xml2::xml_find_all(doc, "//weakness/related_weaknesses"),
               function(x) RJSONIO::toJSON(lapply(rvest::html_children(x),
                                                  rvest::html_attrs),
                                           pretty = T))
df <- data.frame(ID = ids, Related_Weakness = vals, stringsAsFactors = F)
cwes <- dplyr::left_join(cwes, df, by = c("ID"))

ids <- xml2::xml_text(xml2::xml_find_all(doc, "//weakness/weakness_ordinalities/parent::*/@id"))
vals <- sapply(xml2::xml_find_all(doc, "//weakness/weakness_ordinalities"),
               function(x) RJSONIO::toJSON(lapply(rvest::html_children(x),
                                                  function(x) rvest::html_text(rvest::html_children(x))),
                                           pretty = T))
df <- data.frame(ID = ids, Weakness_Ordinality = vals, stringsAsFactors = F)
cwes <- dplyr::left_join(cwes, df, by = c("ID"))

ids <- xml2::xml_text(xml2::xml_find_all(doc, "//weakness/applicable_platforms/parent::*/@id"))
vals <- sapply(xml2::xml_find_all(doc, "//weakness/applicable_platforms"),
               function(x) {
                 y <- lapply(rvest::html_children(x), rvest::html_attrs)
                 names(y) <- rvest::html_name(rvest::html_children(x))
                 RJSONIO::toJSON(y, pretty = T)
               })
df <- data.frame(ID = ids, Applicable_Platforms = vals, stringsAsFactors = F)
cwes <- dplyr::left_join(cwes, df, by = c("ID"))

ids <- xml2::xml_text(xml2::xml_find_all(doc, "//weakness/background_details/parent::*/@id"))
vals <- sapply(xml2::xml_find_all(doc, "//weakness/background_details"),
               function(x) RJSONIO::toJSON(xml2::xml_text(x),
                                           pretty = T))
df <- data.frame(ID = ids, Background_Details = vals, stringsAsFactors = F)
cwes <- dplyr::left_join(cwes, df, by = c("ID"))

ids <- xml2::xml_text(xml2::xml_find_all(doc, "//weakness/alternate_terms/parent::*/@id"))
vals <- sapply(xml2::xml_find_all(doc, "//weakness/alternate_terms"),
               function(x) RJSONIO::toJSON(lapply(rvest::html_children(x),
                                                  rvest::html_text),
                                           pretty = T))
df <- data.frame(ID = ids, Alternate_Terms = vals, stringsAsFactors = F)
cwes <- dplyr::left_join(cwes, df, by = c("ID"))

ids <- xml2::xml_text(xml2::xml_find_all(doc, "//weakness/modes_of_introduction/parent::*/@id"))
vals <- sapply(xml2::xml_find_all(doc, "//weakness/modes_of_introduction"),
               function(x)
                 RJSONIO::toJSON(lapply(lapply(rvest::html_children(x),
                                               function(x) rvest::html_children(x)),
                                        function(y) {
                                          z <- rvest::html_text(y)
                                          names(z) <- rvest::html_name(y)
                                          z
                                        }),
                                 pretty = T))
df <- data.frame(ID = ids, Modes_Of_Introduction = vals, stringsAsFactors = F)
cwes <- dplyr::left_join(cwes, df, by = c("ID"))

ids <- xml2::xml_text(xml2::xml_find_all(doc, "//weakness/likelihood_of_exploit/parent::*/@id"))
vals <- sapply(xml2::xml_find_all(doc, "//weakness/likelihood_of_exploit"), rvest::html_text)
df <- data.frame(ID = ids, Likelihood_Of_Exploit = vals, stringsAsFactors = F)
cwes <- dplyr::left_join(cwes, df, by = c("ID"))
cwes$Likelihood_Of_Exploit <- as.factor(cwes$Likelihood_Of_Exploit)

ids <- xml2::xml_text(xml2::xml_find_all(doc, "//weakness/common_consequences/parent::*/@id"))
vals <- sapply(xml2::xml_find_all(doc, "//weakness/common_consequences"),
               function(x) RJSONIO::toJSON(lapply(
                 lapply(rvest::html_children(x),
                        function(x) rvest::html_children(x)),
                 function(y) {
                   z <- rvest::html_text(y)
                   names(z) <- rvest::html_name(y)
                   z
                 }
               ),
               pretty = T))
df <- data.frame(ID = ids, Common_Consequences = vals, stringsAsFactors = F)
cwes <- dplyr::left_join(cwes, df, by = c("ID"))

ids <- xml2::xml_text(xml2::xml_find_all(doc, "//weakness/detection_methods/parent::*/@id"))
vals <- sapply(xml2::xml_find_all(doc, "//weakness/detection_methods"),
               function(x) RJSONIO::toJSON(lapply(
                 lapply(rvest::html_children(x),
                        function(x) rvest::html_children(x)),
                 function(y) {
                   z <- rvest::html_text(y)
                   names(z) <- rvest::html_name(y)
                   z
                 }
               ),
               pretty = T))
df <- data.frame(ID = ids, Detection_Methods = vals, stringsAsFactors = F)
cwes <- dplyr::left_join(cwes, df, by = c("ID"))

ids <- xml2::xml_text(xml2::xml_find_all(doc, "//weakness/potential_mitigations/parent::*/@id"))
vals <- sapply(xml2::xml_find_all(doc, "//weakness/potential_mitigations"),
               function(x) RJSONIO::toJSON(lapply(
                 lapply(rvest::html_children(x),
                        function(x) rvest::html_children(x)),
                 function(y) {
                   z <- rvest::html_text(y)
                   names(z) <- rvest::html_name(y)
                   z
                 }
               ),
               pretty = T))
df <- data.frame(ID = ids, Potential_Mitigations = vals, stringsAsFactors = F)
cwes <- dplyr::left_join(cwes, df, by = c("ID"))

ids <- xml2::xml_text(xml2::xml_find_all(doc, "//weakness/observed_examples/parent::*/@id"))
vals <- sapply(xml2::xml_find_all(doc, "//weakness/observed_examples"),
               function(x) RJSONIO::toJSON(lapply(
                 lapply(rvest::html_children(x),
                        function(x) rvest::html_children(x)),
                 function(y) {
                   z <- rvest::html_text(y)
                   names(z) <- rvest::html_name(y)
                   z
                 }
               ),
               pretty = T))
df <- data.frame(ID = ids, Observed_Examples = vals, stringsAsFactors = F)
cwes <- dplyr::left_join(cwes, df, by = c("ID"))

ids <- xml2::xml_text(xml2::xml_find_all(doc, "//weakness/functional_areas/parent::*/@id"))
vals <- sapply(xml2::xml_find_all(doc, "//weakness/functional_areas"),
               function(x) RJSONIO::toJSON(sapply(rvest::html_children(x), rvest::html_text)))
df <- data.frame(ID = ids, Functional_Areas = vals, stringsAsFactors = F)
cwes <- dplyr::left_join(cwes, df, by = c("ID"))

ids <- xml2::xml_text(xml2::xml_find_all(doc, "//weakness/affected_resources/parent::*/@id"))
vals <- sapply(xml2::xml_find_all(doc, "//weakness/affected_resources"),
               function(x) RJSONIO::toJSON(sapply(rvest::html_children(x), rvest::html_text)))
df <- data.frame(ID = ids, Affected_Resources = vals, stringsAsFactors = F)
cwes <- dplyr::left_join(cwes, df, by = c("ID"))

ids <- xml2::xml_text(xml2::xml_find_all(doc, "//weakness/taxonomy_mappings/parent::*/@id"))
vals <- sapply(xml2::xml_find_all(doc, "//weakness/taxonomy_mappings"),
               function(x) RJSONIO::toJSON({w <- lapply(
                 lapply(rvest::html_children(x),
                        function(x) rvest::html_children(x)),
                 function(y) {
                   z <- rvest::html_text(y)
                   names(z) <- rvest::html_name(y)
                   z
                 })
               names(w) <- unlist(rvest::html_attrs(rvest::html_children(x)))
               w}, pretty = T))
df <- data.frame(ID = ids, Taxonomy_Mappings = vals, stringsAsFactors = F)
cwes <- dplyr::left_join(cwes, df, by = c("ID"))

ids <- xml2::xml_text(xml2::xml_find_all(doc, "//weakness/related_attack_patterns/parent::*/@id"))
vals <- sapply(xml2::xml_find_all(doc, "//weakness/related_attack_patterns"),
               function(x) RJSONIO::toJSON(sapply(rvest::html_children(x), rvest::html_attrs)))
df <- data.frame(ID = ids, Related_Attack_Patterns = vals, stringsAsFactors = F)
cwes <- dplyr::left_join(cwes, df, by = c("ID"))

cwe.weaknesses <- cwes
usethis::use_data(cwe.weaknesses, compress = "xz", overwrite = TRUE)


#####
# PARSE CATEGORIES

raw.cwes <- rvest::html_nodes(doc, "category")
cwes <- as.data.frame(t(sapply(raw.cwes, rvest::html_attrs)), stringsAsFactors = F)
names(cwes) <- c("ID", "Name", "Status")
cwes$CWE_Type <- rep("Category", nrow(cwes))
cwes$Code_Standard <- paste("CWE-", cwes$ID, sep = "")

cwes$Description <- sapply(rvest::html_nodes(doc, xpath = "//category/summary"),
                           rvest::html_text)

ids <- xml2::xml_text(xml2::xml_find_all(doc, "//category/relationships/parent::*/@id"))
vals <- sapply(xml2::xml_find_all(doc, "//category/relationships"),
               function(x) {
                 members <- rvest::html_attrs(rvest::html_children(x))
                 names(members) <- rvest::html_name(rvest::html_children(x))
                 RJSONIO::toJSON(members, pretty = T)
               }
)
df <- data.frame(ID = ids, Related_Weakness = vals, stringsAsFactors = F)
cwe.categories <- dplyr::left_join(cwes, df, by = c("ID"))
usethis::use_data(cwe.categories, compress = "xz", overwrite = TRUE)


#####
# PARSE VIEWS

raw.cwes <- rvest::html_nodes(doc, "view")
cwes <- as.data.frame(t(sapply(raw.cwes, rvest::html_attrs)), stringsAsFactors = F)
# View attribute "Type" overwriten with character "View"
names(cwes) <- c("ID", "Name", "CWE_Type", "Status")
cwes$CWE_Type <- rep("View", nrow(cwes))
cwes$Code_Standard <- paste("CWE-", cwes$ID, sep = "")
cwes$Description <- sapply(rvest::html_nodes(doc, xpath = "//view/objective"),
                           rvest::html_text)
ids <- xml2::xml_text(xml2::xml_find_all(doc, "//view/members/parent::*/@id"))
vals <- sapply(xml2::xml_find_all(doc, "//view/members"),
               function(x) {
                 members <- rvest::html_attrs(rvest::html_children(x))
                 names(members) <- rvest::html_name(rvest::html_children(x))
                 RJSONIO::toJSON(members, pretty = T)
               }
)
df <- data.frame(ID = ids, Related_Weakness = vals, stringsAsFactors = F)
cwe.views <- dplyr::left_join(cwes, df, by = c("ID"))
usethis::use_data(cwe.views, compress = "xz", overwrite = TRUE)

rm(cwes, df, doc, raw.cwes, cwes.file, ids, vals)
