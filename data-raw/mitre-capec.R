library(xml2)
library(rvest)

# CAPEC
# Ref: https://capec.mitre.org/data/index.html

if (!file.exists("data-raw/capec-latest.xml"))
  download.file(url = "https://capec.mitre.org/data/xml/capec_latest.xml",
                destfile = "data-raw/capec-latest.xml")
doc <- read_xml("data-raw/capec-latest.xml")

# CAPEC VIEWs
views <-
  data.frame(id = doc %>% html_elements(xpath = "//capec:View/@ID") %>% html_text(),
             name = doc %>% html_elements(xpath = "//capec:View/@Name") %>% html_text(),
             type = doc %>% html_elements(xpath = "//capec:View/@Type") %>% html_text(),
             status = doc %>% html_elements(xpath = "//capec:View/@Status") %>% html_text(),
             description = doc %>% html_elements(xpath = "//capec:View/capec:Objective") %>% html_text(),
             stringsAsFactors = FALSE)

views$id <- paste0("CAPEC-", views$id)

# CAPEC CATEGORIES
categories <-
  data.frame(id = doc %>% html_elements(xpath = "//capec:Category/@ID") %>% html_text(),
             name = doc %>% html_elements(xpath = "//capec:Category/@Name") %>% html_text(),
             status = doc %>% html_elements(xpath = "//capec:Category/@Status") %>% html_text(),
             description = doc %>% html_elements(xpath = "//capec:Category/capec:Summary") %>% html_text(),
             stringsAsFactors = FALSE)
categories$id <- paste0("CAPEC-", categories$id)


df <- rvest::html_elements(doc, xpath = "//capec:Category")
df <- lapply(df,
             function(x) {
               kl <- rvest::html_elements(x, xpath = "capec:Taxonomy_Mappings/capec:Taxonomy_Mapping")
               k<-lapply(kl,
                         function(y) {
                           eids <- xml2::xml_text(rvest::html_elements(y, xpath="capec:Entry_ID"))
                           enam <- xml2::xml_text(rvest::html_elements(y,xpath="capec:Entry_Name"))
                           data.frame(taxonomy_id=ifelse(identical(eids, character(0)), NA, eids),
                                      taxonomy_name=ifelse(identical(enam, character(0)), NA, enam),
                                      stringsAsFactors = FALSE)
                         })
               names(k) <- xml2::xml_attr(kl, "Taxonomy_Name")
               c2t <- dplyr::bind_rows(k, .id = "taxonomy")
               c2t
             })
names(df) <- categories$id
df <- dplyr::bind_rows(df, .id = "from")
selected <- which(df$taxonomy == "ATTACK")
df$taxonomy_id[selected] <- paste0("T", df$taxonomy_id[selected])
selected <- which(df$taxonomy == "WASC")
df$taxonomy_id[selected] <- paste0("WASC-", df$taxonomy_id[selected])
selected <- which(df$taxonomy == "OWASP Attacks")
df$taxonomy_id[selected] <- paste0("OWASP-", df$taxonomy_name[selected])
names(df) <- c("from", "name", "to", "title")
capec.relations <- df

# CAPEC ATTACK PATTERNs
capec.atcks <- rvest::html_elements(doc, xpath = "//capec:Attack_Pattern")
att.id <- paste0("CAPEC-", sapply(capec.atcks,
                                  function(x)
                                    rvest::html_text(rvest::html_element(x, xpath="./@ID"))))
att.name <- sapply(capec.atcks,
                   function(x)
                     rvest::html_text(rvest::html_element(x, xpath="./@Name")))
att.status <- sapply(capec.atcks,
                     function(x)
                       rvest::html_text(rvest::html_element(x, xpath="./@Status")))
att.abstraction <- sapply(capec.atcks,
                          function(x)
                            rvest::html_text(rvest::html_element(x, xpath="./@Abstraction")))
att.descr <- sapply(capec.atcks,
                    function(x)
                      rvest::html_text(rvest::html_elements(x, xpath = "capec:Description")))

df <- lapply(capec.atcks,
             function(x) {
               data.frame(id = rvest::html_text(rvest::html_elements(x, xpath=".//capec:Related_Attack_Pattern/@CAPEC_ID")),
                          nature = rvest::html_text(rvest::html_elements(x, xpath=".//capec:Related_Attack_Pattern/@Nature")),
                          stringsAsFactors = F)
             })
names(df) <- att.id
df <- dplyr::bind_rows(df, .id = "from")
names(df) <- c("from", "to", "name")
df$to <- paste0("CAPEC-", df$to)
df$title <- df$name

capec.relations <- dplyr::bind_rows(capec.relations, df)

att.prerequisites <- sapply(capec.atcks,
                            function(x)
                              jsonlite::toJSON(rvest::html_text(rvest::html_elements(x, xpath = "capec:Prerequisites/capec:Prerequisite"))))

att.alernate_terms <- sapply(capec.atcks,
                             function(x)
                               jsonlite::toJSON(rvest::html_text(rvest::html_elements(x, xpath = "capec:Alternate_Terms/capec:Alternate_Term/capec:Term"))))


att.severity <- sapply(capec.atcks,
                       function(x) {
                         ifelse(test = identical(rvest::html_text(rvest::html_nodes(x, xpath = "capec:Typical_Severity")), character(0)),
                                yes = "Unknown",
                                no = rvest::html_text(rvest::html_nodes(x, xpath = "capec:Typical_Severity")))})

att.likelihoodatt <- sapply(capec.atcks,
                            function(x) {
                              ifelse(test = identical(rvest::html_text(rvest::html_nodes(x, xpath = "capec:Likelihood_Of_Attack")), character(0)),
                                     yes = "Unknown",
                                     no = rvest::html_text(rvest::html_nodes(x, xpath = "capec:Likelihood_Of_Attack")))})

att.flow <- sapply(capec.atcks,
                   function(x) {
                     k <- rvest::html_nodes(x, xpath = "capec:Execution_Flow")
                     k <- lapply(xml2::xml_children(k), xml2::as_list)
                     jsonlite::toJSON(k, pretty = T)
                   })

att.skills <- sapply(capec.atcks,
                     function(x) {
                       jsonlite::toJSON(data.frame(
                         skilled = sapply(rvest::html_nodes(x, xpath = "capec:Skills_Required/capec:Skill/@Level"),
                                          xml2::xml_text),
                         skill = sapply(rvest::html_nodes(x, xpath = "capec:Skills_Required/capec:Skill"),
                                        xml2::xml_text), stringsAsFactors = FALSE))
                     })

att.reqsrcs <- sapply(capec.atcks,
                      function(x)
                        jsonlite::toJSON(rvest::html_text(rvest::html_elements(x, xpath = "capec:Resources_Required/capec:Resource"))))

att.indicators <- sapply(capec.atcks,
                         function(x)
                           jsonlite::toJSON(rvest::html_text(rvest::html_elements(x, xpath = "capec:Indicators/capec:Indicator"))))

att.consequences <- sapply(capec.atcks,
                           function(x) {
                             cons <- rvest::html_nodes(x, xpath = "capec:Consequences/capec:Consequence")
                             csq <- lapply(cons, function(y) {
                               data.frame(name = sapply(xml2::xml_children(y), xml2::xml_name),
                                          value = sapply(xml2::xml_children(y), xml2::xml_text),
                                          stringsAsFactors = FALSE)
                             })
                             names(csq) <- sapply(cons, xml2::xml_name)
                             jsonlite::toJSON(csq, pretty = T)
                           })

att.mitigations <- sapply(capec.atcks,
                          function(x)
                            jsonlite::toJSON(rvest::html_text(rvest::html_elements(x, xpath = "capec:Mitigations/capec:Mitigation"))))

att.examples <- sapply(capec.atcks,
                       function(x)
                         jsonlite::toJSON(rvest::html_text(rvest::html_elements(x, xpath = "capec:Example_Instances/capec:Example"))))

df <- lapply(att.examples,
                    function(x)
                      data.frame(to = stringr::str_extract_all(x, "CVE-\\d+-\\d+")[[1]],
                                 stringsAsFactors = FALSE))
names(df) <- att.id
df <- dplyr::bind_rows(df, .id = "from")
df$name <- rep("example", nrow(df))
df$title <- df$name
capec.relations <- rbind(capec.relations, df)

df <- lapply(capec.atcks,
                    function(x)
                      # XXX: Sometimes xml is empty
                      data.frame(to = paste0("CWE-", xml2::xml_text(rvest::html_elements(x, xpath = "capec:Related_Weaknesses/capec:Related_Weakness/@CWE_ID"))),
                                 stringsAsFactors = FALSE))
names(df) <- att.id
df <- dplyr::bind_rows(df, .id = "from")
df$name <- rep("take advantage of", nrow(df))
df$title <- df$name
capec.relations <- rbind(capec.relations, df)


df <- lapply(capec.atcks,
             function(x) {
               kl <- rvest::html_elements(x, xpath = "capec:Taxonomy_Mappings/capec:Taxonomy_Mapping")
               k<-lapply(kl,
                         function(y) {
                           eids <- xml2::xml_text(rvest::html_elements(y, xpath="capec:Entry_ID"))
                           enam <- xml2::xml_text(rvest::html_elements(y,xpath="capec:Entry_Name"))
                           data.frame(taxonomy_id=ifelse(identical(eids, character(0)), NA, eids),
                                      taxonomy_name=ifelse(identical(enam, character(0)), NA, enam),
                                      stringsAsFactors = FALSE)
                         })
               names(k) <- xml2::xml_attr(kl, "Taxonomy_Name")
               c2t <- dplyr::bind_rows(k, .id = "taxonomy")
               c2t
             })
names(df) <- att.id
df <- dplyr::bind_rows(df, .id = "from")
selected <- which(df$taxonomy == "ATTACK")
df$taxonomy_id[selected] <- paste0("T", df$taxonomy_id[selected])
selected <- which(df$taxonomy == "WASC")
df$taxonomy_id[selected] <- paste0("WASC-", df$taxonomy_id[selected])
selected <- which(df$taxonomy == "OWASP Attacks")
df$taxonomy_id[selected] <- paste0("OWASP-", df$taxonomy_name[selected])
names(df) <- c("from", "name", "to", "title")

capec.relations <- dplyr::bind_rows(capec.relations, df)

capec.patterns <- data.frame(id = att.id,
                    name = att.name,
                    abstraction = att.abstraction,
                    status = att.status,
                    description = att.descr,
                    alternate_terms = att.alernate_terms,
                    likelihood = att.likelihoodatt,
                    severity = att.severity,
                    execution_flow = att.flow,
                    prerequisites = att.prerequisites,
                    skills = att.skills,
                    resources = att.reqsrcs,
                    indicators = att.indicators,
                    consequences = att.consequences,
                    mitigations = att.mitigations,
                    examples = att.examples,
                    stringsAsFactors = FALSE)

capec.categories <- categories
capec.views <- views


usethis::use_data(capec.views, compress = "xz", overwrite = TRUE)
usethis::use_data(capec.categories, compress = "xz", overwrite = TRUE)
usethis::use_data(capec.patterns, compress = "xz", overwrite = TRUE)
usethis::use_data(capec.relations, compress = "xz", overwrite = TRUE)

