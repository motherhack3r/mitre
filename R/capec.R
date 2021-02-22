#' ETL process that download current CAPEC definitions and return a list with a
#' data frame for CAPEC objects. The list also contains a visNetwork object with
#' CAPEC objects as nodes and all relations as edges.
#'
#' @param verbose Default set as FALSE
#'
#' @return list of data frames
getCAPECData <- function(verbose = FALSE) {
  #### References: https://capec.mitre.org/data/index.html
  capec.file <- "data-raw/capec_latest.xml"
  if (verbose) print("[.][CAPEC] Indexing CAPEC XML raw file ...")
  doc <- xml2::read_xml(capec.file)

  # CAPEC VIEWs
  if (verbose) print("[.][CAPEC] Parsing CAPEC Views ...")
  views <-
    data.frame(id = rvest::html_text(rvest::xml_nodes(doc, xpath = "//capec:View/@ID")),
               name = rvest::html_text(rvest::xml_nodes(doc, xpath = "//capec:View/@Name")),
               type = rvest::html_text(rvest::xml_nodes(doc, xpath = "//capec:View/@Type")),
               status = rvest::html_text(rvest::xml_nodes(doc, xpath = "//capec:View/@Status")),
               description = rvest::html_text(rvest::xml_nodes(doc, xpath = "//capec:View/capec:Objective")),
               stringsAsFactors = FALSE)
  views$id <- paste0("CAPEC-", views$id)

  # CAPEC CATEGORIES
  if (verbose) print("[.][CAPEC] Parsing CAPEC Categories ...")
  categories <-
    data.frame(id = rvest::html_text(rvest::xml_nodes(doc, xpath = "//capec:Category/@ID")),
               name = rvest::html_text(rvest::xml_nodes(doc, xpath = "//capec:Category/@Name")),
               status = rvest::html_text(rvest::xml_nodes(doc, xpath = "//capec:Category/@Status")),
               description = rvest::html_text(rvest::xml_nodes(doc, xpath = "//capec:Category/capec:Summary")),
               stringsAsFactors = FALSE)
  categories$id <- paste0("CAPEC-", categories$id)


  if (verbose) print("[.][CAPEC] Adding Taxonomy Mappings as network edges ...")
  raw.capec.categs <- rvest::xml_nodes(doc, xpath = "//capec:Category")
  kk <- lapply(raw.capec.categs,
               function(x) {
                 kl <- rvest::xml_nodes(x, xpath = "capec:Taxonomy_Mappings/capec:Taxonomy_Mapping")
                 k<-lapply(kl,
                           function(y) {
                             eids <- xml2::xml_text(rvest::xml_nodes(y, xpath="capec:Entry_ID"))
                             enam <- xml2::xml_text(rvest::xml_nodes(y,xpath="capec:Entry_Name"))
                             data.frame(taxonomy_id=ifelse(identical(eids, character(0)), NA, eids),
                                        taxonomy_name=ifelse(identical(enam, character(0)), NA, enam),
                                        stringsAsFactors = FALSE)
                           })
                 names(k) <- xml2::xml_attr(kl, "Taxonomy_Name")
                 c2t <- dplyr::bind_rows(k, .id = "taxonomy")
                 c2t
               })
  names(kk) <- categories$id
  capec2other <- dplyr::bind_rows(kk, .id = "from")
  rm(kk)
  selected <- which(capec2other$taxonomy == "ATTACK")
  capec2other$taxonomy_id[selected] <- paste0("T", capec2other$taxonomy_id[selected])
  selected <- which(capec2other$taxonomy == "WASC")
  capec2other$taxonomy_id[selected] <- paste0("WASC-", capec2other$taxonomy_id[selected])
  selected <- which(capec2other$taxonomy == "OWASP Attacks")
  capec2other$taxonomy_id[selected] <- paste0("OWASP-", capec2other$taxonomy_name[selected])
  names(capec2other) <- c("from", "label", "to", "title")
  capecnet <- capec2other

  # CAPEC ATTACK PATTERNs
  if (verbose) print("[.][CAPEC] Parsing CAPEC Patterns ...")
  raw.capec.atcks <- rvest::xml_nodes(doc, xpath = "//capec:Attack_Pattern")
  att.id <- paste0("CAPEC-", sapply(raw.capec.atcks,
                   function(x)
                     rvest::html_text(rvest::xml_node(x, xpath="./@ID"))))
  att.name <- sapply(raw.capec.atcks,
                     function(x)
                       rvest::html_text(rvest::xml_node(x, xpath="./@Name")))
  att.status <- sapply(raw.capec.atcks,
                       function(x)
                         rvest::html_text(rvest::xml_node(x, xpath="./@Status")))
  att.abstraction <- sapply(raw.capec.atcks,
                            function(x)
                              rvest::html_text(rvest::xml_node(x, xpath="./@Abstraction")))
  att.descr <- sapply(raw.capec.atcks,
                      function(x)
                        rvest::html_text(rvest::xml_nodes(x, xpath = "capec:Description")))

  if (verbose) print("[.][CAPEC] Extracting relatec attack patterns ...")
  kk <- lapply(raw.capec.atcks,
               function(x) {
                 data.frame(id = rvest::html_text(rvest::xml_nodes(x, xpath=".//capec:Related_Attack_Pattern/@CAPEC_ID")),
                            nature = rvest::html_text(rvest::xml_nodes(x, xpath=".//capec:Related_Attack_Pattern/@Nature")),
                            stringsAsFactors = F)
               })
  names(kk) <- att.id
  capec2capec <- dplyr::bind_rows(kk, .id = "from")
  names(capec2capec) <- c("from", "to", "label")
  capec2capec$to <- paste0("CAPEC-", capec2capec$to)
  capec2capec$title <- capec2capec$label
  rm(kk)

  capecnet <- dplyr::bind_rows(capecnet, capec2capec)

  if (verbose) print("[.][CAPEC] Compacting pre-requisites ...")
  att.prerequisites <- sapply(raw.capec.atcks,
                              function(x)
                                jsonlite::toJSON(rvest::html_text(rvest::xml_nodes(x, xpath = "capec:Prerequisites/capec:Prerequisite"))))

  if (verbose) print("[.][CAPEC] Selecting alternate terms ...")
  att.alernate_terms <- sapply(raw.capec.atcks,
                              function(x)
                                jsonlite::toJSON(rvest::html_text(rvest::xml_nodes(x, xpath = "capec:Alternate_Terms/capec:Alternate_Term/capec:Term"))))


  if (verbose) print("[.][CAPEC] Calculating typical severity ...")
  att.severity <- sapply(raw.capec.atcks,
                         function(x) {
                           ifelse(test = identical(rvest::html_text(rvest::html_nodes(x, xpath = "capec:Typical_Severity")), character(0)),
                                  yes = "Unknown",
                                  no = rvest::html_text(rvest::html_nodes(x, xpath = "capec:Typical_Severity")))})

  if (verbose) print("[.][CAPEC] Estimating likelihood of attack ...")
  att.likelihoodatt <- sapply(raw.capec.atcks,
                              function(x) {
                                ifelse(test = identical(rvest::html_text(rvest::html_nodes(x, xpath = "capec:Likelihood_Of_Attack")), character(0)),
                                       yes = "Unknown",
                                       no = rvest::html_text(rvest::html_nodes(x, xpath = "capec:Likelihood_Of_Attack")))})

  if (verbose) print("[.][CAPEC] Extracting execution flow ...")
  att.flow <- sapply(raw.capec.atcks,
                     function(x) {
                       k <- rvest::html_nodes(x, xpath = "capec:Execution_Flow")
                       k <- lapply(xml2::xml_children(k), xml2::as_list)
                       jsonlite::toJSON(k, pretty = T)
                     })

  if (verbose) print("[.][CAPEC] Identifying required skills ...")
  att.skills <- sapply(raw.capec.atcks,
                       function(x) {
                         jsonlite::toJSON(data.frame(
                           skilled = sapply(rvest::html_nodes(x, xpath = "capec:Skills_Required/capec:Skill/@Level"),
                                            xml2::xml_text),
                           skill = sapply(rvest::html_nodes(x, xpath = "capec:Skills_Required/capec:Skill"),
                                          xml2::xml_text), stringsAsFactors = FALSE))
                       })

  if (verbose) print("[.][CAPEC] Listing required resources ...")
  att.reqsrcs <- sapply(raw.capec.atcks,
                              function(x)
                                jsonlite::toJSON(rvest::html_text(rvest::xml_nodes(x, xpath = "capec:Resources_Required/capec:Resource"))))

  if (verbose) print("[.][CAPEC] Verifying indicators ...")
  att.indicators <- sapply(raw.capec.atcks,
                        function(x)
                          jsonlite::toJSON(rvest::html_text(rvest::xml_nodes(x, xpath = "capec:Indicators/capec:Indicator"))))

  if (verbose) print("[.][CAPEC] Writting consequences ...")
  att.consequences <- sapply(raw.capec.atcks,
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

  if (verbose) print("[.][CAPEC] Detecting Mitigations ...")
  att.mitigations <- sapply(raw.capec.atcks,
                              function(x)
                                jsonlite::toJSON(rvest::html_text(rvest::xml_nodes(x, xpath = "capec:Mitigations/capec:Mitigation"))))

  if (verbose) print("[.][CAPEC] Collecting Example Instances ...")
  att.examples <- sapply(raw.capec.atcks,
                            function(x)
                              jsonlite::toJSON(rvest::html_text(rvest::xml_nodes(x, xpath = "capec:Example_Instances/capec:Example"))))

  if (verbose) print("[.][CAPEC] Adding CVE examples to network ...")
  capec2cve <- lapply(att.examples,
                      function(x)
                        data.frame(to = stringr::str_extract_all(x, "CVE-\\d+-\\d+")[[1]],
                                   stringsAsFactors = FALSE))
  names(capec2cve) <- att.id
  capec2cve <- dplyr::bind_rows(capec2cve, .id = "from")
  capec2cve$label <- rep("example", nrow(capec2cve))
  capec2cve$title <- capec2cve$label
  capecnet <- rbind(capecnet, capec2cve)

  if (verbose) print("[.][CAPEC] Looking for related CWEs ...")
  capec2cwe <- lapply(raw.capec.atcks,
                      function(x)
                        data.frame(to = paste0("CWE-", xml2::xml_text(rvest::xml_nodes(x, xpath = "capec:Related_Weaknesses/capec:Related_Weakness/@CWE_ID"))),
                                   stringsAsFactors = FALSE))
  names(capec2cwe) <- att.id
  capec2cwe <- dplyr::bind_rows(capec2cwe, .id = "from")
  capec2cwe$label <- rep("take advantage of", nrow(capec2cwe))
  capec2cwe$title <- capec2cwe$label
  capecnet <- rbind(capecnet, capec2cwe)


  if (verbose) print("[.][CAPEC] Adding Taxonomy Mappings as network edges ...")
  kk <- lapply(raw.capec.atcks,
               function(x) {
                 kl <- rvest::xml_nodes(x, xpath = "capec:Taxonomy_Mappings/capec:Taxonomy_Mapping")
                 k<-lapply(kl,
                   function(y) {
                     eids <- xml2::xml_text(rvest::xml_nodes(y, xpath="capec:Entry_ID"))
                     enam <- xml2::xml_text(rvest::xml_nodes(y,xpath="capec:Entry_Name"))
                     data.frame(taxonomy_id=ifelse(identical(eids, character(0)), NA, eids),
                                taxonomy_name=ifelse(identical(enam, character(0)), NA, enam),
                                stringsAsFactors = FALSE)
                     })
                 names(k) <- xml2::xml_attr(kl, "Taxonomy_Name")
                 c2t <- dplyr::bind_rows(k, .id = "taxonomy")
                 c2t
                 })
  names(kk) <- att.id
  capec2other <- dplyr::bind_rows(kk, .id = "from")
  rm(kk)
  selected <- which(capec2other$taxonomy == "ATTACK")
  capec2other$taxonomy_id[selected] <- paste0("T", capec2other$taxonomy_id[selected])
  selected <- which(capec2other$taxonomy == "WASC")
  capec2other$taxonomy_id[selected] <- paste0("WASC-", capec2other$taxonomy_id[selected])
  selected <- which(capec2other$taxonomy == "OWASP Attacks")
  capec2other$taxonomy_id[selected] <- paste0("OWASP-", capec2other$taxonomy_name[selected])
  names(capec2other) <- c("from", "label", "to", "title")

  capecnet <- dplyr::bind_rows(capecnet, capec2other)

  if (verbose) print("[.][CAPEC] Building tidy CAPEC data set ...")
  capec <- data.frame(id = att.id,
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

  capecnodes <- dplyr::bind_rows(capec[, c("id", "name", "status", "description")],
                                 categories[,c("id", "name", "status", "description")],
                                 views[,c("id", "name", "status", "description")])
  names(capecnodes) <- c("label", "title", "shadow", "descr")
  capecnodes$title <- paste0("<p><b>", capecnodes$title, "</b>")
  capecnodes$descr <- paste0("<br>", capecnodes$descr, "</p>")
  capecnodes <- tidyr::unite(capecnodes, col = "title", "title", "descr", sep = "")
  capecnodes$color <- rep("aquamarine", nrow(capecnodes))
  capecnodes$shape <- rep("diamond", nrow(capecnodes))
  capecnodes$value <- rep(5, nrow(capecnodes))
  capecnodes$team <- rep("RED", nrow(capecnodes))
  capecnodes$group <- rep("capec", nrow(capecnodes))
  capecnodes$shadow <- capecnodes$shadow == "Deprecated"
  capecnodes$id <- capecnodes$label

  capecedges <- capecnet
  capecedges$arrows <- rep("to", rep(nrow(capecedges)))
  capecedges$team <- rep("RED", rep(nrow(capecedges)))
  capecedges$dashes <- rep(FALSE, rep(nrow(capecedges)))


  mitre.capec <- list(views = views,
                      categories = categories,
                      capec = capec,
                      capecnet = list(nodes = capecnodes,
                                      edges = capecedges))

  return(mitre.capec)
}


