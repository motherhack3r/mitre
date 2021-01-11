#### References: https://capec.mitre.org/data/index.html

#' Title
#'
#' @param capec.file
#' @param verbose
#'
#' @return
#' @export
#'
#' @examples
getCAPECData <- function(verbose = FALSE) {
  capec.file <- "data-raw/capec_latest.xml"
  if (verbose) print("Indexing CAPEC XML raw file ...")
  doc <- xml2::read_xml(capec.file)

  # CAPEC VIEWs
  if (verbose) print("Parsing CAPEC Views ...")
  views <-
    data.frame(id = rvest::html_text(rvest::xml_nodes(doc, xpath = "//capec:View/@ID")),
               name = rvest::html_text(rvest::xml_nodes(doc, xpath = "//capec:View/@Name")),
               type = rvest::html_text(rvest::xml_nodes(doc, xpath = "//capec:View/@Type")),
               status = rvest::html_text(rvest::xml_nodes(doc, xpath = "//capec:View/@Status")),
               description = rvest::html_text(rvest::xml_nodes(doc, xpath = "//capec:View/capec:Objective")),
               stringsAsFactors = FALSE)
  views$id <- paste0("CAPEC-", views$id)

  # CAPEC CATEGORIES
  if (verbose) print("Parsing CAPEC Categories ...")
  categories <-
    data.frame(id = rvest::html_text(rvest::xml_nodes(doc, xpath = "//capec:Category/@ID")),
               name = rvest::html_text(rvest::xml_nodes(doc, xpath = "//capec:Category/@Name")),
               status = rvest::html_text(rvest::xml_nodes(doc, xpath = "//capec:Category/@Status")),
               description = rvest::html_text(rvest::xml_nodes(doc, xpath = "//capec:Category/capec:Summary")),
               stringsAsFactors = FALSE)
  categories$id <- paste0("CAPEC-", categories$id)


  if (verbose) print("Adding Taxonomy Mappings as network edges ...")
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
  if (verbose) print("Parsing CAPEC Patterns ...")
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

  if (verbose) print("Extracting relatec attack patterns ...")
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

  if (verbose) print("Compacting pre-requisites ...")
  att.prerequisites <- sapply(raw.capec.atcks,
                              function(x)
                                jsonlite::toJSON(rvest::html_text(rvest::xml_nodes(x, xpath = "capec:Prerequisites/capec:Prerequisite"))))

  if (verbose) print("Selecting alternate terms ...")
  att.alernate_terms <- sapply(raw.capec.atcks,
                              function(x)
                                jsonlite::toJSON(rvest::html_text(rvest::xml_nodes(x, xpath = "capec:Alternate_Terms/capec:Alternate_Term/capec:Term"))))


  if (verbose) print("Calculating typical severity ...")
  att.severity <- sapply(raw.capec.atcks,
                         function(x) {
                           ifelse(test = identical(rvest::html_text(rvest::html_nodes(x, xpath = "capec:Typical_Severity")), character(0)),
                                  yes = "Unknown",
                                  no = rvest::html_text(rvest::html_nodes(x, xpath = "capec:Typical_Severity")))})

  if (verbose) print("Estimating likelihood of attack ...")
  att.likelihoodatt <- sapply(raw.capec.atcks,
                              function(x) {
                                ifelse(test = identical(rvest::html_text(rvest::html_nodes(x, xpath = "capec:Likelihood_Of_Attack")), character(0)),
                                       yes = "Unknown",
                                       no = rvest::html_text(rvest::html_nodes(x, xpath = "capec:Likelihood_Of_Attack")))})

  if (verbose) print("Extracting execution flow ...")
  att.flow <- sapply(raw.capec.atcks,
                     function(x) {
                       k <- rvest::html_nodes(x, xpath = "capec:Execution_Flow")
                       k <- lapply(xml2::xml_children(k), xml2::as_list)
                       jsonlite::toJSON(k, pretty = T)
                     })

  if (verbose) print("Identifying required skills ...")
  att.skills <- sapply(raw.capec.atcks,
                       function(x) {
                         jsonlite::toJSON(data.frame(
                           skilled = sapply(rvest::html_nodes(x, xpath = "capec:Skills_Required/capec:Skill/@Level"),
                                            xml2::xml_text),
                           skill = sapply(rvest::html_nodes(x, xpath = "capec:Skills_Required/capec:Skill"),
                                          xml2::xml_text), stringsAsFactors = FALSE))
                       })

  if (verbose) print("Listing required resources ...")
  att.reqsrcs <- sapply(raw.capec.atcks,
                              function(x)
                                jsonlite::toJSON(rvest::html_text(rvest::xml_nodes(x, xpath = "capec:Resources_Required/capec:Resource"))))

  if (verbose) print("Verifying indicators ...")
  att.indicators <- sapply(raw.capec.atcks,
                        function(x)
                          jsonlite::toJSON(rvest::html_text(rvest::xml_nodes(x, xpath = "capec:Indicators/capec:Indicator"))))

  if (verbose) print("Writting consequences ...")
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

  if (verbose) print("Detecting Mitigations ...")
  att.mitigations <- sapply(raw.capec.atcks,
                              function(x)
                                jsonlite::toJSON(rvest::html_text(rvest::xml_nodes(x, xpath = "capec:Mitigations/capec:Mitigation"))))

  if (verbose) print("Collecting Example Instances ...")
  att.examples <- sapply(raw.capec.atcks,
                            function(x)
                              jsonlite::toJSON(rvest::html_text(rvest::xml_nodes(x, xpath = "capec:Example_Instances/capec:Example"))))

  if (verbose) print("Adding CVE examples to network ...")
  capec2cve <- lapply(att.examples,
                      function(x)
                        data.frame(to = stringr::str_extract_all(x, "CVE-\\d+-\\d+")[[1]],
                                   stringsAsFactors = FALSE))
  names(capec2cve) <- att.id
  capec2cve <- dplyr::bind_rows(capec2cve, .id = "from")
  capec2cve$label <- rep("example", nrow(capec2cve))
  capec2cve$title <- capec2cve$label
  capecnet <- rbind(capecnet, capec2cve)

  if (verbose) print("Looking for related CWEs ...")
  capec2cwe <- lapply(raw.capec.atcks,
                      function(x)
                        data.frame(to = paste0("CWE-", xml2::xml_text(rvest::xml_nodes(x, xpath = "capec:Related_Weaknesses/capec:Related_Weakness/@CWE_ID"))),
                                   stringsAsFactors = FALSE))
  names(capec2cwe) <- att.id
  capec2cwe <- dplyr::bind_rows(capec2cwe, .id = "from")
  capec2cwe$label <- rep("take advantage of", nrow(capec2cwe))
  capec2cwe$title <- capec2cwe$label
  capecnet <- rbind(capecnet, capec2cwe)


  if (verbose) print("Adding Taxonomy Mappings as network edges ...")
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

  if (verbose) print("Building tidy CAPEC data set ...")
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
  capecnodes <- tidyr::unite(capecnodes, col = "title", title, descr, sep = "")
  capecnodes$color <- rep("aquamarine", nrow(capecnodes))
  capecnodes$shape <- rep("diamond", nrow(capecnodes))
  capecnodes$group <- rep("capec", nrow(capecnodes))
  capecnodes$team <- rep("RED", nrow(capecnodes))
  capecnodes$shadow <- capecnodes$shadow == "Deprecated"

  capecedges <- capecnet
  capecedges$arrows <- rep("to", rep(nrow(capecedges)))

  mitre.capec <- list(views = views,
                      categories = categories,
                      capec = capec,
                      capecnet = list(nodes = capecnodes,
                                      edges = capecedges))

  return(mitre.capec)
}




#
# GetCAPECData <- function(capec.file = "data-raw/capec_latest.xml", verbose = TRUE) {
#   capec.url  <- "https://capec.mitre.org/data/xml/capec_latest.xml"
#   utils::download.file(url = capec.url, destfile = "data-raw/capec_latest.xml")
#
#   doc <- xml2::read_xml(capec.file)
#
#   if (verbose) print("Parsing CAPEC Views...")
#   capec.views <- ParseCAPECData.views(doc)
#   if (verbose) print("Parsing CAPEC Categories...")
#   capec.categories <- ParseCAPECData.categories(doc)
#   if (verbose) print("Parsing CAPEC Attacks...")
#   capec.attacks <- ParseCAPECData.attacks(doc, verbose)
#
#   # TODO: Unify data.frames
#   # capec <- list(views = capec.views,
#   #               categories = capec.categories,
#   #               attacks = capec.attacks)
#   print(paste("CAPEC data frame building process finished."))
#   return(capec.attacks)
# }
#
# #### Private Functions -----------------------------------------------------------------------------
#
# ParseCAPECData.attacks <- function(doc, verbose = TRUE) {
#   # if (verbose) print("Parsing Attacks basic attributes ...")
#   # raw.capec.atcks <- rvest::xml_nodes(doc, xpath = "//capec:Attack_Pattern")
#   # att.id <- rvest::html_text(rvest::xml_nodes(doc, xpath = "//capec:Attack_Pattern/@ID"))
#   # att.name <- rvest::html_text(rvest::xml_nodes(doc, xpath = "//capec:Attack_Pattern/@Name"))
#   # att.status <- rvest::html_text(rvest::xml_nodes(doc, xpath = "//capec:Attack_Pattern/@Status"))
#   # att.abstraction <- rvest::html_text(rvest::xml_nodes(doc, xpath = "//capec:Attack_Pattern/@Abstraction"))
#   # att.descr <- sapply(raw.capec.atcks, function(x) rvest::html_text(rvest::xml_nodes(x, xpath = "capec:Description")))
#   #
#   # kk <- lapply(raw.capec.atcks,
#   #              function(x) {
#   #                data.frame(id = rvest::html_text(rvest::xml_nodes(x, xpath=".//capec:Related_Attack_Pattern/@CAPEC_ID")),
#   #                           nature = rvest::html_text(rvest::xml_nodes(x, xpath=".//capec:Related_Attack_Pattern/@Nature")),
#   #                           stringsAsFactors = F)
#   #              })
#   # capec.ids <- paste0("CAPEC-", att.id)
#   # names(kk) <- capec.ids
#   # c2c <- dplyr::bind_rows(kk, .id = "from")
#   #
#   #
#   #
#   # if (verbose) print("Parsing attacks prerequisites ...")
#   # if (verbose) {utils::setTxtProgressBar(pb, i); i <- i + 1}
#   # att.attack.prerequisites <- sapply(raw.capec.atcks, function(x) RJSONIO::toJSON(rvest::html_text(rvest::xml_nodes(x, xpath = "capec:Attack_Prerequisites/capec:Attack_Prerequisite/capec:Text"))))
#   # if (verbose) print("Parsing attacks severity ...")
#   # if (verbose) {utils::setTxtProgressBar(pb, i); i <- i + 1}
#   # att.severity <- sapply(raw.capec.atcks,
#   #                        function(x) {ifelse(test = identical(rvest::html_text(rvest::html_nodes(x, xpath = "capec:Typical_Severity")), character(0)),
#   #                                            yes = "",
#   #                                            no = rvest::html_text(rvest::html_nodes(x, xpath = "capec:Typical_Severity")))})
#   # if (verbose) print("Parsing exploitability info ...")
#   # att.likelihood.exploit <- sapply(raw.capec.atcks,
#   #                                  function(x) {ifelse(test = identical(rvest::html_text(rvest::html_nodes(x, xpath = "capec:Typical_Likelihood_of_Exploit/capec:Likelihood")), character(0)),
#   #                                                      yes = "",
#   #                                                      no = rvest::html_text(rvest::html_nodes(x, xpath = "capec:Typical_Likelihood_of_Exploit/capec:Likelihood")))})
#   # att.likelihood.exploit.descr <- sapply(raw.capec.atcks,
#   #                                        function(x) {ifelse(test = identical(rvest::html_text(rvest::html_nodes(x, xpath = "capec:Typical_Likelihood_of_Exploit/capec:Explanation")), character(0)),
#   #                                                            yes = "",
#   #                                                            no = rvest::html_text(rvest::html_nodes(x, xpath = "capec:Typical_Likelihood_of_Exploit/capec:Explanation")))})
#   if (verbose) print("Parsing methods of attack and cve examples ...")
#   if (verbose) {utils::setTxtProgressBar(pb, i); i <- i + 1}
#   att.methods.of.attack <- sapply(raw.capec.atcks, function(x) RJSONIO::toJSON(rvest::html_text(rvest::xml_nodes(x, xpath = "capec:Methods_of_Attack/capec:Method_of_Attack"))))
#   att.examples.cves <- sapply(raw.capec.atcks,
#                               function(x) {ifelse(test = identical(rvest::html_text(rvest::html_nodes(x, xpath = "capec:Examples-Instances/capec:Example-Instance/capec:Example-Instance_Related_Vulnerabilities")), character(0)),
#                                                   yes = "[]",
#                                                   no = RJSONIO::toJSON(rvest::html_text(rvest::html_nodes(x, xpath = "capec:Examples-Instances/capec:Example-Instance/capec:Example-Instance_Related_Vulnerabilities"))))})
#   if (verbose) print("Parsing hacking skills and resources required ...")
#   if (verbose) {utils::setTxtProgressBar(pb, i); i <- i + 1}
#   att.hack.skills <- sapply(raw.capec.atcks,
#                             function(x) {ifelse(test = identical(rvest::html_text(rvest::html_nodes(x, xpath = "capec:Attacker_Skills_or_Knowledge_Required/capec:Attacker_Skill_or_Knowledge_Required")), character(0)),
#                                                 yes = "[]",
#                                                 no = RJSONIO::toJSON(xml2::as_list(rvest::html_nodes(x, xpath = "capec:Attacker_Skills_or_Knowledge_Required/capec:Attacker_Skill_or_Knowledge_Required"))))})
#   att.resources.required <- sapply(raw.capec.atcks, function(x) RJSONIO::toJSON(rvest::html_text(rvest::xml_nodes(x, xpath = "capec:Resources_Required/capec:Text"))))
#   if (verbose) print("Parsing proving and obfuscation techniques, also indicators of attack ...")
#   if (verbose) {utils::setTxtProgressBar(pb, i); i <- i + 1}
#   att.proving.techniques <- sapply(raw.capec.atcks,
#                                    function(x) {ifelse(test = identical(rvest::html_text(rvest::html_nodes(x, xpath = "capec:Probing_Techniques/capec:Probing_Technique")), character(0)),
#                                                        yes = "[]",
#                                                        no = RJSONIO::toJSON(rvest::html_text(rvest::html_nodes(x, xpath = "capec:Probing_Techniques/capec:Probing_Technique"))))})
#   att.indicators.warnings.of.Attack <- sapply(raw.capec.atcks,
#                                               function(x) {ifelse(test = identical(rvest::html_text(rvest::html_nodes(x, xpath = "capec:Indicators-Warnings_of_Attack/capec:Indicator-Warning_of_Attack")), character(0)),
#                                                                   yes = "[]",
#                                                                   no = RJSONIO::toJSON(rvest::html_text(rvest::html_nodes(x, xpath = "capec:Indicators-Warnings_of_Attack/capec:Indicator-Warning_of_Attack"))))})
#   att.obfuscation.techniques <- sapply(raw.capec.atcks,
#                                        function(x) {ifelse(test = identical(rvest::html_text(rvest::html_nodes(x, xpath = "capec:Obfuscation_Techniques/capec:Obfuscation_Technique")), character(0)),
#                                                            yes = "[]",
#                                                            no = RJSONIO::toJSON(rvest::html_text(rvest::html_nodes(x, xpath = "capec:Obfuscation_Techniques/capec:Obfuscation_Technique"))))})
#   if (verbose) print("Parsing solutions and mitigations ...")
#   if (verbose) {utils::setTxtProgressBar(pb, i); i <- i + 1}
#   att.solutions.mitigations <- sapply(raw.capec.atcks,
#                                       function(x) {ifelse(test = identical(rvest::html_text(rvest::html_nodes(x, xpath = "capec:Solutions_and_Mitigations/capec:Solution_or_Mitigation")), character(0)),
#                                                           yes = "",
#                                                           no = RJSONIO::toJSON(rvest::html_text(rvest::html_nodes(x, xpath = "capec:Solutions_and_Mitigations/capec:Solution_or_Mitigation"))))})
#   if (verbose) print("Parsing motivation consequences ...")
#   if (verbose) {utils::setTxtProgressBar(pb, i); i <- i + 1}
#   att.attack.motivation.consequences <- sapply(raw.capec.atcks,
#                                                function(x) {ifelse(test = identical(rvest::html_text(rvest::html_nodes(x, xpath = "capec:Attack_Motivation-Consequences/capec:Attack_Motivation-Consequence")), character(0)),
#                                                                    yes = "[]",
#                                                                    no = {
#                                                                      RJSONIO::toJSON(lapply(rvest::html_nodes(x, xpath = "capec:Attack_Motivation-Consequences/capec:Attack_Motivation-Consequence"),
#                                                                                             function(y) {
#                                                                                               con <- list(
#                                                                                                 scope = rvest::html_text(rvest::html_nodes(y, xpath = "capec:Consequence_Scope")),
#                                                                                                 impact = rvest::html_text(rvest::html_nodes(y, xpath = "capec:Consequence_Technical_Impact")),
#                                                                                                 note = rvest::html_text(rvest::html_nodes(y, xpath = "capec:Consequence_Note"))
#                                                                                               )
#                                                                                               con <- con[sapply(con, function(x) !identical(x, character(0)))]
#                                                                                               con
#                                                                                             }))
#                                                                    })})
#   if (verbose) print("Parsing injection vector, activation zone and payload info ...")
#   if (verbose) {utils::setTxtProgressBar(pb, i); i <- i + 1}
#   att.injection.vector <- sapply(raw.capec.atcks,
#                                  function(x) {ifelse(test = identical(rvest::html_text(rvest::html_nodes(x, xpath = "capec:Injection_Vector/capec:Text")), character(0)),
#                                                      yes = "[]",
#                                                      no = RJSONIO::toJSON(rvest::html_text(rvest::html_nodes(x, xpath = "capec:Injection_Vector/capec:Text"))))})
#   att.payload <- sapply(raw.capec.atcks,
#                         function(x) {ifelse(test = identical(rvest::html_text(rvest::html_nodes(x, xpath = "capec:Payload/capec:Text")), character(0)),
#                                             yes = "[]",
#                                             no = RJSONIO::toJSON(rvest::html_text(rvest::html_nodes(x, xpath = "capec:Payload/capec:Text"))))})
#   att.activation.zone <- sapply(raw.capec.atcks,
#                                 function(x) {ifelse(test = identical(rvest::html_text(rvest::html_nodes(x, xpath = "capec:Activation_Zone/capec:Text")), character(0)),
#                                                     yes = "[]",
#                                                     no = RJSONIO::toJSON(rvest::html_text(rvest::html_nodes(x, xpath = "capec:Activation_Zone/capec:Text"))))})
#   att.payload.activation.impact <- sapply(raw.capec.atcks,
#                                           function(x) {ifelse(test = identical(rvest::html_text(rvest::html_nodes(x, xpath = "capec:Payload_Activation_Impact/capec:Description/capec:Text")), character(0)),
#                                                               yes = "[]",
#                                                               no = RJSONIO::toJSON(rvest::html_text(rvest::html_nodes(x, xpath = "capec:Payload_Activation_Impact/capec:Description/capec:Text"))))})
#   if (verbose) print("Parsing related CWE, CVE, CAPEC and other standards ...")
#   if (verbose) {utils::setTxtProgressBar(pb, i); i <- i + 1}
#   att.related.cwe.target <- sapply(raw.capec.atcks,
#                                    function(x) {ifelse(test = identical(rvest::html_text(rvest::html_nodes(x, xpath = "capec:Related_Weaknesses/capec:Related_Weakness/capec:CWE_ID")), character(0)),
#                                                        yes = "[]",
#                                                        no = RJSONIO::toJSON(rvest::html_text(rvest::html_nodes(x, xpath = "capec:Related_Weaknesses/capec:Related_Weakness/capec:CWE_ID[../capec:Weakness_Relationship_Type/text() = 'Targeted']/text()"))))})
#   if (verbose) {utils::setTxtProgressBar(pb, i); i <- i + 1}
#   att.related.cwe.second <- sapply(raw.capec.atcks,
#                                    function(x) {ifelse(test = identical(rvest::html_text(rvest::html_nodes(x, xpath = "capec:Related_Weaknesses/capec:Related_Weakness/capec:CWE_ID")), character(0)),
#                                                        yes = "[]",
#                                                        no = RJSONIO::toJSON(rvest::html_text(rvest::html_nodes(x, xpath = "capec:Related_Weaknesses/capec:Related_Weakness/capec:CWE_ID[../capec:Weakness_Relationship_Type/text() = 'Secondary']/text()"))))})
#   if (verbose) {utils::setTxtProgressBar(pb, i); i <- i + 1}
#   att.related.cves <- sapply(raw.capec.atcks,
#                              function(x) {ifelse(test = identical(rvest::html_text(rvest::html_nodes(x, xpath = "capec:Related_Vulnerabilities/capec:Related_Vulnerability")), character(0)),
#                                                  yes = "[]",
#                                                  no = {
#                                                    RJSONIO::toJSON(
#                                                      lapply(
#                                                        rvest::html_nodes(x, xpath = "capec:Related_Vulnerabilities/capec:Related_Vulnerability"),
#                                                        function(y) {
#                                                          vulns <- rvest::html_text(rvest::xml_nodes(y, xpath = "capec:Vulnerability_Description/capec:Text"))
#                                                          if (identical(vulns, character(0))) vulns <- ""
#                                                          names(vulns) <- rvest::html_text(rvest::xml_nodes(y, xpath = "capec:Vulnerability_ID"))
#                                                          vulns
#                                                        }
#                                                      )
#                                                    )
#                                                  }
#                              )
#                              }
#   )
#   if (verbose) {utils::setTxtProgressBar(pb, i); i <- i + 1}
#   att.related.capec <- sapply(raw.capec.atcks,
#                               function(x) {ifelse(test = identical(rvest::html_text(rvest::html_nodes(x, xpath = "capec:Related_Attack_Patterns/capec:Related_Attack_Pattern")), character(0)),
#                                                   yes = "[]",
#                                                   no = RJSONIO::toJSON(rvest::html_text(rvest::html_nodes(x, xpath = "capec:Related_Attack_Patterns/capec:Related_Attack_Pattern/capec:Relationship_Target_ID"))))})
#   if (verbose) {utils::setTxtProgressBar(pb, i); i <- i + 1}
#   att.related.attack.patterns <- sapply(raw.capec.atcks,
#                                         function(x) {ifelse(test = identical(rvest::html_text(rvest::html_nodes(x, xpath = "capec:Related_Attack_Patterns/capec:Related_Attack_Pattern")), character(0)),
#                                                             yes = "[]",
#                                                             no = {
#                                                               RJSONIO::toJSON(lapply(rvest::html_nodes(x, xpath = "capec:Related_Attack_Patterns/capec:Related_Attack_Pattern"),
#                                                                                      function(y) {
#                                                                                        con <- list(
#                                                                                          related.view = rvest::html_text(rvest::html_nodes(y, xpath = "capec:Relationship_Views/capec:Relationship_View_ID")),
#                                                                                          target.form = rvest::html_text(rvest::html_nodes(y, xpath = "capec:Relationship_Target_Form")),
#                                                                                          nature = rvest::html_text(rvest::html_nodes(y, xpath = "capec:Relationship_Nature")),
#                                                                                          target.id = rvest::html_text(rvest::html_nodes(y, xpath = "capec:Relationship_Target_ID"))
#                                                                                        )
#                                                                                      }))
#                                                             })})
#   if (verbose) print("Parsing security requirements, principles and guidelines ...")
#   if (verbose) {utils::setTxtProgressBar(pb, i); i <- i + 1}
#   att.relevant.security.requirements <- sapply(raw.capec.atcks,
#                                                function(x) {ifelse(test = identical(rvest::html_text(rvest::html_nodes(x, xpath = "capec:Relevant_Security_Requirements/capec:Relevant_Security_Requirement/capec:Text")), character(0)),
#                                                                    yes = "[]",
#                                                                    no = RJSONIO::toJSON(rvest::html_text(rvest::html_nodes(x, xpath = "capec:Relevant_Security_Requirements/capec:Relevant_Security_Requirement/capec:Text"))))})
#   if (verbose) {utils::setTxtProgressBar(pb, i); i <- i + 1}
#   att.related.security.principles <- sapply(raw.capec.atcks,
#                                             function(x) {ifelse(test = identical(rvest::html_text(rvest::html_nodes(x, xpath = "capec:Related_Security_Principles/capec:Related_Security_Principle/capec:Text")), character(0)),
#                                                                 yes = "[]",
#                                                                 no = RJSONIO::toJSON(rvest::html_text(rvest::html_nodes(x, xpath = "capec:Related_Security_Principles/capec:Related_Security_Principle/capec:Text"))))})
#   if (verbose) {utils::setTxtProgressBar(pb, i); i <- i + 1}
#   att.related.guidelines <- sapply(raw.capec.atcks,
#                                    function(x) {ifelse(test = identical(rvest::html_text(rvest::html_nodes(x, xpath = "capec:Related_Guidelines/capec:Related_Guideline/capec:Text")), character(0)),
#                                                        yes = "[]",
#                                                        no = RJSONIO::toJSON(rvest::html_text(rvest::html_nodes(x, xpath = "capec:Related_Guidelines/capec:Related_Guideline/capec:Text"))))})
#   if (verbose) print("Parsing purposes ...")
#   if (verbose) {utils::setTxtProgressBar(pb, i); i <- i + 1}
#   att.purposes <- sapply(raw.capec.atcks,
#                          function(x) {ifelse(test = identical(rvest::html_text(rvest::html_nodes(x, xpath = "capec:Purposes/capec:Purpose")), character(0)),
#                                              yes = "[]",
#                                              no = RJSONIO::toJSON(rvest::html_text(rvest::html_nodes(x, xpath = "capec:Purposes/capec:Purpose"))))})
#   if (verbose) print("Parsing impact CIA values ...")
#   if (verbose) {utils::setTxtProgressBar(pb, i); i <- i + 1}
#   att.impact.confidentiality <- sapply(raw.capec.atcks,
#                                        function(x) {ifelse(test = identical(rvest::html_text(rvest::html_nodes(x, xpath = "capec:CIA_Impact/capec:Confidentiality_Impact")), character(0)),
#                                                            yes = "",
#                                                            no = rvest::html_text(rvest::html_nodes(x, xpath = "capec:CIA_Impact/capec:Confidentiality_Impact")))})
#   if (verbose) {utils::setTxtProgressBar(pb, i); i <- i + 1}
#   att.impact.integrity <- sapply(raw.capec.atcks,
#                                  function(x) {ifelse(test = identical(rvest::html_text(rvest::html_nodes(x, xpath = "capec:CIA_Impact/capec:Integrity_Impact")), character(0)),
#                                                      yes = "",
#                                                      no = rvest::html_text(rvest::html_nodes(x, xpath = "capec:CIA_Impact/capec:Integrity_Impact")))})
#   if (verbose) {utils::setTxtProgressBar(pb, i); i <- i + 1}
#   att.impact.availability <- sapply(raw.capec.atcks,
#                                     function(x) {ifelse(test = identical(rvest::html_text(rvest::html_nodes(x, xpath = "capec:CIA_Impact/capec:Availability_Impact")), character(0)),
#                                                         yes = "",
#                                                         no = rvest::html_text(rvest::html_nodes(x, xpath = "capec:CIA_Impact/capec:Availability_Impact")))})
#   if (verbose) print("Parsing context technical architectures, frameworks, platforms and languages ...")
#   if (verbose) {utils::setTxtProgressBar(pb, i); i <- i + 1}
#   att.tech.architectural.paradigms <- sapply(raw.capec.atcks,
#                                              function(x) {ifelse(test = identical(rvest::html_text(rvest::html_nodes(x, xpath = "capec:Technical_Context/capec:Architectural_Paradigms/capec:Architectural_Paradigm")), character(0)),
#                                                                  yes = "[]",
#                                                                  no = RJSONIO::toJSON(rvest::html_text(rvest::html_nodes(x, xpath = "capec:Technical_Context/capec:Architectural_Paradigms/capec:Architectural_Paradigm"))))})
#   if (verbose) {utils::setTxtProgressBar(pb, i); i <- i + 1}
#   att.tech.frameworks <- sapply(raw.capec.atcks,
#                                 function(x) {ifelse(test = identical(rvest::html_text(rvest::html_nodes(x, xpath = "capec:Technical_Context/capec:Frameworks/capec:Framework")), character(0)),
#                                                     yes = "[]",
#                                                     no = RJSONIO::toJSON(rvest::html_text(rvest::html_nodes(x, xpath = "capec:Technical_Context/capec:Frameworks/capec:Framework"))))})
#   if (verbose) {utils::setTxtProgressBar(pb, i); i <- i + 1}
#   att.tech.platforms <- sapply(raw.capec.atcks,
#                                function(x) {ifelse(test = identical(rvest::html_text(rvest::html_nodes(x, xpath = "capec:Technical_Context/capec:Platforms/capec:Platform")), character(0)),
#                                                    yes = "[]",
#                                                    no = RJSONIO::toJSON(rvest::html_text(rvest::html_nodes(x, xpath = "capec:Technical_Context/capec:Platforms/capec:Platform"))))})
#   if (verbose) {utils::setTxtProgressBar(pb, i); i <- i + 1}
#   att.tech.languages <- sapply(raw.capec.atcks,
#                                function(x) {ifelse(test = identical(rvest::html_text(rvest::html_nodes(x, xpath = "capec:Technical_Context/capec:Languages/capec:Language")), character(0)),
#                                                    yes = "[]",
#                                                    no = RJSONIO::toJSON(rvest::html_text(rvest::html_nodes(x, xpath = "capec:Technical_Context/capec:Languages/capec:Language"))))})
#   if (verbose) print("Parsing references, books, links ...")
#   if (verbose) {utils::setTxtProgressBar(pb, i); i <- i + 1}
#   att.references <- sapply(raw.capec.atcks,
#                            function(x) {ifelse(test = identical(rvest::html_text(rvest::html_nodes(x, xpath = "capec:Related_Attack_Patterns/capec:Related_Attack_Pattern")), character(0)),
#                                                yes = "[]",
#                                                no = {
#                                                  RJSONIO::toJSON(lapply(rvest::html_nodes(x, xpath = "capec:References/capec:Reference"),
#                                                                         function(y) {
#                                                                           con <- list(
#                                                                             author = rvest::html_text(rvest::html_nodes(y, xpath = "capec:Reference_Author")),
#                                                                             title = rvest::html_text(rvest::html_nodes(y, xpath = "capec:Reference_Title")),
#                                                                             section = rvest::html_text(rvest::html_nodes(y, xpath = "capec:Reference_Section")),
#                                                                             edition = rvest::html_text(rvest::html_nodes(y, xpath = "capec:Reference_Edition")),
#                                                                             publication = rvest::html_text(rvest::html_nodes(y, xpath = "capec:Reference_Publication")),
#                                                                             publisher = rvest::html_text(rvest::html_nodes(y, xpath = "capec:Reference_Publisher")),
#                                                                             date = rvest::html_text(rvest::html_nodes(y, xpath = "capec:Reference_Date")),
#                                                                             pubDate = rvest::html_text(rvest::html_nodes(y, xpath = "capec:Reference_PubDate")),
#                                                                             link = rvest::html_text(rvest::html_nodes(y, xpath = "capec:Reference_Link"))
#                                                                           )
#                                                                           con <- con[sapply(con, function(x) !identical(x, character(0)))]
#                                                                           con
#                                                                         }))
#                                                })})
#   if (verbose) print("Building attacks tidy data.frame ...")
#   if (verbose) {utils::setTxtProgressBar(pb, i); i <- i + 1}
#
#   # Attacks Data Frame
#   attacks <- data.frame(id = att.id,
#                         name = att.name,
#                         status = as.factor(att.status),
#                         pattern.abstraction = as.factor(att.pattern.abstraction),
#                         pattern.completeness = as.factor(att.pattern.completeness),
#                         descr = att.descr,
#                         attack.prerequisites = att.attack.prerequisites,
#                         severity = as.factor(att.severity),
#                         likelihood.exploit = as.factor(att.likelihood.exploit),
#                         likelihood.exploit.descr = att.likelihood.exploit.descr,
#                         methods.of.attack = att.methods.of.attack,
#                         examples.cves = att.examples.cves,
#                         hack.skills = att.hack.skills,
#                         resources.required = att.resources.required,
#                         proving.techniques = att.proving.techniques,
#                         indicators.warnings.of.Attack = att.indicators.warnings.of.Attack,
#                         obfuscation.techniques = att.obfuscation.techniques,
#                         solutions.mitigations = att.solutions.mitigations,
#                         attack.motivation.consequences = att.attack.motivation.consequences,
#                         injection.vector = att.injection.vector,
#                         payload = att.payload,
#                         activation.zone = att.activation.zone,
#                         payload.activation.impact = att.payload.activation.impact,
#                         related.cwe.target = att.related.cwe.target,
#                         related.cwe.second = att.related.cwe.second,
#                         related.cves = att.related.cves,
#                         related.capec = att.related.capec,
#                         related.attack.patterns = att.related.attack.patterns,
#                         relevant.security.requirements = att.relevant.security.requirements,
#                         related.security.principles = att.related.security.principles,
#                         related.guidelines = att.related.guidelines,
#                         purposes = att.purposes,
#                         impact.confidentiality = as.factor(att.impact.confidentiality),
#                         impact.integrity = as.factor(att.impact.integrity),
#                         impact.availability = as.factor(att.impact.availability),
#                         tech.architectural.paradigms = att.tech.architectural.paradigms,
#                         tech.frameworks = att.tech.frameworks,
#                         tech.platforms = att.tech.platforms,
#                         tech.languages = att.tech.languages,
#                         references = att.references,
#                         stringsAsFactors = FALSE)
#   if (verbose) {utils::setTxtProgressBar(pb, i); i <- i + 1}
#
#   close(pb)
#   return(attacks)
# }
