#' ETL process that download current CVE definitions and return a list with a
#' data frame for CVE objects. The list also contains a visNetwork object with
#' CVE objects as nodes and all relations as edges.
#'
#' @param verbose Default set as FALSE
#'
#' @return data frame
getCWEData <- function(verbose = FALSE, deprecated = FALSE) {
  cwexmlfile <- dir(path = "data-raw", pattern = "^cwe.*\\.xml$")
  cwes.file <- file.path("data-raw", cwexmlfile)
  if (verbose) print(paste("[.][CWE] Indexing CWE raw file", cwexmlfile, "..."))
  doc <- rvest::read_html(cwes.file)
  cwes.weaknesses <- ParseCWEWeaknesses(doc, cwes.file, verbose)
  cwes.categories <- ParseCWECategories(doc, cwes.file, verbose)
  cwes.views <- ParseCWEViews(doc, cwes.file, verbose)
  cwes <- dplyr::bind_rows(cwes.weaknesses, cwes.categories, cwes.views)
  cwes$CWE_Type <- as.factor(cwes$CWE_Type)

  cwenet <- getCWENetwork(cwes, verbose, deprecated)
  cwe <- list(cwe = cwes, cwenet = cwenet)

  return(cwe)
}

getCWENetwork <- function(cwes, verbose, deprecated) {
  if (verbose) print("[-][CWE] Collecting CWE nodes ...")
  if (!deprecated) cwes <- cwes[!(cwes$Status %in% c("Deprecated", "Obsolete")), ]

  cwenodes <- cwes[, c("Code_Standard", "Name", "Description", "Status", "Abstraction")]
  names(cwenodes) <- c("id", "label", "title", "shadow", "group")
  cwenodes$shadow <- cwenodes$shadow %in% c("Deprecated", "Obsolete", "Incomplete")
  cwenodes$value <- rep(2, nrow(cwenodes))
  cwenodes$shape <- rep("rectangle", nrow(cwenodes))
  cwenodes$color <- rep("papayawhip", nrow(cwenodes))
  cwenodes$group <- rep("cwe", nrow(cwenodes))
  cwenodes$team <- rep("BLUE", nrow(cwenodes))

  if (verbose) print("[-][CWE] Looking for CWE to CVE edges ...")
  cwe2cve <- lapply(cwes$Observed_Examples,
                    function(x) {
                      cves <- stringr::str_extract_all(x, "CVE-\\d+-\\d+")[[1]]
                      data.frame(to = cves, stringsAsFactors = FALSE)
                    })
  names(cwe2cve) <- cwes$Code_Standard
  cwe2cve <- plyr::ldply(cwe2cve, rbind)
  names(cwe2cve) <- c("from", "to")
  cwe2cve <- cwe2cve[stats::complete.cases(cwe2cve), ]
  cwe2cve$team <- rep("SYSADMIN", nrow(cwe2cve))
  cwe2cve$label <- rep("example", nrow(cwe2cve))
  cwe2cve$dashes <- rep(FALSE, nrow(cwe2cve))
  cwe2cve$arrows <- rep("to", nrow(cwe2cve))
  cwe2cve$title <- rep("vulnerability example", nrow(cwe2cve))

  if (verbose) print("[-][CWE] Looking for CWE to CWE edges ...")
  cwe2cwe <- cwes[, c("Code_Standard","Related_Weakness")]
  cwe2cwe <- cwe2cwe[stats::complete.cases(cwe2cwe), ]

  kk <- lapply(cwe2cwe$Related_Weakness,
               function(x) {
                 k <- RJSONIO::fromJSON(x)
                 if (length(k) == 1) {
                   k <- as.data.frame.array(t(k[[1]]))
                 } else {
                   k <- dplyr::bind_rows(lapply(k, function(x) as.data.frame(t(x))))
                   # names(k) <- c("nature", "cwe_id", "view_id", "ordinal")
                 }
                 k
               })
  names(kk) <- cwe2cwe$Code_Standard
  cwe2cwe <- dplyr::bind_rows(kk, .id = "from")
  names(cwe2cwe) <- c("from", "label", "to", "view_id", "dashes", "chain_id")
  cwe2cwe$label[which(is.na(cwe2cwe$label))] <- "HasMember"
  cwe2cwe$to <- paste0("CWE-", cwe2cwe$to)
  cwe2cwe$dashes <- is.na(cwe2cwe$dashes)
  cwe2cwe$arrows <- rep("to", nrow(cwe2cwe))
  cwe2cwe$team <- rep("BLUE", nrow(cwe2cwe))
  cwe2cwe$chain_id <- NULL
  cwe2cwe$title <- cwe2cwe$label

  if (verbose) print("[-][CWE] Finding relations from CWE to CAPEC ...")
  cwe2capec <- cwes[, c("Code_Standard", "Related_Attack_Patterns")]
  cwe2capec <- cwe2capec[stats::complete.cases(cwe2capec), ]

  kk <- lapply(cwe2capec$Related_Attack_Patterns,
               function(x)
                 data.frame(to = paste0("CAPEC-", RJSONIO::fromJSON(x)),
                            stringsAsFactors = FALSE))
  names(kk) <- cwe2capec$Code_Standard
  cwe2capec <- dplyr::bind_rows(kk, .id = "from")
  cwe2capec <- cwe2capec[stats::complete.cases(cwe2capec), ]
  cwe2capec$team <- rep("BLUE", nrow(cwe2capec))
  cwe2capec$label <- rep("leverage", nrow(cwe2capec))
  cwe2capec$dashes <- rep(FALSE, nrow(cwe2capec))
  cwe2capec$arrows <- rep("to", nrow(cwe2capec))
  cwe2capec$title <- rep("leverage attack", nrow(cwe2capec))

  if (verbose) print("[-][CWE] Building CWE network ...")
  cweedges <- dplyr::bind_rows(cwe2cve, cwe2cwe, cwe2capec)
  cweedges$view_id <- NULL

  cwenet <- list(nodes = cwenodes,
                 edges = cweedges)

  return(cwenet)
}

ParseCWEViews <- function(doc, cwes.file, verbose) {
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
  cwes <- dplyr::left_join(cwes, df, by = c("ID"))
  return(cwes)
}

ParseCWECategories <- function(doc, cwes.file, verbose) {
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
  cwes <- dplyr::left_join(cwes, df, by = c("ID"))
  return(cwes)
}


ParseCWEWeaknesses <- function(doc, cwes.file, verbose) {
  if (verbose) print("[.][CWE] Parsing Basic attributes...")
  raw.cwes <- rvest::html_nodes(doc, "weakness")
  # Extract Weakness node attributes
  cwes <- as.data.frame(t(sapply(raw.cwes, rvest::html_attrs)), stringsAsFactors = F)
  names(cwes) <- c("ID", "Name", "Abstraction", "Structure", "Status")
  cwes$CWE_Type <- rep("Weakness", nrow(cwes))
  # Set factors (improve setting levels according to XSD)
  cwes$Abstraction <- as.factor(cwes$Abstraction)
  cwes$Structure <- as.factor(cwes$Structure)
  # Add extra field with code standard
  cwes$Code_Standard <- paste("CWE-", cwes$ID, sep = "")

  if (verbose) print("[.][CWE] Parsing Description...")
  cwes$Description <- sapply(rvest::html_nodes(doc, xpath = "//weakness/description"),
                             rvest::html_text)
  ids <- xml2::xml_text(xml2::xml_find_all(doc, "//weakness/extended_description/parent::*/@id"))
  vals <- sapply(xml2::xml_find_all(doc, "//weakness/extended_description"), xml2::xml_text)
  df <- data.frame(ID = ids, Extended_Description = vals, stringsAsFactors = F)
  cwes <- dplyr::left_join(cwes, df, by = c("ID"))

  if (verbose) print("[.][CWE] Parsing Related Weakness...")
  ids <- xml2::xml_text(xml2::xml_find_all(doc, "//weakness/related_weaknesses/parent::*/@id"))
  vals <- sapply(xml2::xml_find_all(doc, "//weakness/related_weaknesses"),
                 function(x) RJSONIO::toJSON(lapply(rvest::html_children(x),
                                                    rvest::html_attrs),
                                             pretty = T)
  )
  df <- data.frame(ID = ids, Related_Weakness = vals, stringsAsFactors = F)
  cwes <- dplyr::left_join(cwes, df, by = c("ID"))

  if (verbose) print("[.][CWE] Parsing Weakness Ordinality...")
  ids <- xml2::xml_text(xml2::xml_find_all(doc, "//weakness/weakness_ordinalities/parent::*/@id"))
  vals <- sapply(xml2::xml_find_all(doc, "//weakness/weakness_ordinalities"),
                 function(x) RJSONIO::toJSON(lapply(rvest::html_children(x),
                                                    function(x) rvest::html_text(rvest::html_children(x))),
                                             pretty = T)
  )
  df <- data.frame(ID = ids, Weakness_Ordinality = vals, stringsAsFactors = F)
  cwes <- dplyr::left_join(cwes, df, by = c("ID"))

  if (verbose) print("[.][CWE] Parsing Applicable Platforms...")
  ids <- xml2::xml_text(xml2::xml_find_all(doc, "//weakness/applicable_platforms/parent::*/@id"))
  vals <- sapply(xml2::xml_find_all(doc, "//weakness/applicable_platforms"),
                 function(x) {
                   y <- lapply(rvest::html_children(x), rvest::html_attrs)
                   names(y) <- rvest::html_name(rvest::html_children(x))
                   RJSONIO::toJSON(y, pretty = T)
                 }
  )
  df <- data.frame(ID = ids, Applicable_Platforms = vals, stringsAsFactors = F)
  cwes <- dplyr::left_join(cwes, df, by = c("ID"))

  if (verbose) print("[.][CWE] Parsing Background Details...")
  ids <- xml2::xml_text(xml2::xml_find_all(doc, "//weakness/background_details/parent::*/@id"))
  vals <- sapply(xml2::xml_find_all(doc, "//weakness/background_details"),
                 function(x) RJSONIO::toJSON(xml2::xml_text(x),
                                             pretty = T)
  )
  df <- data.frame(ID = ids, Background_Details = vals, stringsAsFactors = F)
  cwes <- dplyr::left_join(cwes, df, by = c("ID"))

  if (verbose) print("[.][CWE] Parsing Alternate Terms...")
  ids <- xml2::xml_text(xml2::xml_find_all(doc, "//weakness/alternate_terms/parent::*/@id"))
  vals <- sapply(xml2::xml_find_all(doc, "//weakness/alternate_terms"),
                 function(x) RJSONIO::toJSON(lapply(rvest::html_children(x),
                                                    rvest::html_text),
                                             pretty = T)
  )
  df <- data.frame(ID = ids, Alternate_Terms = vals, stringsAsFactors = F)
  cwes <- dplyr::left_join(cwes, df, by = c("ID"))

  if (verbose) print("[.][CWE] Parsing Modes Of Introduction...")
  ids <- xml2::xml_text(xml2::xml_find_all(doc, "//weakness/modes_of_introduction/parent::*/@id"))
  vals <- sapply(xml2::xml_find_all(doc, "//weakness/modes_of_introduction"),
                 function(x) RJSONIO::toJSON(lapply(
                   lapply(rvest::html_children(x),
                          function(x) rvest::html_children(x)),
                   function(y) {
                     z <- rvest::html_text(y)
                     names(z) <- rvest::html_name(y)
                     z
                   }
                 ),
                 pretty = T)
  )
  df <- data.frame(ID = ids, Modes_Of_Introduction = vals, stringsAsFactors = F)
  cwes <- dplyr::left_join(cwes, df, by = c("ID"))

  if (verbose) print("[.][CWE] Parsing Likelihood Of Exploit...")
  ids <- xml2::xml_text(xml2::xml_find_all(doc, "//weakness/likelihood_of_exploit/parent::*/@id"))
  vals <- sapply(xml2::xml_find_all(doc, "//weakness/likelihood_of_exploit"), rvest::html_text)
  df <- data.frame(ID = ids, Likelihood_Of_Exploit = vals, stringsAsFactors = F)
  cwes <- dplyr::left_join(cwes, df, by = c("ID"))
  cwes$Likelihood_Of_Exploit <- as.factor(cwes$Likelihood_Of_Exploit)

  if (verbose) print("[.][CWE] Parsing Common Consequences...")
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
                 pretty = T)
  )
  df <- data.frame(ID = ids, Common_Consequences = vals, stringsAsFactors = F)
  cwes <- dplyr::left_join(cwes, df, by = c("ID"))

  if (verbose) print("[.][CWE] Parsing Detection Methods...")
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
                 pretty = T)
  )
  df <- data.frame(ID = ids, Detection_Methods = vals, stringsAsFactors = F)
  cwes <- dplyr::left_join(cwes, df, by = c("ID"))

  if (verbose) print("[.][CWE] Parsing Potential Mitigations...")
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
                 pretty = T)
  )
  df <- data.frame(ID = ids, Potential_Mitigations = vals, stringsAsFactors = F)
  cwes <- dplyr::left_join(cwes, df, by = c("ID"))

  if (verbose) print("[.][CWE] Parsing Observed Examples...")
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
                 pretty = T)
  )
  df <- data.frame(ID = ids, Observed_Examples = vals, stringsAsFactors = F)
  cwes <- dplyr::left_join(cwes, df, by = c("ID"))

  if (verbose) print("[.][CWE] Parsing Functional Areas...")
  ids <- xml2::xml_text(xml2::xml_find_all(doc, "//weakness/functional_areas/parent::*/@id"))
  vals <- sapply(xml2::xml_find_all(doc, "//weakness/functional_areas"),
                 function(x) RJSONIO::toJSON(sapply(rvest::html_children(x), rvest::html_text))
  )
  df <- data.frame(ID = ids, Functional_Areas = vals, stringsAsFactors = F)
  cwes <- dplyr::left_join(cwes, df, by = c("ID"))

  if (verbose) print("[.][CWE] Parsing Affected Resources...")
  ids <- xml2::xml_text(xml2::xml_find_all(doc, "//weakness/affected_resources/parent::*/@id"))
  vals <- sapply(xml2::xml_find_all(doc, "//weakness/affected_resources"),
                 function(x) RJSONIO::toJSON(sapply(rvest::html_children(x), rvest::html_text))
  )
  df <- data.frame(ID = ids, Affected_Resources = vals, stringsAsFactors = F)
  cwes <- dplyr::left_join(cwes, df, by = c("ID"))

  if (verbose) print("[.][CWE] Parsing Taxonomy Mappings...")
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
                 w}, pretty = T)
  )
  df <- data.frame(ID = ids, Taxonomy_Mappings = vals, stringsAsFactors = F)
  cwes <- dplyr::left_join(cwes, df, by = c("ID"))

  if (verbose) print("[.][CWE] Parsing Related Attack Patterns...")
  ids <- xml2::xml_text(xml2::xml_find_all(doc, "//weakness/related_attack_patterns/parent::*/@id"))
  vals <- sapply(xml2::xml_find_all(doc, "//weakness/related_attack_patterns"),
                 function(x) RJSONIO::toJSON(sapply(rvest::html_children(x), rvest::html_attrs))
  )
  df <- data.frame(ID = ids, Related_Attack_Patterns = vals, stringsAsFactors = F)
  cwes <- dplyr::left_join(cwes, df, by = c("ID"))

  return(cwes)
}
