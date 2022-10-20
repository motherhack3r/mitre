#' @import dplyr
#' @import tidyr
#' @importFrom utils download.file read.csv

wfn_new <- function(part = "*", vendor = "*", product = "*", version = "*",
                    update = "*", edition = "*", language = "*", sw_edition = "*",
                    target_sw = "*", target_hw = "*", other = "*",
                    as_string = TRUE) {
  cpe <- data.frame(part = part,
                    vendor = vendor,
                    product = product,
                    version = version,
                    update = update,
                    edition = edition,
                    language = language,
                    sw_edition = sw_edition,
                    target_sw = target_sw,
                    target_hw = target_hw,
                    other = other)
  if (as_string) {
    cpe <- paste("cpe", "2.3", paste(cpe, collapse = ":"), sep = ":")
  }
  return(cpe)
}

#' Encode strings to only 73 accepted characters for custom WFN.
#'   - Replace accents, dieresis, etc to simple ASCII chars
#'   - Replace tabs with spaces
#'   - Deal with valid escaped symbols
#'   - Replace not valid characters with "*"
#'
#' @param name character vector with CPE names
#' @param na_replace character, no valid chars will be replaced with "*" by default
#'
#' @return character
str73enc <- function(name = character(), na_replace = "*") {
  encname <- iconv(name, to = 'ASCII//TRANSLIT', sub = na_replace)

  # valid_dec_chars <- dec_valid_chars("input")
  # valid_chars <- sapply(valid_dec_chars, DescTools::AscToChar)
  valid_chars <- " !&()+,-./0123456789:ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"
  regex_notvalid <- stringr::fixed(paste0("[^", paste0(valid_chars, collapse = ""), "]"))

  # Deal with tabs `\t`
  selected_rows <- stringr::str_detect(encname, "[\\x09]")
  encname[selected_rows] <- stringr::str_replace_all(encname[selected_rows], "^[\\x09]", "")
  selected_rows <- stringr::str_detect(encname, "[\\x09]")
  encname[selected_rows] <- stringr::str_replace_all(encname[selected_rows], "\\s[\\x09]", " ")
  selected_rows <- stringr::str_detect(encname, "[\\x09]")
  encname[selected_rows] <- stringr::str_replace_all(encname[selected_rows], "[\\x09]", " ")
  selected_rows <- stringr::str_detect(encname, "[\\x09]")

  # Deal with `escaped and accepted symbols`
  selected_rows <- stringr::str_detect(encname, "[\\x5c]")
  encname[selected_rows] <- stringr::str_replace_all(encname[selected_rows], "[\\x5c]\\-", "-")
  selected_rows <- stringr::str_detect(encname, "[\\x5c]")
  encname[selected_rows] <- stringr::str_replace_all(encname[selected_rows], "[\\x5c]\\!", "!")
  selected_rows <- stringr::str_detect(encname, "[\\x5c]")
  encname[selected_rows] <- stringr::str_replace_all(encname[selected_rows], "[\\x5c]\\&", "&")
  selected_rows <- stringr::str_detect(encname, "[\\x5c]")
  encname[selected_rows] <- stringr::str_replace_all(encname[selected_rows], "[\\x5c]\\(", "(")
  selected_rows <- stringr::str_detect(encname, "[\\x5c]")
  encname[selected_rows] <- stringr::str_replace_all(encname[selected_rows], "[\\x5c]\\)", ")")
  selected_rows <- stringr::str_detect(encname, "[\\x5c]")
  encname[selected_rows] <- stringr::str_replace_all(encname[selected_rows], "[\\x5c]\\,", ",")
  selected_rows <- stringr::str_detect(encname, "[\\x5c]")
  encname[selected_rows] <- stringr::str_replace_all(encname[selected_rows], "[\\x5c]\\.", ".")
  selected_rows <- stringr::str_detect(encname, "[\\x5c]")
  encname[selected_rows] <- stringr::str_replace_all(encname[selected_rows], "[\\x5c]\\/", "/")
  selected_rows <- stringr::str_detect(encname, "[\\x5c]")
  encname[selected_rows] <- stringr::str_replace_all(encname[selected_rows], "[\\x5c]\\:", ":")
  selected_rows <- stringr::str_detect(encname, "[\\x5c]")
  encname[selected_rows] <- stringr::str_replace_all(encname[selected_rows], "[\\x5c]\\_", "_")
  selected_rows <- stringr::str_detect(encname, "[\\x5c]")
  encname[selected_rows] <- stringr::str_replace_all(encname[selected_rows], "[\\x5c]\\+", "+")

  # Finally, replace all escaped
  selected_rows <- stringr::str_detect(encname, "[\\x5c]")
  encname[selected_rows] <- stringr::str_replace_all(encname[selected_rows],
                                                     paste0("[\\x5c]+(", regex_notvalid, ")"), "\\1")

  # Replace underscore with spaces
  selected_rows <- stringr::str_detect(encname, "[\\x5f]")
  encname[selected_rows] <- stringr::str_replace_all(encname[selected_rows], "[\\x5f]", " ")

  # Remove not valid characters
  encname <- stringi::stri_replace_all_regex(encname, regex_notvalid, na_replace)

  return(encname)
}

#' Encode strings to only 49 accepted characters for custom WFN.
#'
#' @param name character vector with CPE names
#' @param na_replace character, no valid chars will be replaced with "*" by default
#'
#' @return character
str49enc <- function(name = character(), na_replace = "*") {
  encname <- iconv(name, to = 'ASCII//TRANSLIT', sub = na_replace)

  # valid_dec_chars <- dec_valid_chars("output")
  # valid_chars <- sapply(valid_dec_chars, DescTools::AscToChar)
  valid_chars <- " !&()+,-./0123456789:abcdefghijklmnopqrstuvwxyz"
  regex_notvalid <- stringr::fixed(paste0("[^", paste0(valid_chars, collapse = ""), "]"))

  # To lowercase
  encname <- tolower(encname)

  # Deal with tabs `\t`
  selected_rows <- stringr::str_detect(encname, "[\\x09]")
  encname[selected_rows] <- stringr::str_replace_all(encname[selected_rows], "^[\\x09]", "")
  selected_rows <- stringr::str_detect(encname, "[\\x09]")
  encname[selected_rows] <- stringr::str_replace_all(encname[selected_rows], "\\s[\\x09]", " ")
  selected_rows <- stringr::str_detect(encname, "[\\x09]")
  encname[selected_rows] <- stringr::str_replace_all(encname[selected_rows], "[\\x09]", " ")
  selected_rows <- stringr::str_detect(encname, "[\\x09]")

  # Deal with `escaped and accepted symbols`
  selected_rows <- stringr::str_detect(encname, "[\\x5c]")
  encname[selected_rows] <- stringr::str_replace_all(encname[selected_rows], "[\\x5c]\\-", "-")
  selected_rows <- stringr::str_detect(encname, "[\\x5c]")
  encname[selected_rows] <- stringr::str_replace_all(encname[selected_rows], "[\\x5c]\\!", "!")
  selected_rows <- stringr::str_detect(encname, "[\\x5c]")
  encname[selected_rows] <- stringr::str_replace_all(encname[selected_rows], "[\\x5c]\\&", "&")
  selected_rows <- stringr::str_detect(encname, "[\\x5c]")
  encname[selected_rows] <- stringr::str_replace_all(encname[selected_rows], "[\\x5c]\\(", "(")
  selected_rows <- stringr::str_detect(encname, "[\\x5c]")
  encname[selected_rows] <- stringr::str_replace_all(encname[selected_rows], "[\\x5c]\\)", ")")
  selected_rows <- stringr::str_detect(encname, "[\\x5c]")
  encname[selected_rows] <- stringr::str_replace_all(encname[selected_rows], "[\\x5c]\\,", ",")
  selected_rows <- stringr::str_detect(encname, "[\\x5c]")
  encname[selected_rows] <- stringr::str_replace_all(encname[selected_rows], "[\\x5c]\\.", ".")
  selected_rows <- stringr::str_detect(encname, "[\\x5c]")
  encname[selected_rows] <- stringr::str_replace_all(encname[selected_rows], "[\\x5c]\\/", "/")
  selected_rows <- stringr::str_detect(encname, "[\\x5c]")
  encname[selected_rows] <- stringr::str_replace_all(encname[selected_rows], "[\\x5c]\\:", ":")
  selected_rows <- stringr::str_detect(encname, "[\\x5c]")
  encname[selected_rows] <- stringr::str_replace_all(encname[selected_rows], "[\\x5c]\\_", "_")
  selected_rows <- stringr::str_detect(encname, "[\\x5c]")
  encname[selected_rows] <- stringr::str_replace_all(encname[selected_rows], "[\\x5c]\\+", "+")

  # Finally, replace all escaped
  selected_rows <- stringr::str_detect(encname, "[\\x5c]")
  encname[selected_rows] <- stringr::str_replace_all(encname[selected_rows],
                                                     paste0("[\\x5c]+(", regex_notvalid, ")"), "\\1")

  # Replace underscore with spaces
  selected_rows <- stringr::str_detect(encname, "[\\x5f]")
  encname[selected_rows] <- stringr::str_replace_all(encname[selected_rows], "[\\x5f]", " ")

  # Remove not valid characters
  encname <- stringi::stri_replace_all_regex(encname, regex_notvalid, na_replace)

  return(encname)
}

#' Title
#'
#' @param x character
#'
#' @return character
cpe_wfn_vendor <- function(x = "Microsoft Corporation") {
  # Remove (c) (tm) (r)
  x <- stringr::str_replace_all(x, "(?i)\\([c|tm|r]\\)", "")
  x <- stringr::str_replace_all(x, "\\u00AE", "")
  x <- stringr::str_replace_all(x, "\\u00A9", "")

    # Normalize vendor: First apply translit, then remove bad words and HTML entities
  x <- iconv(x, to = 'ASCII//TRANSLIT', sub = "")

  # CUSTOM MODIFICATORS
  # R Core Team --> r_project
  x <- stringr::str_replace_all(x, "(?i)R Core Team", "r_project")
  # The R Foundation --> r_foundation
  x <- stringr::str_replace_all(x, "(?i)The R Foundation", "r_foundation")

  # If starts with (, remove parenthesis and keep text until end or )
  x <- stringr::str_replace_all(x, "(?i)^\\(([^\\)]+)\\){0,1}", "\\1")
  # Extract text inside parenthesis
  x <- stringr::str_replace_all(x, "\\(([^\\)]+)\\)", "")
  # Remove any combination of development|core and team|company, separated by space, comma or -
  x <- stringr::str_replace_all(x, "(?i)(\\s|,|-)*(development|core){0,1}(\\s|,|-)*(team|company){0,1}$", " ")
  x <- stringr::str_trim(x)
  # Remove any text equal or equivalent to: corporation, incorporated, international, etc.
  x <- stringr::str_replace_all(x, "(?i)(\\s|,)+(co|corp|corporation|corporations|ltd|llc|cc|inc|incorporated|company|international)\\.{0,1}$", "")
  # Extract word software or soft
  x <- stringr::str_replace_all(x, "(?i)(\\s|,)+(software|soft)(\\s|,){0,1}", " ")
  x <- stringr::str_trim(x)
  # Remove S.A. and S.L. variants
  x <- stringr::str_replace_all(x, "(?i)(\\s|,)+s\\.(a|l)\\.(\\s|\\,){0,1}", " ")
  x <- stringr::str_trim(x)
  # Remove L.P. variants
  x <- stringr::str_replace_all(x, "(?i)(\\s|,)+l\\.p\\.(\\s|,){0,1}", " ")
  x <- stringr::str_trim(x)
  # Remove word foundation
  x <- stringr::str_replace_all(x, "(?i)(\\s|,)+foundation(\\s|,){0,1}", " ")
  x <- stringr::str_trim(x)
  # Remove word systems
  # x <- stringr::str_replace_all(x, "(?i)(\\s|,)+systems(\\s|,){0,1}", " ")
  # x <- stringr::str_trim(x)
  # Remove word technologies
  x <- stringr::str_replace_all(x, "(?i)(\\s|,)+technologies(\\s|,){0,1}", " ")
  x <- stringr::str_trim(x)
  # Remove word limited
  x <- stringr::str_replace_all(x, "(?i)(\\s|,)+limited(\\s|,){0,1}", " ")
  x <- stringr::str_trim(x)
  # Remove words with only numbers or -
  x <- stringr::str_replace_all(x, "(?i)(\\s|,)+[\\d\\-]+(\\s)*", " ")
  x <- stringr::str_trim(x)
  # Again remove any combination of development|core and team|company, separated by space, comma or -
  x <- stringr::str_replace_all(x, "(?i)(\\s|,|-)*(development|core){0,1}(\\s|,|-)*(team|company){0,1}$", " ")
  x <- stringr::str_trim(x)
  # Again remove any text equal or equivalent to: corporation, incorporated, international, etc.
  x <- stringr::str_replace_all(x, "(?i)(\\s|,)+(co|corp|corporation|corporations|ltd|llc|cc|inc|incorporated|company|international)\\.{0,1}$", "")
  x <- stringr::str_trim(x)
  # Extract text inside HTML tags
  x <- sapply(x, function(y) xml2::xml_text(xml2::read_html(paste0("<x>",y,"</x>"))))

  x <- stringr::str_replace_all(x, "^[^a-zA-Z0-9]+$", "")
  x <- stringr::str_replace_all(x, "^[^a-zA-Z0-9]+\\s([a-zA-Z0-9].+)$", "\\1")
  # Extract text inside ${}
  x <- stringr::str_replace_all(x, "^\\${0,1}\\{(.+)\\}$", "")
  # Extract text between $
  x <- stringr::str_replace_all(x, "^\\$(.+)\\$$", "")
  # Remove ()
  x <- stringr::str_replace_all(x, "\\(\\)", "")
  # Text finishing with () --> remove ()
  x <- stringr::str_replace_all(x, "\\(([^\\)]+)$", "")
  # Remove []
  x <- stringr::str_replace_all(x, "\\[\\]", "")
  # Extract text between '' or ""
  x <- stringr::str_replace_all(x, "^'([^']+)'$", "\\1")
  x <- stringr::str_replace_all(x, "^\"([^']+)\"$", "\\1")

  # Errors from SCCM query
  x <- stringr::str_replace_all(x, "(?i)CFullName", "")

  # CUSTOM MODIFICATORS
  # sap_xx --> sap
  x <- stringr::str_replace_all(x, "(?i)sap_[a-z]{2}", "sap")
  # Advanced Micro Devices --> AMD
  x <- stringr::str_replace_all(x, "(?i)Advanced Micro Devices", "AMD")
  # ASUSTek Computer --> ASUSTEK
  x <- stringr::str_replace_all(x, "(?i)ASUSTek(\\s|\\.)*Computer(\\s|\\.)*(inc){0,1}", "ASUSTEK")
  # Hewlett-Packard --> hp
  x <- stringr::str_replace_all(x, "(?i)Hewlett(\\s|\\.|\\-)*Packard(\\s|\\.|\\-)*", "HP ")
  # Internet Testing Systems --> ITS
  x <- stringr::str_replace_all(x, "(?i)Internet Testing Systems", "ITS")
  # Amazon Web Services --> Amazon
  x <- stringr::str_replace_all(x, "(?i)Amazon Web Services", "Amazon")
  # Adobe Systems Incorporated (+variations)--> Adobe
  x <- stringr::str_replace_all(x, "(?i)Adobe([[:punct:]]|\\s)*(System|s)*([[:punct:]]|\\s|\\t)*(Inc)*([[:punct:]]|\\s)*(orporated){0,1}([[:punct:]]|\\s)*(Company)*", "Adobe")

  x <- stringr::str_replace_all(x, "(?i)(\\s)+S\\.(A|p)\\.(S|a)\\.(\\s|$)", " ")
  x <- stringr::str_trim(x)

  x[x == "NA"] <- NA
  x[is.na(x)] <- ""

  return(x)
}

#' Title
#'
#' @param x character
#'
#' @return character
cpe_wfn_product <- function(x = "Oracle VM VirtualBox 6.1.34") {
  x <- stringr::str_replace_all(x, "\\u00AE", "")
  x <- stringr::str_replace_all(x, "\\u00A9", "")
  x <- iconv(x, to = 'ASCII//TRANSLIT')
  x <- stringr::str_replace_all(x, "\\(.*$", "")
  x <- stringr::str_trim(x)
  x <- stringr::str_replace_all(x, "(\\s|,|-)+v*(\\d+\\.{0,1})+\\.{0,1}\\d*$", "")
  x <- stringr::str_replace_all(x, "\\s", "_")
  x <- stringr::str_replace_all(x, "_\\-_.*$", "")
  x <- stringr::str_replace_all(x, "_\\d+\\.\\d+.*$", "")
  x <- stringr::str_replace_all(x, "(?i)_{0,1}(x|amd)(32|64|86).*$", "")
  x <- stringr::str_replace_all(x, "(?i)_for_.*$", "")
  x <- stringr::str_replace_all(x, "\\(\\)", "")
  x <- stringr::str_replace_all(x, "\\(([^\\)]+)\\)", "")
  x <- stringr::str_replace_all(x, "\\(([^\\)]+)$", "")
  x <- stringr::str_replace_all(x, "(?i)\\(r\\)", "")
  x <- stringr::str_replace_all(x, "(?i)\\(tm\\)", "")
  x <- stringr::str_replace_all(x, "(?i)\\(c\\)", "")
  x <- stringr::str_replace_all(x, "_", " ")
  x <- stringr::str_replace_all(x, "(?i)\\([c|tm|r]\\)", "")
  x <- stringr::str_replace_all(x, "(?i)^\\(([^\\)]+)\\)", "\\1")
  x <- stringr::str_trim(x)

  return(x)
}

#' Load CPE data frame from local file or download latest
#'
#' @param local_path path to RDS file. NA value implies remote TRUE
#' @param remote logical
#' @param keep_deprecated logical
#' @param allcols logical
#' @param verbose logical
#'
#' @return data.frame
cpe_latest_data <- function(local_path = NA, remote = F, keep_deprecated = F, allcols = F, verbose = F) {
  if (is.na(local_path) | remote) {
    local_path <- tempfile(fileext = ".rds")
    download.file(url = "https://github.com/motherhack3r/mitre-datasets/raw/master/latest/simple/cpe.rds",
                  destfile = local_path, quiet = !verbose)
  }
  cpes <- readRDS(local_path)
  if (!keep_deprecated) {
    cpes <- cpes[!cpes$deprecated, ]
  }

  if (!allcols) {
    cpes <- cpes[, c("id", "title", "cpe.23", "part", "vendor", "product", "version")]
  }

  return(cpes)
}

#' Returns data frame with grouped count by vendor and product.
#'
#' @param df data.frame
#' @param scale_log logical
#' @param only_vendor logical
#'
#' @return data.frame
getCPEstats <- function(df = cpe_latest_data(), only_vendor = TRUE, scale_log = FALSE) {
  if (only_vendor) {
    sts_cpes <- dplyr::count(df, .data$vendor, sort = TRUE)
  } else {
    sts_cpes <- dplyr::count(df, .data$vendor, .data$product, sort = TRUE)
  }
  if (scale_log) {
    sts_cpes$n <- log(sts_cpes$n)
  }

  return(sts_cpes)
}


#' Title
#'
#' @param path_sccm path to sccm component definitions csv file
#' @param verbose logical
#' @param df_sccm data.frame
#' @param csv.headr logical
#'
#' @return data.frame
cpe_sccm_inventory <- function(path_sccm = "inst/extdata/sccm_component_definitions.csv",
                               df_sccm = data.frame(),
                               verbose = FALSE,
                               csv.headr = TRUE) {
  if (nrow(df_sccm) == 0) {
    if (verbose) print(paste0("[*] ", "Reading CSV file..."))
    df_sccm <- read.csv(path_sccm, header = csv.headr, col.names = c("product", "vendor", "version"))
  }
  if (!("id" %in% names(df_sccm))) {
    if (verbose) print(paste0("[*] ", "Adding id column..."))
    df_sccm$id <- 1:nrow(df_sccm)
  }
  df <- df_sccm
  if (verbose) print(paste0(" |> ", "Input rows: ", nrow(df)))

  # Clean vendor strings
  if (verbose) print(paste0("[|] ", "Clean vendor strings"))
  df_sccm$wfn_vendor <- iconv(df_sccm$vendor, from = "UTF-8", to = 'ASCII//TRANSLIT')
  df_sccm$wfn_vendor <- stringr::str_replace_all(df_sccm$wfn_vendor, "\\?", "")
  df_sccm$wfn_vendor <- stringr::str_trim(df_sccm$wfn_vendor)
  df_sccm$wfn_vendor <- textclean::replace_white(textclean::replace_html(textclean::replace_emoji(df_sccm$wfn_vendor)))
  df_sccm$bad_vendor <- (stringr::str_count(df_sccm$wfn_vendor, "[^a-zA-Z0-9 \\.]")
                         / sapply(df_sccm$wfn_vendor, nchar)
  ) > 0.2
  df_sccm$bad_vendor[is.na(df_sccm$bad_vendor)] <- TRUE
  df_vendor <- df_sccm[!df_sccm$bad_vendor, c("id", "wfn_vendor")]
  df_vendor <- dplyr::rename(df_vendor, vendor = .data$wfn_vendor)
  df_vendor$wfn_vendor <- cpe_wfn_vendor(df_vendor$vendor)
  df_vendor <- df_vendor[stringr::str_length(df_vendor$wfn_vendor) > 1, c("id", "wfn_vendor")]
  df_vendor <- dplyr::rename(df_vendor, vendor = .data$wfn_vendor)
  if (verbose) print(paste0(" |> ", "Good vendor rows: ", nrow(df_vendor)))

  # Clean product strings
  if (verbose) print(paste0("[|] ", "Clean product strings"))
  df_sccm$wfn_product <- iconv(df_sccm$product, from = "UTF-8", to = 'ASCII//TRANSLIT')
  df_sccm$wfn_product <- stringr::str_replace_all(df_sccm$wfn_product, "\\?", "")
  df_sccm$wfn_product <- stringr::str_trim(df_sccm$wfn_product)
  df_sccm$wfn_product <- textclean::replace_white(textclean::replace_html(textclean::replace_emoji(df_sccm$wfn_product)))
  df_sccm$bad_product <- (stringr::str_count(df_sccm$wfn_product, "[^a-zA-Z0-9 \\.\\+]")
                          / sapply(df_sccm$wfn_product, nchar)
  ) > 0.2
  df_sccm$bad_product[is.na(df_sccm$bad_product)] <- TRUE
  df_product <- df_sccm[!df_sccm$bad_product, c("id", "wfn_product")]
  df_product <- dplyr::rename(df_product, product = .data$wfn_product)
  df_product$wfn_product <- cpe_wfn_product(df_product$product)
  df_product <- df_product[stringr::str_length(df_product$wfn_product) > 1, c("id", "wfn_product")]
  df_product <- dplyr::rename(df_product, product = .data$wfn_product)
  if (verbose) print(paste0(" |> ", "Good product rows: ", nrow(df_product)))

  # Clean version strings
  if (verbose) print(paste0("[|] ", "Clean version strings"))
  df_sccm$wfn_version <- df_sccm$version
  df_sccm$wfn_version <- textclean::replace_white(textclean::replace_html(textclean::replace_emoji(df_sccm$wfn_version)))
  df_sccm$wfn_version[(grepl("\\d+(, \\d+)+", df_sccm$wfn_version))] <-
    gsub("\\, ", "\\.", df_sccm$wfn_version[(grepl("\\d+(, \\d+)+", df_sccm$wfn_version))])
  df_sccm$wfn_version[(grepl("\\d+(,\\d+)+", df_sccm$wfn_version))] <-
    gsub("\\,", "\\.", df_sccm$wfn_version[(grepl("\\d+(,\\d+)+", df_sccm$wfn_version))])
  df_sccm$wfn_version[(grepl("\\d+(\\. \\d+)+", df_sccm$wfn_version))] <-
    gsub("\\. ", "\\.", df_sccm$wfn_version[(grepl("\\d+(\\. \\d+)+", df_sccm$wfn_version))])

  df_sccm$wfn_version <- stringr::str_extract(df_sccm$wfn_version, "\\d+(\\.\\d+)*")
  # df_sccm$wfn_version <- stringr::str_replace_all(df_sccm$wfn_version, "(.*)(\\s|,|-)+[vers\\.]*([\\d|\\.]+)(.*)$", "\\3")
  df_version <- df_sccm[, c("id", "wfn_version")]
  df_version <- dplyr::rename(df_version, version = .data$wfn_version)
  df_version$version[is.na(df_version$version)] <- ""
  if (verbose) print(paste0(" |> ", "Good version rows: ", nrow(df_version)))

  if (verbose) print(paste0("[|] ", "General cleansing and remove NAs"))
  df_inv <- dplyr::inner_join(df_vendor, df_product, by = "id")
  df_inv <- dplyr::inner_join(df_inv, df_version, by = "id")
  # df_inv <- dplyr::full_join(df_version, df_vendor, by = "id")
  df_inv$vendor[is.na(df_inv$vendor)] <- ""
  # df_inv <- dplyr::full_join(df_inv, df_product, by = "id")
  df_inv$product[is.na(df_inv$product)] <- ""
  df_inv$version[is.na(df_inv$version)] <- ""

  df_inv$vendor <- stringr::str_replace_all(df_inv$vendor, "\\s+", " ")
  df_inv$vendor <- stringr::str_trim(df_inv$vendor)
  df_inv$product <- stringr::str_replace_all(df_inv$product, "\\s+", " ")
  df_inv$product <- stringr::str_trim(df_inv$product)
  df_inv$version <- stringr::str_replace_all(df_inv$version, "\\s+", " ")
  df_inv$version <- stringr::str_trim(df_inv$version)

  df_inv$product[nchar(df_inv$version) == 0] <- df_sccm$product[df_sccm$id %in% df_inv$id[nchar(df_inv$version) == 0]]
  if (verbose) print(paste0(" |> ", "Good rows: ", nrow(df_inv)))

  if (verbose) print(paste0("[|] ", "Removing duplicated vendor in title"))
  df_inv <- dplyr::mutate(df_inv,
                          title = ifelse(stringr::str_starts(.data$product, stringr::fixed(paste0(.data$vendor, " "))),
                                         paste(.data$product, .data$version),
                                         paste(.data$vendor, .data$product, .data$version)))

  df_inv$title <- stringr::str_replace_all(df_inv$title, "\\s+", " ")
  df_inv$title <- stringr::str_replace_all(df_inv$title, "\\b(\\w+\\s)\\1\\b(.*)", "\\1\\2")
  df_inv$title <- stringr::str_trim(df_inv$title)

  if (verbose) print(paste0("[|] ", "Custom common title cleansing ..."))
  df_inv$title <- stringr::str_replace_all(df_inv$title, "Microsoft vs ", "Microsoft Visual Studio ")
  df_inv$title <- stringr::str_replace_all(df_inv$title, "Microsoft vcpp ", "Microsoft Visual C++ ")

  df <- dplyr::left_join(df, dplyr::select(df_inv, "id", "title"), by = "id")
  df <- df[, c("id", "title", "vendor", "product", "version")]

  # Final cleansing
  if (verbose) print(paste0("[|] ", "Final cleansing..."))
  df <- as.data.frame(df)
  df <- df[!is.na(df$title), ]
  df$title <- iconv(df$title, to = 'ASCII//TRANSLIT')
  df$version <- iconv(df$version, to = 'ASCII//TRANSLIT')
  df <- df[!is.na(df$title), ]
  df$valid <- stringr::str_detect(str73enc(df$title), "\\*", negate = T)
  df <- df[df$valid, c("id", "title", "vendor", "product", "version")]
  if (verbose) print(paste0(" |> ", "Good rows: ", nrow(df)))

  return(df)
}

#' Create a CPE for each inventory item.
#'
#' @param df_inventory data.frame with columns: vendor, name and version; title is optional
#' @param model_name huggingface reference, by default: Neurona/cpener-test
#'
#' @return data.frame
predict_cpe <- function(df_inventory = mitre::getInventory(),
                        model_name = "Neurona/cpener-test") {

  # Function for processing NER output
  embed2cpener <- function(df_ner = data.frame()) {
    df_cpe <- as.data.frame(df_ner$x_NER)
    df_cpe$entity <- stringr::str_replace_all(string = df_cpe$entity,
                                              pattern = "^[BIOLU]\\-(cpe.+)$",
                                              replacement = "\\1")
    df_cpe <- dplyr::inner_join(x = df_cpe %>%
                                  group_by(.data$entity) %>%
                                  summarise(score = mean(.data$score)),
                                y = df_cpe %>%
                                  select(.data$entity, .data$word) %>%
                                  group_by(.data$entity) %>%
                                  mutate(word = paste(.data$word, collapse = " ")) %>%
                                  unique() %>%
                                  as.data.frame(),
                                by = "entity")
    df_cpe$word <- stringr::str_replace_all(string = df_cpe$word, pattern = "\\s##", replacement = "")
    df_cpe$word <- stringr::str_replace_all(string = df_cpe$word, pattern = "^\\s*##", replacement = "")
    df_cpe$word[df_cpe$entity == "cpe_version"] <-
      stringr::str_replace_all(df_cpe$word[df_cpe$entity == "cpe_version"], " \\. ", ".")
    return(df_cpe)
  }

  # Function for CPE creation
  cpener2cpe23 <- function(df_ner = data.frame()) {
    part <- "a"
    vendor <- ifelse(test = "cpe_vendor" %in% df_ner$entity,
                     yes = df_ner$word[df_ner$entity == "cpe_vendor"],
                     no = "*")
    product <- ifelse(test = "cpe_product" %in% df_ner$entity,
                      yes = df_ner$word[df_ner$entity == "cpe_product"],
                      no = "*")
    version <- ifelse(test = "cpe_version" %in% df_ner$entity,
                      yes = df_ner$word[df_ner$entity == "cpe_version"],
                      no = "*")
    vendor <- stringr::str_replace_all(vendor, " ", "_")
    product <- stringr::str_replace_all(product, " ", "_")
    version <- stringr::str_replace_all(version, "(\\d) (\\d)", "\\1\\.\\2")
    version <- stringr::str_replace_all(version, "(\\d) (\\d)", "\\1\\.\\2")
    # version <- stringr::str_replace_all(version, " \\. ", ".")
    if (vendor == "*") vendor <- product
    if (product == "*") product <- vendor

    cpe <- mean(df_ner$score)
    names(cpe) <- stringr::str_replace_all(paste("cpe", "2.3",
                                                 part, vendor, product, version,
                                                 "*:*:*:*:*:*:*", sep = ":"),
                                           "_([[:punct:]])_", "\\1")
    return(cpe)
  }

  if (!("title" %in% names(df_inventory))) {
    sw_title <- paste(cpe_wfn_vendor(df_inventory$vendor),
                      cpe_wfn_product(df_inventory$name), sep = " ")
    df_pred <- data.frame(title = sapply(sw_title, function(x) paste(unique(unlist(strsplit(x, " "))), collapse = " ")),
                          cpe = rep(NA, length(sw_title)))
    df_pred$title <- paste0(stringr::str_trim(paste(df_pred$title, df_inventory$version)),".")
  } else {
    df_pred <- df_inventory
  }

  predicted_cpes <- sapply(df_pred$title,
                           function(x)
                             cpener2cpe23(embed2cpener(text::textNER(x = x,
                                                                     model = model_name,
                                                                     device = "gpu",
                                                                     logging_level = "critical"))),
                           USE.NAMES = F)
  df_inventory$ner_cpe <- names(predicted_cpes)
  df_inventory$ner_score <- as.numeric(predicted_cpes)

  return(df_inventory)
}


#' Title
#'
#' @param df data.frame
#' @param verbose logical
#'
#' @return data.frame
#' @export
cpe_generate <- function(df = getInventory(), verbose = FALSE) {
  if (verbose) print(paste0("[*] ", "Ready to generate CPEs..."))

  # df_inventory <- cpe_sccm_inventory(df_sccm = df, verbose = verbose)
  df_inventory <- df
  df$vd_match_type <- rep(NA, nrow(df))
  df$vd_match_type[!(df$id %in% df_inventory$id)] <- "NONE"

  if (verbose) print(paste0("[*] ", "Name entity recognition for titles..."))
  df_inventory <- predict_cpe(df_inventory)

  df_inventory2 <- df_inventory
  df_inventory <- df_inventory %>%
    separate(col = .data$ner_cpe , sep = ":", extra = "merge",
             into = c("std", "v", "part", "vend", "prod", "vers", "tail")) %>%
    mutate(ner_cpe = paste(.data$std, .data$v, .data$part, .data$vend,
                           .data$prod, .data$version, .data$tail, sep = ":")) %>%
    select(names(df_inventory))

  # Load Official CPEs
  if (verbose) print(paste0("[*] ", "Loading Official CPEs and stats..."))
  cpes <- cpe_latest_data(remote = T)
  cpes_vp <- getCPEstats(cpes, only_vendor = FALSE)
  cpes_vend <- getCPEstats(cpes)
  cpes_prod <- cpes %>%
    group_by(.data$product) %>%
    summarise(n = n(), .groups = "drop") %>%
    arrange(desc(n))

  # prepare data.frame for evaluation and candidates selection
  if (verbose) print(paste0("[*] ", "Prepare for CPE candidates selection..."))
  df_eval <- df_inventory %>%
    separate(col = .data$ner_cpe , sep = ":", extra = "merge",
             into = c("std", "v", "part", "ner_vend", "ner_prod", "ner_vers", "tail")) %>%
    select(.data$id, .data$vendor, .data$product, .data$version,
           .data$ner_vend, .data$ner_prod, .data$ner_vers) %>%
    mutate(ner_cpelite = paste(.data$ner_vend, .data$ner_prod, sep = ":"))

  df_match <- data.frame(id = integer(),
                         vd_vendor = character(),
                         vd_product = character(),
                         vd_match_type = character(),
                         vd_score = numeric(), stringsAsFactors = FALSE)

  # TEST M1 Perfect match with vendor using inner_join
  if (verbose) print(paste0("[*] ", "TEST M1: Perfect match with vendor:product ..."))
  df_test <- inner_join(df_eval %>% select(.data$id, .data$ner_vend, .data$ner_prod),
                        cpes_vp %>% select(.data$vendor, .data$product),
                        by = c("ner_vend" = "vendor", "ner_prod" = "product")) %>%
    rename("vd_vendor" = "ner_vend", "vd_product" = "ner_prod") %>%
    select(.data$id, .data$vd_vendor, .data$vd_product)
  df_test$vd_match_type <- rep("M1", nrow(df_test))
  df_test$vd_score <- rep(1.0, nrow(df_test))
  df_match <- bind_rows(df_match, df_test)

  df_eval <- df_eval[!(df_eval$id %in% df_match$id), ]

  # TEST M1A
  if (verbose) print(paste0("[*] ", "TEST M1A: Perfect match with vendor and product ..."))
  df_test <- inner_join(df_eval %>% select(.data$id, .data$ner_vend, .data$ner_prod),
                        cpes_vend %>% select(.data$vendor),
                        by = c("ner_vend" = "vendor")) %>%
    inner_join(cpes_prod %>% select(.data$product), by = c("ner_prod" = "product")) %>%
    rename("vd_vendor" = "ner_vend", "vd_product" = "ner_prod") %>%
    select(.data$id, .data$vd_vendor, .data$vd_product)
  df_test$vd_match_type <- rep("M1A", nrow(df_test))
  df_test$vd_score <- rep(.95, nrow(df_test))

  df_match <- bind_rows(df_match, df_test)
  df_eval <- df_eval[!(df_eval$id %in% df_match$id), ]

  # TEST M1B
  if (verbose) print(paste0("[*] ", "TEST M1B: Perfect match with vendor:(product)* ..."))
  df_test <- inner_join(df_eval %>% select(.data$id, .data$ner_vend, .data$ner_prod),
                        cpes_vp %>% select(.data$vendor, .data$product),
                        by = c("ner_vend" = "vendor"))
  df_test$ner_prod <- stringr::str_replace_all(df_test$ner_prod, "_", " ")
  df_test$product <- stringr::str_replace_all(df_test$product, "_", " ")
  df_test$vd_match_type <- rep("M1B", nrow(df_test))
  df_test$vd_score <- stringdist::stringsim(df_test$ner_prod, df_test$product, method = "jw")
  # df_test$vd_score <- stringdist::stringsim(df_test$ner_prod, df_test$product, method = "osa")
  # df_test$vd_score <- stringdist::stringsim(df_test$ner_prod, df_test$product, method = "qgram")

  df_test <- inner_join(df_test %>% group_by(.data$id) %>%
                          summarise(vd_score = max(.data$vd_score), .groups = "drop"),
                        df_test, by = c("id" = "id", "vd_score" = "vd_score")) %>%
    select(names(df_test)) %>%
    filter(.data$vd_score >= 0.8)
  df_test$ner_prod <- stringr::str_replace_all(df_test$ner_prod, " ", "_")
  df_test$product <- stringr::str_replace_all(df_test$product, " ", "_")

  df_test <- df_test %>%
    rename("vd_vendor" = "ner_vend", "vd_product" = "product") %>%
    select(names(df_match))

  df_match <- bind_rows(df_match, df_test)
  df_eval <- df_eval[!(df_eval$id %in% df_match$id), ]

  # TEST M1C
  if (verbose) print(paste0("[*] ", "TEST M1C: Perfect match with (vendor):product ..."))
  df_test <- inner_join(df_eval %>% select(.data$id, .data$ner_vend, .data$ner_prod),
                        cpes_vp %>% select(.data$vendor, .data$product),
                        by = c("ner_prod" = "product"))
  df_test$ner_vend <- stringr::str_replace_all(df_test$ner_vend, "_", " ")
  df_test$vendor <- stringr::str_replace_all(df_test$vendor, "_", " ")
  df_test$vd_match_type <- rep("M1C", nrow(df_test))
  df_test$vd_score <- stringdist::stringsim(df_test$ner_vend, df_test$vendor, method = "jw")
  # df_test$vd_score <- stringdist::stringsim(df_test$ner_vend, df_test$vendor, method = "osa")
  # df_test$vd_score <- stringdist::stringsim(df_test$ner_vend, df_test$vendor, method = "qgram")

  df_test <- inner_join(df_test %>% group_by(.data$id) %>%
                          summarise(vd_score = max(.data$vd_score), .groups = "drop"),
                        df_test, by = c("id" = "id", "vd_score" = "vd_score")) %>%
    select(names(df_test)) %>%
    filter(.data$vd_score >= 0.8)
  df_test$ner_vend <- stringr::str_replace_all(df_test$ner_vend, " ", "_")
  df_test$vendor <- stringr::str_replace_all(df_test$vendor, " ", "_")

  df_test <- df_test %>%
    rename("vd_product" = "ner_prod", "vd_vendor" = "vendor") %>%
    select(names(df_match))

  df_match <- bind_rows(df_match, df_test)
  df_eval <- df_eval[!(df_eval$id %in% df_match$id), ]

  # TEST M1F
  df_test <- inner_join(df_eval %>% select(.data$id, .data$vendor, .data$ner_prod) %>%
                          mutate(vendor = tolower(.data$vendor)),
                        cpes_vp %>% select(.data$vendor, .data$product),
                        by = c("vendor" = "vendor", "ner_prod" = "product")) %>%
    rename("vd_vendor" = "vendor", "vd_product" = "ner_prod") %>%
    select(.data$id, .data$vd_vendor, .data$vd_product)
  df_test$vd_match_type <- rep("M1F", nrow(df_test))
  df_test$vd_score <- rep(1.0, nrow(df_test))
  df_match <- bind_rows(df_match, df_test)

  df_eval <- df_eval[!(df_eval$id %in% df_match$id), ]

  # TEST M1G
  if (verbose) print(paste0("[*] ", "TEST M1G: Perfect match with vendor:(product)* ..."))
  df_test <- inner_join(df_eval %>% select(.data$id, .data$vendor, .data$ner_prod) %>%
                          mutate(vendor = tolower(.data$vendor)),
                        cpes_vp %>% select(.data$vendor, .data$product),
                        by = c("vendor" = "vendor"))
  df_test$ner_prod <- stringr::str_replace_all(df_test$ner_prod, "_", " ")
  df_test$product <- stringr::str_replace_all(df_test$product, "_", " ")
  df_test$vd_match_type <- rep("M1G", nrow(df_test))
  df_test$vd_score <- stringdist::stringsim(df_test$ner_prod, df_test$product, method = "jw")
  # df_test$vd_score <- stringdist::stringsim(df_test$ner_prod, df_test$product, method = "osa")
  # df_test$vd_score <- stringdist::stringsim(df_test$ner_prod, df_test$product, method = "qgram")

  df_test_match <- inner_join(df_test %>% group_by(.data$id) %>%
                                summarise(vd_score = max(.data$vd_score), .groups = "drop"),
                              df_test, by = c("id" = "id", "vd_score" = "vd_score")) %>%
    select(names(df_test)) %>%
    filter(.data$vd_score >= 0.75)
  df_test_match$ner_prod <- stringr::str_replace_all(df_test_match$ner_prod, " ", "_")
  df_test_match$product <- stringr::str_replace_all(df_test_match$product, " ", "_")
  df_test_match$vd_match_type <- rep("M1G", nrow(df_test_match))

  df_test_new <- df_test[!(df_test$id %in% df_test_match$id), ]
  df_test_new$ner_prod <- stringr::str_replace_all(df_test_new$ner_prod, " ", "_")
  df_test_new$product <- stringr::str_replace_all(df_test_new$product, " ", "_")
  df_test_new$vd_match_type <- rep("NEW", nrow(df_test_new))


  df_test_match <- df_test_match %>%
    rename("vd_vendor" = "vendor", "vd_product" = "product") %>%
    select(names(df_match))
  df_test_new <- df_test_new %>%
    rename("vd_vendor" = "vendor", "vd_product" = "ner_prod") %>%
    select(names(df_match)) %>%
    left_join(df_inventory %>% select(.data$id, .data$ner_score), by = "id") %>%
    select(-.data$vd_score) %>% rename("vd_score" = "ner_score")

  df_match <- bind_rows(df_match, df_test_match, df_test_new)

  rm(df_test_match, df_test_new)
  df_eval <- df_eval[!(df_eval$id %in% df_match$id), ]
  if (verbose) print(paste0("[*] ", "No more candidates for today."))

  # Merge
  if (verbose) print(paste0("[*] ", "Fine tune scorings..."))
  df_eval <- df_eval %>%
    left_join(df_inventory %>% select(.data$id, .data$ner_score), by = "id")
  df_eval$vd_score <- df_eval$ner_score/2
  df_eval$vd_match_type <- rep("UNK", nrow(df_eval))
  df_eval <- df_eval %>%
    rename("vd_vendor" = "ner_vend", "vd_product" = "ner_prod") %>%
    select(-.data$vendor, -.data$product, -.data$version,
           -.data$ner_cpelite, -.data$ner_vers, -.data$ner_score)
  df_inventory <- left_join(df_inventory,
                            bind_rows(df_match, df_eval),
                            by = "id")

  if (verbose) print(paste0("[*] ", "Merging candidates..."))
  df_inventory$vd_cpe <- paste("cpe:2.3:a",
                               df_inventory$vd_vendor,
                               df_inventory$vd_product,
                               df_inventory$version,
                               "*:*:*:*:*:*:*", sep = ":")

  df_inventory <- left_join(df_inventory2 %>% select(.data$id, .data$title, .data$vendor,
                                                     .data$product, .data$version),
                            df_inventory %>% unique() %>%
                              select(.data$id, .data$vd_cpe, .data$vd_score) %>%
                              group_by(.data$id) %>%
                              mutate(candidates = jsonlite::toJSON(data.frame(cpe = .data$vd_cpe,
                                                                              score = .data$vd_score))) %>%
                              select(.data$id, .data$candidates) %>%
                              unique(),
                            by = "id") %>%
    rowwise() %>%
    mutate(cpe = jsonlite::fromJSON(.data$candidates)[1,1]) %>%
    mutate(cpe_score = jsonlite::fromJSON(.data$candidates)[1,2]) %>%
    ungroup() %>%
    select(.data$id, .data$title, .data$vendor, .data$product, .data$version,
           .data$cpe, .data$cpe_score, .data$candidates)

  return(df_inventory)
}

cpe_is_version <- function(x = c("")) {
  if (is.na(x)) return(FALSE)
  return(grepl(pattern = "^\\d+([\\.\\-]\\d+)*$", x))
}

# cpe_is_vulnerable <- function(x = c(""), cves = cve_latest_data()) {
#   cpes_iv <- rep(FALSE, length(x))
#
#   return(cpes_iv)
# }

cpelite_check_vers <- function(x_vers, versionStartExcluding, versionStartIncluding, versionEndExcluding, versionEndIncluding) {
  k_sex <- if (cpe_is_version(versionStartExcluding)) numeric_version(versionStartExcluding) else versionStartExcluding
  k_sin <- if (cpe_is_version(versionStartIncluding)) numeric_version(versionStartIncluding) else versionStartIncluding
  k_eex <- if (cpe_is_version(versionEndExcluding)) numeric_version(versionEndExcluding) else versionEndExcluding
  k_ein <- if (cpe_is_version(versionEndIncluding)) numeric_version(versionEndIncluding) else versionEndIncluding

  if (is.na(k_sex)) {
    # OK:
    # NA: Start Excluding
    if (is.na(k_sin)) {
      # OK:
      # NA: Start Excluding, Start Including
      if (is.na(k_eex)) {
        # OK:
        # NA: Start Excluding, Start Including, End Excluding
        if (is.na(k_ein)) {
          # OK:
          # NA: Start Excluding, Start Including, End Excluding, End Including
          FALSE
        } else {
          # OK: End Including
          # NA: Start Excluding, Start Including, End Excluding
          ifelse(test = is.numeric_version(k_ein),
                 yes = (x_vers <= k_ein),
                 no = FALSE)
        }
      } else {
        # OK: End Excluding
        # NA: Start Excluding, Start Including
        if (is.na(k_ein)) {
          # OK: End Excluding
          # NA: Start Excluding, Start Including, End Including
          ifelse(test = is.numeric_version(k_eex),
                 yes = (x_vers < k_eex),
                 no = FALSE)
        } else {
          # OK: End Excluding, End Including
          # NA: Start Excluding, Start Including
          ifelse(test = is.numeric_version(k_eex) & is.numeric_version(k_ein),
                 yes = (x_vers < k_eex) & (x_vers <= k_ein),
                 no = FALSE)
        }
      }
    } else {
      # OK: Start Including
      # NA: Start Excluding
      if (is.na(k_eex)) {
        # OK: Start Including
        # NA: Start Excluding, End Excluding
        if (is.na(k_ein)) {
          # OK: Start Including
          # NA: Start Excluding, End Excluding, End Including
          ifelse(test = is.numeric_version(k_sin),
                 yes = (k_sin <= x_vers),
                 no = FALSE)
        } else {
          # OK: Start Including, End Including
          # NA: Start Excluding, End Excluding
          ifelse(test = is.numeric_version(k_sin) & is.numeric_version(k_ein),
                 yes = (k_sin <= x_vers) & (x_vers <= k_ein),
                 no = FALSE)
        }
      } else {
        # OK: Start Including, End Excluding
        # NA: Start Excluding
        if (is.na(k_ein)) {
          # OK: Start Including, End Excluding
          # NA: Start Excluding, End Including
          ifelse(test = is.numeric_version(k_sin) & is.numeric_version(k_eex),
                 yes = (k_sin <= x_vers) & (x_vers < k_eex),
                 no = FALSE)
        } else {
          # OK: Start Including, End Excluding, End Including
          # NA: Start Excluding
          ifelse(test = is.numeric_version(k_sin) & is.numeric_version(k_eex) & is.numeric_version(k_ein),
                 yes = (k_sin <= x_vers) & (x_vers < k_eex) & (x_vers <= k_ein),
                 no = FALSE)
        }
      }
    }
  } else {
    # OK: Start Excluding
    # NA:
    if (is.na(k_sin)) {
      # OK: Start Excluding
      # NA: Start Including
      if (is.na(k_eex)) {
        # OK: Start Excluding
        # NA: Start Including, End Excluding
        if (is.na(k_ein)) {
          # OK: Start Excluding
          # NA: Start Including, End Excluding, End Including
          ifelse(test = is.numeric_version(k_sex),
                 yes = (k_sex < x_vers),
                 no = FALSE)
        } else {
          # OK: Start Excluding, End Including
          # NA: Start Including, End Excluding
          ifelse(test = is.numeric_version(k_sex) & is.numeric_version(k_ein),
                 yes = (k_sex < x_vers) & (x_vers <= k_ein),
                 no = FALSE)
        }
      } else {
        # OK: Start Excluding, End Excluding
        # NA: Start Including
        if (is.na(k_ein)) {
          # OK: Start Excluding, End Excluding
          # NA: Start Including, End Including
          ifelse(test = is.numeric_version(k_sex) & is.numeric_version(k_eex),
                 yes = (k_sex < x_vers) & (x_vers < k_eex),
                 no = FALSE)

        } else {
          # OK: Start Excluding, End Excluding, End Including
          # NA: Start Including
          ifelse(test = is.numeric_version(k_sex) & is.numeric_version(k_eex) & is.numeric_version(k_ein),
                 yes = (k_sex < x_vers) & (x_vers < k_eex) & (x_vers <= k_ein),
                 no = FALSE)
        }
      }
    } else {
      # OK: Start Excluding, Start Including
      # NA:
      if (is.na(k_eex)) {
        # OK: Start Excluding, Start Including
        # NA: End Excluding
        if (is.na(k_ein)) {
          # OK: Start Excluding, Start Including
          # NA: End Excluding, End Including
          ifelse(test = is.numeric_version(k_sex) & is.numeric_version(k_sin),
                 yes = (k_sex < x_vers) & (k_sin <= x_vers),
                 no = FALSE)
        } else {
          # OK: Start Excluding, Start Including, End Including
          # NA: End Excluding
          ifelse(test = is.numeric_version(k_sex) & is.numeric_version(k_sin) & is.numeric_version(k_ein),
                 yes = (k_sex < x_vers) & (k_sin <= x_vers) & (x_vers <= k_ein),
                 no = FALSE)
        }
      } else {
        # OK: Start Excluding, Start Including, End Excluding
        # NA:
        if (is.na(k_ein)) {
          # OK: Start Excluding, Start Including, End Excluding
          # NA: End Including
          ifelse(test = is.numeric_version(k_sex) & is.numeric_version(k_sin) & is.numeric_version(k_eex),
                 yes = (k_sex < x_vers) & (k_sin <= x_vers) & (x_vers < k_eex),
                 no = FALSE)
        } else {
          # OK: Start Excluding, Start Including, End Excluding, End Including
          # NA:
          ifelse(test = is.numeric_version(k_sex) & is.numeric_version(k_sin) & is.numeric_version(k_eex) & is.numeric_version(k_ein),
                 yes = (k_sex < x_vers) & (k_sin <= x_vers) & (x_vers < k_eex) & (x_vers <= k_ein),
                 no = FALSE)
        }
      }
    }
  }
}

cpelite_vulnerable_configs <- function(x, x_vers, cves = cve_latest_data(), verbose = FALSE) {
  if (length(x) > 1) {
    rx <- paste0("(", paste(x, collapse = "|"), ")")
  } else {
    rx <- x
  }

  if (verbose) print(paste0("[*] ", "Searching CVEs for ", x, "..."))
  x_vc <- cves[which(grepl(pattern = rx, x = cves$vulnerable.configuration)), c("cve.id", "vulnerable.configuration")]

  if (nrow(x_vc) == 0) return(jsonlite::toJSON(NA))

  if (verbose) print(paste0("[*] ", "Flattening vulnerable configurations..."))
  vulnerableconfigurations <- data.frame()
  for (i in 1:nrow(x_vc)) {
    vulnconf <- cve_flatten_vulnconf(x_vc$vulnerable.configuration[i],
                                     x_vc$cve.id[i])
    vulnerableconfigurations <- dplyr::bind_rows(vulnerableconfigurations, vulnconf)
    if (verbose)
      if (i %% 10 == 0) print(paste0("[.] ", round(i / 10, 0), "0 vulnerable configurations processed..."))
  }

  if (verbose) print(paste0("[*] ", "Checking versions..."))
  k <- vulnerableconfigurations %>%
    filter(grepl(pattern = rx, x = cpe23Uri)) %>%
    filter(vulnerable)
  k_nas <- which(is.na(k$versionStartExcluding) & is.na(k$versionStartIncluding) & is.na(k$versionEndExcluding) & is.na(k$versionEndIncluding))

  # Fill empty ranges
  k$versionStartIncluding[k_nas] <- sapply(k$cpe23Uri[k_nas], function(x) stringr::str_split(x, ":", 7, T)[6])
  k$versionEndIncluding[k_nas] <- sapply(k$cpe23Uri[k_nas], function(x) stringr::str_split(x, ":", 7, T)[6])

  k <- k %>% select(vc_id, versionStartExcluding, versionStartIncluding,
                    versionEndExcluding, versionEndIncluding)

  x_vers <- if (cpe_is_version(x_vers)) numeric_version(x_vers) else x_vers
  k_selected <- k %>%
    rowwise() %>%
    mutate(mark = cpelite_check_vers(x_vers,
                                     versionStartExcluding, versionStartIncluding,
                                     versionEndExcluding, versionEndIncluding)) %>%
    filter(mark > 0) %>% select(-mark) %>%
    mutate(cve = stringr::str_extract(string = vc_id, pattern = "CVE\\-\\d+\\-\\d+")) %>%
    mutate(conditional = stringr::str_detect(string = vc_id, pattern = "\\:AND\\d+")) %>%
    ungroup() %>%
    select(cve, conditional) %>%
    unique()

  return(jsonlite::toJSON(k_selected))
}

