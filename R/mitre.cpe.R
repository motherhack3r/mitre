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


#' Create a CPE for each inventory item.
#'
#' @param df_inventory data.frame with columns: vendor, name and version; title is optional
#' @param model_name huggingface reference, by default: Neurona/cpener-test
#'
#' @return data.frame
#' @export
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
#' @param path_sccm path to sccm component definitions csv file
#' @param verbose logical
#' @param df_sccm data.frame
#' @param csv.headr logical
#'
#' @return data.frame
#' @export
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
  df_sccm$bad_product <- (stringr::str_count(df_sccm$wfn_product, "[^a-zA-Z0-9 \\.]")
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

  valid_dec_chars <- enc_valid_chars(taste = "dec", type = "input")
  valid_chars <- sapply(valid_dec_chars, DescTools::AscToChar)
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

  valid_dec_chars <- enc_valid_chars(taste = "dec", type = "output")
  valid_chars <- sapply(valid_dec_chars, DescTools::AscToChar)
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

#' Returns a custom list of valid chars.
#'
#' @param taste character, type of output "char", "dec", or "hex"
#' @param add_tab logical, include tab char
#' @param add_enter logical, include enter char
#' @param add_underline logical, include "_"
#' @param type character, returns valid chars for "input" or "output"
#'
#' @return character
enc_valid_chars <- function(taste = c("char", "dec", "hex")[1],
                            add_tab = FALSE, add_enter = FALSE,
                            add_underline = FALSE,
                            type = c("input", "output")[1]) {

  valid_dec_chars <- c(32,33,38,40,41, 43:58, 65:90, 97:122)
  if (type == "output")
    valid_dec_chars <- c(32,33,38,40,41, 43:58, 97:122)
  if (add_enter) valid_dec_chars <- c(10, valid_dec_chars)
  if (add_tab) valid_dec_chars <- c(9, valid_dec_chars)
  if (add_underline) valid_dec_chars <- c(95, valid_dec_chars)

  if (taste == "char") {
    valid_chars <- sapply(valid_dec_chars, DescTools::AscToChar)
  } else if (taste == "hex") {
    valid_chars <- sapply(valid_dec_chars, DescTools::DecToHex)
  } else {
    return(valid_dec_chars)
  }

  return(valid_chars)
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
#' @param df data.frame
#' @param verbose logical
#'
#' @return data.frame
#' @export
cpe_generate <- function(df = getInventory(), verbose = FALSE) {
  if (verbose) print(paste0("[*] ", "Ready to generate CPEs..."))

  df_inventory <- cpe_sccm_inventory(df_sccm = df, verbose = T)
  df$vd_match_type <- rep(NA, nrow(df))
  df$vd_match_type[!(df$id %in% df_inventory$id)] <- "NONE"

  df_inventory <- predict_cpe(df_inventory)

  df_inventory2 <- df_inventory
  df_inventory <- df_inventory %>%
    separate(col = .data$ner_cpe , sep = ":", extra = "merge",
             into = c("std", "v", "part", "vend", "prod", "vers", "tail")) %>%
    mutate(ner_cpe = paste(.data$std, .data$v, .data$part, .data$vend,
                           .data$prod, .data$version, .data$tail, sep = ":")) %>%
    select(names(df_inventory))

  # Load Official CPEs
  cpes <- cpe_latest_data(remote = T)
  cpes_vp <- getCPEstats(cpes, only_vendor = FALSE)
  cpes_vend <- getCPEstats(cpes)
  cpes_prod <- cpes %>%
    group_by(.data$product) %>%
    summarise(n = n(), .groups = "drop") %>%
    arrange(desc(n))

  # prepare data.frame for evaluation and candidates selection
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

  # Merge
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

#' Title
#'
#' @param x character
#'
#' @return character
cpe_wfn_vendor <- function(x = "Microsoft Corporation") {
  # Normalize vendor: First apply translit, then remove bad words and HTML entities
  x <- iconv(x, to = 'ASCII//TRANSLIT', sub = "")
  x <- stringr::str_replace_all(x, "(?i)\\([c|tm|r]\\)", "")
  x <- stringr::str_replace_all(x, "(?i)^\\(([^\\)]+)\\){0,1}", "\\1")
  x <- stringr::str_replace_all(x, "\\(([^\\)]+)\\)", "")
  x <- stringr::str_replace_all(x, "(?i)(\\s|,|-)*(development|core){0,1}(\\s|,|-)*(team|company){0,1}$", " ")
  x <- stringr::str_trim(x)
  x <- stringr::str_replace_all(x, "(?i)(\\s|,)+(co|corp|corporation|ltd|llc|cc|inc|incorporated|company|international)\\.{0,1}$", "")
  x <- stringr::str_replace_all(x, "(?i)(\\s|,)+(software|soft)(\\s|,){0,1}", " ")
  x <- stringr::str_trim(x)
  x <- stringr::str_replace_all(x, "(?i)(\\s|,)+s\\.(a|l)\\.(\\s|,){0,1}", " ")
  x <- stringr::str_trim(x)
  x <- stringr::str_replace_all(x, "(?i)(\\s|,)+l\\.p\\.(\\s|,){0,1}", " ")
  x <- stringr::str_trim(x)
  x <- stringr::str_replace_all(x, "(?i)(\\s|,)+foundation(\\s|,){0,1}", " ")
  x <- stringr::str_trim(x)
  # x <- stringr::str_replace_all(x, "(?i)(\\s|,)+systems(\\s|,){0,1}", " ")
  # x <- stringr::str_trim(x)
  x <- stringr::str_replace_all(x, "(?i)(\\s|,)+technologies(\\s|,){0,1}", " ")
  x <- stringr::str_trim(x)
  x <- stringr::str_replace_all(x, "(?i)(\\s|,)+limited(\\s|,){0,1}", " ")
  x <- stringr::str_trim(x)
  x <- stringr::str_replace_all(x, "(?i)(\\s|,)+[\\d\\-]+(\\s)*", " ")
  x <- stringr::str_trim(x)
  x <- stringr::str_replace_all(x, "(?i)(\\s|,|-)*(development|core){0,1}(\\s|,|-)*(team|company){0,1}$", " ")
  x <- stringr::str_trim(x)
  x <- stringr::str_replace_all(x, "(?i)(\\s|,)+(co|corp|corporation|ltd|llc|cc|inc|incorporated|company|international)\\.{0,1}$", "")
  x <- stringr::str_trim(x)
  x <- sapply(x, function(y) xml2::xml_text(xml2::read_html(paste0("<x>",y,"</x>"))))

  x <- stringr::str_replace_all(x, "^[^a-zA-Z0-9]+$", "")
  x <- stringr::str_replace_all(x, "^[^a-zA-Z0-9]+\\s([a-zA-Z0-9].+)$", "\\1")
  x <- stringr::str_replace_all(x, "^\\${0,1}\\{(.+)\\}$", "")
  x <- stringr::str_replace_all(x, "^\\$(.+)\\$$", "")
  x <- stringr::str_replace_all(x, "\\(\\)", "")
  x <- stringr::str_replace_all(x, "\\(([^\\)]+)$", "")
  x <- stringr::str_replace_all(x, "\\[\\]", "")
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

