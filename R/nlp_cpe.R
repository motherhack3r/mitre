# ONLY NEEDED FOR VULNDIGGER


#' Load CPE data frame from local file or download latest
#'
#' @param local_path path to RDS file. NA value implies remote TRUE
#' @param remote logical
#' @param keep_deprecated logical
#'
#' @return data.frame
#' @export
#'
#' @examples
#' cpes <- mitre::cpe_latest_date(local_path = "inst/extdata/cpe.nist.rds")
cpe_latest_data <- function(local_path = NA, remote = F, keep_deprecated = F) {
  if (is.na(local_path) | remote) {
    local_path <- tempfile(fileext = ".rds")
    download.file(url = "https://github.com/motherhack3r/mitre-datasets/raw/master/latest/simple/cpe.rds",
                  destfile = local_path)
  }
  cpes <- readRDS(local_path)
  if (!keep_deprecated) {
    cpes <- cpes[!cpes$deprecated, ]
  }
  cpes <- cpes[, c("id", "title", "part", "vendor", "product", "version")]

  return(cpes)
}

#' Returns data frame with grouped count by vendor and product.
#'
#' @param df data.frame
#' @param only_vendor logical
#'
#' @return data.frame
#' @export
cpe_stats <- function(df = cpe_latest_data(), only_vendor = TRUE) {
  if (only_vendor) {
    sts_cpes <- dplyr::count(df, vendor, sort = TRUE)
  } else {
    sts_cpes <- dplyr::count(df, vendor, product, sort = TRUE)
  }

  return(sts_cpes)
}

#' Returns a custom list of valid chars defined in notebook Explore_CPE_charset.
#' The parameters are used to customize the output.
#'
#' @param taste character, type of output "char", "dec", or "hex"
#' @param add_tab logical, include tab char
#' @param add_enter logical, include enter char
#' @param add_underline logical, include "_"
#' @param type character, returns valid chars for "input" or "output"
#'
#' @return character
#' @export
cpe_valid_chars <- function(taste = c("char", "dec", "hex")[1],
                            add_tab = FALSE, add_enter = FALSE,
                            add_underline = FALSE,
                            type = c("input", "output")[1]) {

  # Subset of CPE accepted characters in WFN
  valid_chars <- c(32,33,38,40,41, 43:58, 65:90, 97:122)
  if (type == "output")
    # lowercase
    valid_chars <- c(32,33,38,40,41, 43:58, 97:122)

  # Add extra characters
  if (add_enter) valid_chars <- c(10, valid_chars)
  if (add_tab) valid_chars <- c(9, valid_chars)
  if (add_underline) valid_chars <- c(95, valid_chars)

  # output as characters, hexadecimal or decimal
  if (taste == "char") {
    valid_chars <- sapply(valid_chars, DescTools::AscToChar)
  } else if (taste == "hex") {
    valid_chars <- sapply(valid_chars, DescTools::DecToHex)
  }

  return(valid_chars)
}


#' Used by generic annotate cpes function
#'
#' @param df data.frame
#' @param type character
#'
#' @return data.frame
#' @export
cpe_add_notation <- function(df = cpe_latest_data(),
                             type = c("vpv", "vp", "pv", "vv", "vend", "prod", "vers")[1]) {

  df_ner <- df
  # Method: Use specific regex for each case and replace matches with RASA.
  print(paste0("[*] ", "RASA notation..."))
  # remove rows with escaped chars because of tagging regex
  df_ner <- df_ner[!grepl(pattern = "\\\\", df_ner$vendor), ]
  df_ner <- df_ner[!grepl(pattern = "\\\\", df_ner$product), ]
  df_ner <- df_ner[!grepl(pattern = "\\\\", df_ner$version), ]
  # replace WFN _ to space in vendor and product
  df_ner$vendor <- stringr::str_replace_all(df_ner$vendor, "_", " ")
  df_ner$product <- stringr::str_replace_all(df_ner$product, "_", " ")
  # remove titles with equal vendor and product
  df_ner <- df_ner[which(df_ner$vendor != df_ner$product), ]
  # vendor candidates
  df_ner$train_v <- rep(F, nrow(df_ner))
  df_ner$train_v <- stringr::str_detect(df_ner$title, stringr::fixed(df_ner$vendor, ignore_case = TRUE))
  # product candidates
  df_ner$train_p <- rep(F, nrow(df_ner))
  df_ner$train_p <- stringr::str_detect(df_ner$title, stringr::fixed(df_ner$product, ignore_case = TRUE))
  # version candidates
  df_ner$train_r <- rep(F, nrow(df_ner))
  df_ner$train_r <- stringr::str_detect(df_ner$title, stringr::fixed(df_ner$version, ignore_case = TRUE))

  if (type == "vpv") {
    # Keep only titles with all entities
    df_ner <- dplyr::filter(df_ner, train_v & train_p & train_r)
    df_ner <- dplyr::select(df_ner, -train_v, -train_p, -train_r)
    ## vendor + product + version
    df_ner$annotated <- stringr::str_replace_all(string = df_ner$title,
                                                 pattern = paste0("(?i)(.*)(", df_ner$vendor,")(\\s.*)(", df_ner$product,")(.*)(", df_ner$version,")(.*)"),
                                                 replacement = "\\1\\[\\2\\]\\(cpe_vendor\\)\\3\\[\\4\\]\\(cpe_product\\)\\5\\[\\6\\]\\(cpe_version\\)\\7")
    df_ner <- df_ner[which(grepl(pattern = ".*\\(cpe_vendor\\).*\\(cpe_product\\).*\\(cpe_version\\).*", df_ner$annotated)), ]
  } else if (type == "vp") {
    # Keep only titles with vendor and product entities
    df_ner <- dplyr::filter(df_ner, train_v & train_p)
    df_ner <- dplyr::select(df_ner, -train_v, -train_p, -train_r)
    ## vendor + product
    df_ner$annotated <- stringr::str_replace_all(string = df_ner$title,
                                                 pattern = paste0("(?i)(.*)(", df_ner$vendor,")(\\s.*)(", df_ner$product,")(.*)"),
                                                 replacement = "\\1\\[\\2\\]\\(cpe_vendor\\)\\3\\[\\4\\]\\(cpe_product\\)\\5")
    df_ner <- df_ner[which(grepl(pattern = ".*\\(cpe_vendor\\).*\\(cpe_product\\).*", df_ner$annotated)), ]
  } else if (type == "pv") {
    # Keep only titles with product and version entities
    df_ner <- dplyr::filter(df_ner, train_p & train_r)
    df_ner <- dplyr::select(df_ner, -train_v, -train_p, -train_r)
    ## product + version
    df_ner$annotated <- stringr::str_replace_all(string = df_ner$title,
                                                 pattern = paste0("(?i)(.*)(", df_ner$product,")(\\s.*)(", df_ner$version,")(.*)"),
                                                 replacement = "\\1\\[\\2\\]\\(cpe_product\\)\\3\\[\\4\\]\\(cpe_version\\)\\5")
    df_ner <- df_ner[which(grepl(pattern = ".*\\(cpe_product\\).*\\(cpe_version\\).*", df_ner$annotated)), ]
  } else if (type == "vv") {
    # Keep only titles with vendor and version entities
    df_ner <- dplyr::filter(df_ner, train_v & train_r)
    df_ner <- dplyr::select(df_ner, -train_v, -train_p, -train_r)
    ## vendor + version
    df_ner$annotated <- stringr::str_replace_all(string = df_ner$title,
                                                 pattern = paste0("(?i)(.*)(", df_ner$vendor,")(\\s.*)(", df_ner$version,")(.*)"),
                                                 replacement = "\\1\\[\\2\\]\\(cpe_vendor\\)\\3\\[\\4\\]\\(cpe_version\\)\\5")
    df_ner <- df_ner[which(grepl(pattern = ".*\\(cpe_vendor\\).*\\(cpe_version\\).*", df_ner$annotated)), ]
  } else if (type == "vend") {
    # Keep only titles with vendor entity
    df_ner <- dplyr::filter(df_ner, train_v)
    df_ner <- dplyr::select(df_ner, -train_v, -train_p, -train_r)
    ## vendor
    df_ner$annotated <- stringr::str_replace_all(string = df_ner$title,
                                                 pattern = paste0("(?i)(.*)(", df_ner$vendor,")(\\s.*)(.*)"),
                                                 replacement = "\\1\\[\\2\\]\\(cpe_vendor\\)\\3")
    df_ner <- df_ner[which(grepl(pattern = ".*\\(cpe_vendor\\).*", df_ner$annotated)), ]
  } else if (type == "prod") {
    # Keep only titles with product entity
    df_ner <- dplyr::filter(df_ner, train_p)
    df_ner <- dplyr::select(df_ner, -train_v, -train_p, -train_r)
    ## product
    df_ner$annotated <- stringr::str_replace_all(string = df_ner$title,
                                                 pattern = paste0("(?i)(.*)(", df_ner$product,")(\\s.*)(.*)"),
                                                 replacement = "\\1\\[\\2\\]\\(cpe_product\\)\\3")
    df_ner <- df_ner[which(grepl(pattern = ".*\\(cpe_product\\).*", df_ner$annotated)), ]
  } else if (type == "vers") {
    # Keep only titles with version entity
    df_ner <- dplyr::filter(df_ner, train_r)
    df_ner <- dplyr::select(df_ner, -train_v, -train_p, -train_r)
    ## version
    df_ner$annotated <- stringr::str_replace_all(string = df_ner$title,
                                                 pattern = paste0("(?i)(.*)(", df_ner$version,")(\\s.*)(.*)"),
                                                 replacement = "\\1\\[\\2\\]\\(cpe_version\\)\\3")
    df_ner <- df_ner[which(grepl(pattern = ".*\\(cpe_version\\).*", df_ner$annotated)), ]
  } else {
    df_ner <- dplyr::select(df_ner, -train_v, -train_p, -train_r)
    print("[ERROR] type not valid. Read manual to check allowed values.")
  }

  df <- dplyr::left_join(df, df_ner[, c("id", "annotated")], by = "id")

  return(df)
}

#' Title
#'
#' @param df data.frame
#'
#' @return data.frame
#' @export
cpe_add_features <- function(df = cpe_latest_data()) {
  df <- dplyr::select(df, c("id", "title", "part", "vendor", "product", "version"))

  df$len_title <- stringr::str_length(df$title)
  df$len_vendor <- stringr::str_length(df$vendor)
  df$len_product <- stringr::str_length(df$product)
  df$len_version <- stringr::str_length(df$version)

  df$num_title <- stringr::str_count(df$title, "[0-9]") / df$len_title
  df$num_vendor <- stringr::str_count(df$vendor, "[0-9]") / df$len_vendor
  df$num_product <- stringr::str_count(df$product, "[0-9]") / df$len_product
  df$num_version <- stringr::str_count(df$version, "[0-9]") / df$len_version

  df$abc_title <- stringr::str_count(df$title, "[a-zA-Z]") / df$len_title
  df$abc_vendor <- stringr::str_count(df$vendor, "[a-zA-Z]") / df$len_vendor
  df$abc_product <- stringr::str_count(df$product, "[a-zA-Z]") / df$len_product
  df$abc_version <- stringr::str_count(df$version, "[a-zA-Z]") / df$len_version

  df$dot_title <- stringr::str_count(df$title, "[\\.\\_]") / df$len_title
  df$dot_vendor <- stringr::str_count(df$vendor, "[\\_]") / df$len_vendor
  df$dot_product <- stringr::str_count(df$product, "[\\_]") / df$len_product
  df$dot_version <- stringr::str_count(df$version, "[\\.]") / df$len_version

  df$sym_title <- stringr::str_count(df$title, "[^0-9a-zA-Z]") / df$len_title
  df$sym_vendor <- stringr::str_count(df$vendor, "[^0-9a-zA-Z\\_]") / df$len_vendor
  df$sym_product <- stringr::str_count(df$product, "[^0-9a-zA-Z\\_]") / df$len_product
  df$sym_version <- stringr::str_count(df$version, "[^0-9a-zA-Z\\.]") / df$len_version

  df$train_vendor <- F | ((df$sym_vendor < 0.05) & ((df$abc_vendor + df$dot_vendor) > 0.5))
  df$train_product <- F | ((df$sym_product < 0.2) & ((df$abc_product + df$dot_product) > 0.8))
  df$train_version <- F | ((df$abc_version < 0.3) & ((df$num_version + df$dot_version) > 0.5))

  return(df)
}
