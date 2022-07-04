
#' Title
#'
#' @param cpes
#' @param num_samples
#'
#' @return
#' @export
cpe_train_set <- function(cpes = mitre::cpe.nist, num_samples = 1000) {
  suppressPackageStartupMessages(library(dplyr))
  df <- cpes %>%
    select(id, title, vendor, product, version) %>%
    mutate(title_len = nchar(title)) %>%
    mutate(vendor_len = nchar(vendor)) %>%
    mutate(product_len = nchar(product)) %>%
    mutate(version_len = nchar(version))

  sts_t25 <- as.integer(quantile(df$title_len, 0.25))
  sts_t75 <- as.integer(quantile(df$title_len, 0.75))

  df_25 <- df %>% filter(title_len <= sts_t25)
  df_50 <- df %>% filter(title_len > sts_t25, title_len < sts_t75)
  df_75 <- df %>% filter(title_len >= sts_t75)
  nbias <- num_samples %% 3

  df_sam <- df_25 %>% sample_n(num_samples/3)
  df_sam <- df_sam %>% bind_rows(df_50 %>% sample_n((num_samples/3) + nbias))
  df_sam <- df_sam %>% bind_rows(df_75 %>% sample_n(num_samples/3))

  return(df_sam)
}

#' Title
#'
#' @param cpes
#' @param overlap
#' @param columns
#'
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#'  df <- ner_tags_cpe()
#' }
ner_tags_cpe <- function(cpes = mitre::cpe.nist, overlap = TRUE, columns = "all") {
  df <- cpes[ , c("title", "vendor", "product", "version")]

  df$title <- tolower(df$title)
  df$vendor <- stringr::str_replace_all(string = df$vendor, pattern = "\\\\",  replacement = "")
  df$vendor <- stringr::str_replace_all(string = df$vendor, pattern = "_",  replacement = " ")
  df$product <- stringr::str_replace_all(string = df$product, pattern = "\\\\",  replacement = "")
  df$product <- stringr::str_replace_all(string = df$product, pattern = "_",  replacement = " ")
  df$version <- stringr::str_replace_all(string = df$version, pattern = "\\\\",  replacement = "")
  df$version <- stringr::str_replace_all(string = df$version, pattern = "_",  replacement = " ")

  if (!overlap) {
    # Replace version "-" by "*", which will prevent overlapping
    df$version <- stringr::str_replace_all(string = df$version, pattern = "^\\-$",  replacement = "*")
  }

  df$annotations <- rep(NA, nrow(df))
  for (i in 1:nrow(df)) {
    tit <- df$title[i]
    entities <- data.frame(start = integer(),
                           end = integer(),
                           label = character())
    # Check overlapping vendor-product
    if (!overlap) {
      if (stringr::str_detect(df$vendor[i], stringr::fixed(df$product[i])) |
          stringr::str_detect(df$product[i], stringr::fixed(df$vendor[i]))) {
        next
      }
    }

    if (any(columns %in% c("all", "vendor"))) {
      # Annotate Vendor
      pos_vend <- stringr::str_locate_all(tit, stringr::fixed(df$vendor[i]))[[1]]
      a_vend <- NA
      if (length(pos_vend) > 0) {
        a_vend <- cbind(as.data.frame(pos_vend - 1), label = "CPE_VENDOR")
        tit <- paste0("[", stringr::str_sub(tit, a_vend$start, a_vend$end + 1),
                      "](", a_vend$label, ")",
                      stringr::str_sub(tit, a_vend$end + 2, nchar(tit)))
      }

    }

    if (any(columns %in% c("all", "product"))) {
      # Annotate Product
      pos_prod <- stringr::str_locate_all(tit, stringr::fixed(df$product[i]))[[1]]
      a_prod <- NA
      if (length(pos_prod) > 0) {
        a_prod <- cbind(as.data.frame(pos_prod - 1), label = "CPE_PRODUCT")[nrow(pos_prod), ]
        tit <- paste0(stringr::str_sub(tit, 0, a_prod$start),
                      "[", stringr::str_sub(tit, a_prod$start + 1, a_prod$end + 1),
                      "](", a_prod$label, ")",
                      stringr::str_sub(tit, a_prod$end + 2, nchar(tit)))
      }
    }

    if (any(columns %in% c("all", "version"))) {
      # Annotate Version
      pos_vers <- stringr::str_locate_all(tit, stringr::fixed(df$version[i]))[[1]]
      a_vers <- NA
      if (length(pos_vers) > 0) {
        a_vers <- cbind(as.data.frame(pos_vers - 1), label = "CPE_VERSION")
        tit <- paste0(stringr::str_sub(tit, 0, a_vers$start),
                      "[", stringr::str_sub(tit, a_vers$start + 1, a_vers$end + 1),
                      "](", a_vers$label, ")",
                      stringr::str_sub(tit, a_vers$end + 2, nchar(tit)))
      }
    }

    df$annotations[i] <- tit

    if ((i %% 1000) == 0) print(paste0("[.] step ", i, " ..."))
  }

  df <- df[!is.na(df$annotations), ]

  return(df)
}

#' Title
#'
#' @param cpes
#' @param overlap
#' @param columns
#'
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#'  df <- ner_offsets_cpe()
#'  write.csv(df[, c("title", "annotations")], "inst/extdata/cpes_notoverlap.csv",
#'            row.names = FALSE, fileEncoding = "UTF-8")
#' }
ner_offsets_cpe <- function(cpes = mitre::cpe.nist, overlap = TRUE, columns = "all") {
  df <- cpes[ , c("title", "vendor", "product", "version")]

  df$title <- tolower(df$title)
  df$vendor <- stringr::str_replace_all(string = df$vendor, pattern = "\\\\",  replacement = "")
  df$vendor <- stringr::str_replace_all(string = df$vendor, pattern = "_",  replacement = " ")
  df$product <- stringr::str_replace_all(string = df$product, pattern = "\\\\",  replacement = "")
  df$product <- stringr::str_replace_all(string = df$product, pattern = "_",  replacement = " ")
  df$version <- stringr::str_replace_all(string = df$version, pattern = "\\\\",  replacement = "")
  df$version <- stringr::str_replace_all(string = df$version, pattern = "_",  replacement = " ")

  if (!overlap) {
    # Replace version "-" by "*", which will prevent overlapping
    df$version <- stringr::str_replace_all(string = df$version, pattern = "^\\-$",  replacement = "*")
  }

  df$annotations <- rep(NA, nrow(df))
  for (i in 1:nrow(df)) {
    entities <- data.frame(start = integer(),
                           end = integer(),
                           label = character())
    # Check overlapping vendor-product
    if (!overlap) {
      if (stringr::str_detect(df$vendor[i], stringr::fixed(df$product[i])) |
          stringr::str_detect(df$product[i], stringr::fixed(df$vendor[i]))) {
        next
      }
    }

    if (any(columns %in% c("all", "vendor"))) {
      # Annotate Vendor
      pos_vend <- stringr::str_locate_all(df$title[i], stringr::fixed(df$vendor[i]))[[1]]
      a_vend <- NA
      if (length(pos_vend) > 0) {
        a_vend <- cbind(as.data.frame(pos_vend - 1), label = "CPE_VENDOR")
      }

    }

    if (any(columns %in% c("all", "product"))) {
      # Annotate Product
      pos_prod <- stringr::str_locate_all(df$title[i], stringr::fixed(df$product[i]))[[1]]
      a_prod <- NA
      if (length(pos_prod) > 0) {
        a_prod <- cbind(as.data.frame(pos_prod - 1), label = "CPE_PRODUCT")[nrow(pos_prod), ]
      }
    }

    if (any(columns %in% c("all", "version"))) {
      # Annotate Version
      pos_vers <- stringr::str_locate_all(df$title[i], stringr::fixed(df$version[i]))[[1]]
      a_vers <- NA
      if (length(pos_vers) > 0) {
        a_vers <- cbind(as.data.frame(pos_vers - 1), label = "CPE_VERSION")
      }
    }

    if (any(columns %in% c("all", "vendor"))) {
      if (all(!is.na(a_vend))) entities <- rbind(entities, a_vend)
    }
    if (any(columns %in% c("all", "product"))) {
      if (all(!is.na(a_prod))) entities <- rbind(entities, a_prod)
    }
    if (any(columns %in% c("all", "version"))) {
      if (all(!is.na(a_vers))) entities <- rbind(entities, a_vers)
    }


    df$annotations[i] <- jsonlite::toJSON(entities)

    if ((i %% 1000) == 0) print(paste0("[.] step ", i, " ..."))
  }

  df <- df[!is.na(df$annotations), ]

  return(df)
}

#' Title
#'
#' @param taste
#' @param add_tab
#' @param add_enter
#'
#' @return
#' @export
ml_cpe_valid_chars <- function(taste = c("char", "dec", "hex")[1],
                               add_tab = FALSE, add_enter = FALSE) {

  valid_dec_chars <- c(32,33,38,40,41, 43:58, 65:90, 97:122)
  if (add_enter) valid_dec_chars <- c(10, valid_dec_chars)
  if (add_tab) valid_dec_chars <- c(9, valid_dec_chars)

  if (taste == "char") {
    valid_chars <- sapply(valid_dec_chars, DescTools::AscToChar)
  } else if (taste == "hex") {
    valid_chars <- sapply(valid_dec_chars, DescTools::DecToHex)
  } else {
    return(valid_dec_chars)
  }

  return(valid_chars)
}

#' Title
#'
#' @param name
#'
#' @return
#' @export
cpe_encode_name <- function(name = "", lower = TRUE) {
  encname <- iconv(name, to = 'ASCII//TRANSLIT')

  valid_dec_chars <- c(32,33,38,40,41, 43:58, 65:90, 97:122)
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
  encname <- stringi::stri_replace_all_regex(encname, regex_notvalid, "*")
  if (lower) encname <- tolower(encname)

  return(encname)
}
