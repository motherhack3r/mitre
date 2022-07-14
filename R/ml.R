# REF: https://sanjayasubedi.com.np/deeplearning/training-ner-with-huggingface-transformer/

#' Title
#'
#' @param cpes
#' @param num_samples
#' @param samples_mix
#'
#' @return
#' @export
createTrainSample <- function(cpes = data.frame(), num_samples = 5000,
                              samples_mix = c(min = 0.6,
                                              med = 0.25,
                                              max = 0.15)) {
  # part is not included in title
  df_ner <- cpes %>% select(id, title, vendor, product, version)
  # remove rows with escaped chars due to regex for tagging
  df_ner <- df_ner[!grepl(pattern = "\\\\", df_ner$vendor), ]
  df_ner <- df_ner[!grepl(pattern = "\\\\", df_ner$product), ]
  df_ner <- df_ner[!grepl(pattern = "\\\\", df_ner$version), ]
  # replace WFN _ to space
  df_ner$vendor <- stringr::str_replace_all(df_ner$vendor, "_", " ")
  df_ner$product <- stringr::str_replace_all(df_ner$product, "_", " ")
  # remove titles with equal vendor and product
  df_ner <- df_ner[which(df_ner$vendor != df_ner$product), ]
  # lowercase title
  df_ner$title <- tolower(df_ner$title)
  # vendor entities candidates
  df_ner$train_v <- rep(F, nrow(df_ner))
  df_ner$train_v <- stringr::str_detect(df_ner$title, stringr::fixed(df_ner$vendor))
  # product entities candidates
  df_ner$train_p <- rep(F, nrow(df_ner))
  df_ner$train_p <- stringr::str_detect(df_ner$title, stringr::fixed(df_ner$product))
  # version entities candidates
  df_ner$train_r <- rep(F, nrow(df_ner))
  df_ner$train_r <- stringr::str_detect(df_ner$title, stringr::fixed(df_ner$version))
  # Keep only titles with all entities
  df_ner <- df_ner %>% filter(train_v & train_p & train_r) %>% select(-train_v, -train_p, -train_r)

  # Add tags
  df_ner$annotated <- df_ner$title

  ## vendor + product + version
  df_ner$annotated <- str_replace_all(string = df_ner$annotated,
                                      pattern = paste0("(.*)(", df_ner$vendor,")(\\s.*)(", df_ner$product,")(.*)(", df_ner$version,")(.*)"),
                                      replacement = "\\1\\\\[\\2\\]\\(cpe_vendor\\)\\3\\[\\4\\]\\(cpe_product\\)\\5\\[\\6\\]\\(cpe_version\\)\\7\\")

  ## Keep rows that miss a cpe element
  df_not_ner <- df_ner[which(!grepl(pattern = ".*\\]\\(cpe_vendor\\).*\\]\\(cpe_product\\).*\\]\\(cpe_version\\).*", df_ner$annotated)), ]
  df_ner_vpr <- df_ner[which(grepl(pattern = ".*\\]\\(cpe_vendor\\).*\\]\\(cpe_product\\).*\\]\\(cpe_version\\).*", df_ner$annotated)), ]

  ## product + version
  df_not_ner$annotated <- str_replace_all(string = df_not_ner$annotated,
                                      pattern = paste0("(.*)(", df_not_ner$product,")(.*)(", df_not_ner$version,")(.*)"),
                                      replacement = "\\1\\\\[\\2\\]\\(cpe_product\\)\\3\\[\\4\\]\\(cpe_version\\)\\5")
  df_ner_pr <- df_not_ner[which(grepl(pattern = ".*\\]\\(cpe_product\\).*\\]\\(cpe_version\\).*", df_not_ner$annotated)), ]
  df_not_ner <- df_not_ner[which(!grepl(pattern = ".*\\]\\(cpe_product\\).*\\]\\(cpe_version\\).*", df_not_ner$annotated)), ]

  ## vendor + product
  df_not_ner$annotated <- str_replace_all(string = df_not_ner$annotated,
                                          pattern = paste0("(.*)(", df_not_ner$vendor,")(.*)(", df_not_ner$product,")(.*)"),
                                          replacement = "\\1\\\\[\\2\\]\\(cpe_vendor\\)\\3\\[\\4\\]\\(cpe_product\\)\\5")
  df_ner_vp <- df_not_ner[which(grepl(pattern = ".*\\]\\(cpe_vendor\\).*\\]\\(cpe_product\\).*", df_not_ner$annotated)), ]
  df_not_ner <- df_not_ner[which(!grepl(pattern = ".*\\]\\(cpe_vendor\\).*\\]\\(cpe_product\\).*", df_not_ner$annotated)), ]

  ## product
  df_not_ner$annotated <- str_replace_all(string = df_not_ner$annotated,
                                          pattern = paste0("(.*)(", df_not_ner$product,")(.*)"),
                                          replacement = "\\1\\\\[\\2\\]\\(cpe_product\\)\\3")
  df_ner_p <- df_not_ner[which(grepl(pattern = ".*\\]\\(cpe_product\\).*", df_not_ner$annotated)), ]
  df_not_ner <- df_not_ner[which(!grepl(pattern = ".*\\]\\(cpe_product\\).*", df_not_ner$annotated)), ]

  df_ner <- rbind(df_ner_vpr, df_ner_vp, df_ner_pr, df_ner_p)
  rm(df_ner_vpr, df_ner_vp, df_ner_pr, df_ner_p)

  # Select rows for training set
  #  - min_vendors with less than 0.9 titles
  #  - med_vendors from 0.9 to 0.9975 titles
  #  - max_vendors more than 0.9975 titles (outliers)
  min_vendors <- table(cpes$vendor)
  sts_min <- as.integer(quantile(min_vendors, 0.9))
  sts_max <- as.integer(quantile(min_vendors, 0.9975))

  max_vendors <- names(min_vendors[min_vendors > sts_max])
  min_vendors <- names(min_vendors[min_vendors < sts_min])
  med_vendors <- unique(c(min_vendors, max_vendors))
  med_vendors <- sort(unique(cpes$vendor[!(cpes$vendor %in% med_vendors)]))

  train_min <- df_ner[which(df_ner$vendor %in% min_vendors),]
  train_med <- df_ner[which(df_ner$vendor %in% med_vendors),]
  train_max <- df_ner[which(df_ner$vendor %in% max_vendors),]

  nbias <- num_samples %% 3

  df_min <- mitre::cpe_train_set(train_min, round(floor(num_samples) * samples_mix["min"]))
  df_med <- mitre::cpe_train_set(train_med, round((floor(num_samples) + nbias) * samples_mix["med"]))
  df_max <- mitre::cpe_train_set(train_max, round(floor(num_samples) * samples_mix["max"]))


  # df_min <- mitre::cpe_train_set(train_min, floor(num_samples/3))
  # df_med <- mitre::cpe_train_set(train_med, floor(num_samples/3) + nbias)
  # df_max <- mitre::cpe_train_set(train_max, floor(num_samples/3))

  train_df <- bind_rows(df_min, df_med, df_max)

  train_df <- train_df %>% select(id) %>%
    left_join(df_ner, by = "id") %>%
    sample_n(nrow(train_df)) %>% select(title, annotated)


  # Select rows for training set
  #  - min_vendors with less than 100 titles
  #  - med_vendors with 100 to 1000 titles
  #  - max_vendors with more than 1000 titles

  # min_vendors <- table(df_ner$vendor)
  # max_vendors <- names(min_vendors[min_vendors > 1000])
  # min_vendors <- names(min_vendors[min_vendors < 100])
  # med_vendors <- unique(c(min_vendors, max_vendors))
  # med_vendors <- sort(unique(df_ner$vendor[!(df_ner$vendor %in% med_vendors)]))

  # train_min <- df_ner[which(df_ner$vendor %in% min_vendors),] %>%
  #   sample_n(round(num_samples * samples_mix["min"]))
  # train_med <- df_ner[which(df_ner$vendor %in% med_vendors),] %>%
  #   sample_n(round(num_samples * samples_mix["med"]))
  # train_max <- df_ner[which(df_ner$vendor %in% max_vendors),] %>%
  #   sample_n(round(num_samples * samples_mix["max"]))

  # train_df <- bind_rows(train_min, train_med, train_max)

  # train_df <- train_df %>% sample_n(nrow(train_df)) %>% select(title, annotated)

  return(train_df)
}

#' Title
#'
#' @param cpes
#' @param num_samples
#'
#' @return
#' @export
cpe_train_set <- function(cpes = mitre::cpe.nist, num_samples = 1000) {
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
  df <- cpes[ , c("id", "title", "vendor", "product", "version")]

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
