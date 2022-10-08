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
#' @export
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
#' @export
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
#' @export
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


#' Transform the CPEs data frame removing unnecessary columns. Change default parameters
#' to remove deprecated or those that doesn't follow the WFN definition.
#'
#' @param df data.frame default mitre cpes
#' @param keep_deprecated logical, condition to keep or remove deprecated CPEs
#' @param only_wfn logical, condition to keep or remove CPEs following WFN definition
#'
#' @return data.frame
#' @export
nlp_cpe_dataset <- function(df = mitre::cpe.nist, keep_deprecated = FALSE, only_wfn = TRUE) {
  df$id <- 1:nrow(df)
  if (!keep_deprecated) {
    df <- df[!df$deprecated, ]
  }

  if (only_wfn) {
    df$valid <- stringr::str_detect(str73enc(df$title), "\\*", negate = T)
    df <- df[df$valid, ]
    df$valid <- stringr::str_detect(str49enc(df$vendor), "\\*", negate = T)
    df <- df[df$valid, ]
    df$valid <- stringr::str_detect(str49enc(df$product), "\\*", negate = T)
    df <- df[df$valid, ]
    df$valid <- stringr::str_detect(str49enc(df$version), "\\*", negate = T)
    df <- df[df$valid, ]
  }
  df <- df[, c("id", "title", "part", "vendor", "product", "version")]

  return(df)
}


#' Used by generic annotate cpes function
#'
#' @param df_ner data.frame
#' @param type character
#'
#' @return data.frame
nlp_cpe_rasa_notation <- function(df_ner = data.frame(),
                                  type = c("vpv", "vp", "pv", "vv", "vend", "prod", "version")[1]) {

  # Method: Use specific regex for each case and replace matches with RASA.
  print(paste0("[*] ", "RASA notation..."))
  # remove rows with escaped chars because of tagging regex
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

  if (type == "vpv") {
    # Keep only titles with all entities
    df_ner <- dplyr::filter(df_ner, train_v & train_p & train_r)
    df_ner <- dplyr::select(df_ner, -train_v, -train_p, -train_r)
    ## vendor + product + version
    df_ner$annotated <- stringr::str_replace_all(string = df_ner$title,
                                                 pattern = paste0("(.*)(", df_ner$vendor,")(\\s.*)(", df_ner$product,")(.*)(", df_ner$version,")(.*)"),
                                                 replacement = "\\1\\[\\2\\]\\(cpe_vendor\\)\\3\\[\\4\\]\\(cpe_product\\)\\5\\[\\6\\]\\(cpe_version\\)\\7")
    df_ner <- df_ner[which(grepl(pattern = ".*\\(cpe_vendor\\).*\\(cpe_product\\).*\\(cpe_version\\).*", df_ner$annotated)), ]
  }
  else if (type == "vp") {
    # Keep only titles with vendor and product entities
    df_ner <- dplyr::filter(df_ner, train_v & train_p)
    df_ner <- dplyr::select(df_ner, -train_v, -train_p, -train_r)
    ## vendor + product
    df_ner$annotated <- stringr::str_replace_all(string = df_ner$title,
                                                 pattern = paste0("(.*)(", df_ner$vendor,")(\\s.*)(", df_ner$product,")(.*)"),
                                                 replacement = "\\1\\[\\2\\]\\(cpe_vendor\\)\\3\\[\\4\\]\\(cpe_product\\)\\5")
    df_ner <- df_ner[which(grepl(pattern = ".*\\(cpe_vendor\\).*\\(cpe_product\\).*", df_ner$annotated)), ]
  }
  else if (type == "pv") {
    # Keep only titles with product and version entities
    df_ner <- dplyr::filter(df_ner, train_p & train_r)
    df_ner <- dplyr::select(df_ner, -train_v, -train_p, -train_r)
    ## product + version
    df_ner$annotated <- stringr::str_replace_all(string = df_ner$title,
                                                 pattern = paste0("(.*)(", df_ner$product,")(\\s.*)(", df_ner$version,")(.*)"),
                                                 replacement = "\\1\\[\\2\\]\\(cpe_product\\)\\3\\[\\4\\]\\(cpe_version\\)\\5")
    df_ner <- df_ner[which(grepl(pattern = ".*\\(cpe_product\\).*\\(cpe_version\\).*", df_ner$annotated)), ]
  }
  else if (type == "vv") {
    # Keep only titles with vendor and version entities
    df_ner <- dplyr::filter(df_ner, train_v & train_r)
    df_ner <- dplyr::select(df_ner, -train_v, -train_p, -train_r)
    ## vendor + version
    df_ner$annotated <- stringr::str_replace_all(string = df_ner$title,
                                                 pattern = paste0("(.*)(", df_ner$vendor,")(\\s.*)(", df_ner$version,")(.*)"),
                                                 replacement = "\\1\\[\\2\\]\\(cpe_vendor\\)\\3\\[\\4\\]\\(cpe_version\\)\\5")
    df_ner <- df_ner[which(grepl(pattern = ".*\\(cpe_vendor\\).*\\(cpe_version\\).*", df_ner$annotated)), ]
  }
  else if (type == "vend") {
    # Keep only titles with vendor entity
    df_ner <- dplyr::filter(df_ner, train_v)
    df_ner <- dplyr::select(df_ner, -train_v, -train_p, -train_r)
    ## vendor
    df_ner$annotated <- stringr::str_replace_all(string = df_ner$title,
                                                 pattern = paste0("(.*)(", df_ner$vendor,")(\\s.*)(.*)"),
                                                 replacement = "\\1\\[\\2\\]\\(cpe_vendor\\)\\3")
    df_ner <- df_ner[which(grepl(pattern = ".*\\(cpe_vendor\\).*", df_ner$annotated)), ]
  }
  else if (type == "prod") {
    # Keep only titles with product entity
    df_ner <- dplyr::filter(df_ner, train_p)
    df_ner <- dplyr::select(df_ner, -train_v, -train_p, -train_r)
    ## product
    df_ner$annotated <- stringr::str_replace_all(string = df_ner$title,
                                                 pattern = paste0("(.*)(", df_ner$product,")(\\s.*)(.*)"),
                                                 replacement = "\\1\\[\\2\\]\\(cpe_product\\)\\3")
    df_ner <- df_ner[which(grepl(pattern = ".*\\(cpe_product\\).*", df_ner$annotated)), ]
  }
  else if (type == "vers") {
    # Keep only titles with version entity
    df_ner <- dplyr::filter(df_ner, train_r)
    df_ner <- dplyr::select(df_ner, -train_v, -train_p, -train_r)
    ## version
    df_ner$annotated <- stringr::str_replace_all(string = df_ner$title,
                                                 pattern = paste0("(.*)(", df_ner$version,")(\\s.*)(.*)"),
                                                 replacement = "\\1\\[\\2\\]\\(cpe_version\\)\\3")
    df_ner <- df_ner[which(grepl(pattern = ".*\\(cpe_version\\).*", df_ner$annotated)), ]
  }
  else {
    df_ner <- dplyr::select(df_ner, -train_v, -train_p, -train_r)
    print("[ERROR] type not valid. Read manual to check allowed values.")
  }

  return(df_ner)
}

#' Create annotations for NER into new column `annotation`. By default, it uses
#' RASA notation and detect vendor, product and version.
#' Rows with overlapped entities are skipped.
#' The output can be configured with parameters:
#'  - `type` to select which entities annotate
#'  - `kind` for notation: RASA, BILUO, entities
#'
#' @param df data.frame nlp_cpe_dataset
#' @param type character, default vpv
#' @param kind character, default RASA
#' @param pydict logical, python offsets begins with 0, otherwise 1
#' @param seed integer, used for reproducible research
#' @param strict logical, used to include subtypes of selected type
#'
#' @return data.frame
#' @export
nlp_cpe_annotate <- function(df = nlp_cpe_dataset(),
                             type = c("vpv", "vp", "pv", "vv", "vend", "prod", "version")[1],
                             kind = c("RASA", "entities", "BILUO")[1],
                             pydict = TRUE,
                             seed = 42,
                             strict = TRUE) {
  set.seed(seed)
  df_ner <- df[, c("id", "title", "vendor", "product", "version")]

  # Add tags
  print(paste0("[*] ", "start annotation process..."))
  df_ner$annotated <- df_ner$title

  if (strict) {
    # Remove rows without version for types: vpv, pv, vv and version
    if (type %in% c("vpv", "pv", "vv", "version")) {
      print(paste0("[+] ", "removing empty versions..."))
      df_ner <- df_ner[stringr::str_detect(string = df_ner$version, pattern = "^\\-$", negate = T), ]
    }
  }

  # Check overlapping vendor-product
  if (type %in% c("vpv", "vp")) {
    print(paste0("[+] ", "removing overlapping vendor-product..."))
    df_ner <- df_ner[!stringr::str_detect(df_ner$vendor, stringr::fixed(df_ner$product)), ]
    df_ner <- df_ner[!stringr::str_detect(df_ner$product, stringr::fixed(df_ner$vendor)), ]
  }

  if (kind == "RASA") {
    df_ner <- nlp_cpe_rasa_notation(df_ner, type)
  } else if (kind == "entities") {
    print(paste0("[*] ", "Entities notation..."))
    # remove rows with escaped chars because of tagging regex
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

    # Find offsets for each Entity (vend, prod, vers)
    print(paste0("[+] ", "vendor offsets..."))
    pos_vend <- dplyr::bind_rows(
      apply(df_ner, 1,
            function(x)
              as.data.frame.matrix(stringi::stri_locate_first(str = x[2],
                                                              regex = stringr::fixed(x[3]))))
    )
    names(pos_vend) <- c("vend_ini", "vend_fin")
    df_ner <- dplyr::bind_cols(df_ner, pos_vend - pydict)
    print(paste0("[+] ", "product offsets..."))
    pos_prod <- dplyr::bind_rows(
      apply(df_ner, 1,
            function(x)
              as.data.frame.matrix(stringi::stri_locate_last(str = x[2],
                                                             regex = stringr::fixed(x[4]))))
    )
    names(pos_prod) <- c("prod_ini", "prod_fin")
    df_ner <- dplyr::bind_cols(df_ner, pos_prod - pydict)
    print(paste0("[+] ", "version offsets..."))
    pos_vers <- dplyr::bind_rows(
      apply(df_ner, 1,
            function(x)
              as.data.frame.matrix(stringi::stri_locate_last(str = x[2],
                                                             regex = stringr::fixed(x[5]))))
    )
    names(pos_vers) <- c("vers_ini", "vers_fin")
    df_ner <- dplyr::ungroup(dplyr::bind_cols(df_ner, pos_vers - pydict))

    df_ner$type_vpv <- (!is.na(df_ner$vend_ini) & !is.na(df_ner$prod_ini) & !is.na(df_ner$vers_ini))
    df_ner$type_vp <- (!is.na(df_ner$vend_ini) & !is.na(df_ner$prod_ini))
    df_ner$type_pv <- (!is.na(df_ner$prod_ini) & !is.na(df_ner$vers_ini))
    df_ner$type_vv <- (!is.na(df_ner$vend_ini) & !is.na(df_ner$vers_ini))
    df_ner$type_vend <- (!is.na(df_ner$vend_ini))
    df_ner$type_prod <- (!is.na(df_ner$prod_ini))
    df_ner$type_vers <- (!is.na(df_ner$vers_ini))

    # Prepare entities
    df_ner$annotated <- jsonlite::toJSON(NULL, null = "list")
    for (i in 1:nrow(df_ner)) {
      a_vend <- NA
      a_prod <- NA
      a_vers <- NA
      if (df_ner$type_vend[i]) {
        a_vend <- data.frame(start = df_ner$vend_ini[i],
                             end = df_ner$vend_fin[i],
                             label = "cpe_vendor")
      }
      if (df_ner$type_prod[i]) {
        a_prod <- data.frame(start = df_ner$prod_ini[i],
                             end = df_ner$prod_fin[i],
                             label = "cpe_product")
      }
      if (df_ner$type_vers[i]) {
        a_vers <- data.frame(start = df_ner$vers_ini[i],
                             end = df_ner$vers_fin[i],
                             label = "cpe_version")
      }

      if (type == "vpv") {
        ent <- rbind(a_vend, a_prod, a_vers)
      } else if (type == "vp") {
        ent <- rbind(a_vend, a_prod)
      } else if (type == "pv") {
        ent <- rbind(a_prod, a_vers)
      } else if (type == "vv") {
        ent <- rbind(a_vend, a_vers)
      } else if (type == "vend") {
        ent <- rbind(a_vend)
      } else if (type == "prod") {
        ent <- rbind(a_prod)
      } else if (type == "vers") {
        ent <- rbind(a_vers)
      }

      ent <- ent[complete.cases(ent),]
      df_ner$annotated[i] <- jsonlite::toJSON(ent, null = "list", na = "null")

      if ((i %% 10000) == 0) print(paste0("[.] ", i, " rows..."))
    }
    df_ner <- df_ner[(df_ner$annotated != "[]"), c("id", "title", "vendor", "product", "version", "annotated")]
  } else if (kind == "BILUO") {

  }

  return(df_ner)
}


#' Returns data frame with grouped count by vendor and product.
#'
#' @param df data.frame
#' @param scale_log logical
#' @param only_vendor logical
#'
#' @return data.frame
#' @export
getCPEstats <- function(df = cpe.nist, only_vendor = TRUE, scale_log = FALSE) {
  if (only_vendor) {
    sts_cpes <- dplyr::count(df, vendor, sort = TRUE)
  } else {
    sts_cpes <- dplyr::count(df, vendor, product, sort = TRUE)
  }
  if (scale_log) {
    sts_cpes$n <- log(sts_cpes$n)
  }

  return(sts_cpes)
}

#' Title
#'
#' @param df data.frame
#' @param num_samples integer
#' @param seed integer
#' @param quantiles numeric
#'
#' @return data.frame
#' @export
nlp_cpe_sample_dataset <- function(df = nlp_cpe_dataset(),
                                   num_samples = 1000,
                                   seed = 42,
                                   quantiles = c(0, 0.8, 0.9, 0.99, 1)) {

  sts_vend <- getCPEstats(df, only_vendor = TRUE)
  sts_qntl <- quantile(sts_vend$n, probs = quantiles)

  df02 <- sts_vend[sts_vend$n <= sts_qntl[2], ]
  df02 <- dplyr::inner_join(df, df02, by = c("vendor" = "vendor"))
  df25 <- sts_vend[((sts_qntl[2] < sts_vend$n) & (sts_vend$n <= sts_qntl[3])), ]
  df25 <- dplyr::inner_join(df, df25, by = c("vendor" = "vendor"))
  df57 <- sts_vend[((sts_qntl[3] < sts_vend$n) & (sts_vend$n <= sts_qntl[4])), ]
  df57 <- dplyr::inner_join(df, df57, by = c("vendor" = "vendor"))
  df70 <- sts_vend[sts_vend$n > sts_qntl[4], ]
  df70 <- dplyr::inner_join(df, df70, by = c("vendor" = "vendor"))

  # Sampling using quantiles to select slices
  nbias <- num_samples %% 4

  df_sam <- dplyr::sample_n(df02, num_samples/4)
  df_sam <- dplyr::bind_rows(df_sam, dplyr::sample_n(df25, (num_samples/4) + nbias))
  df_sam <- dplyr::bind_rows(df_sam, dplyr::sample_n(df57, (num_samples/4)))
  df_sam <- dplyr::bind_rows(df_sam, dplyr::sample_n(df70, (num_samples/4)))

  df_sam <- dplyr::sample_n(df_sam, nrow(df_sam))
  df_sam <- df_sam[, c("id", "title", "part", "vendor", "product", "version")]

  return(df_sam)
}

#' Title
#'
#' @param df data.frame
#' @param scale_log logical
#'
#' @return data.frame
#' @export
nlp_cpe_feateng <- function(df = nlp_cpe_dataset(), scale_log = FALSE) {

  df$len_title <- stringr::str_length(df$title)
  df$len_vendor <- stringr::str_length(df$vendor)
  df$len_product <- stringr::str_length(df$product)
  df$len_version <- stringr::str_length(df$version)

  df$num_title <- stringr::str_count(df$title, "[0-9]")
  df$num_vendor <- stringr::str_count(df$vendor, "[0-9]")
  df$num_product <- stringr::str_count(df$product, "[0-9]")
  df$num_version <- stringr::str_count(df$version, "[0-9]")

  df$abc_title <- stringr::str_count(df$title, "[a-zA-Z]")
  df$abc_vendor <- stringr::str_count(df$vendor, "[a-zA-Z]")
  df$abc_product <- stringr::str_count(df$product, "[a-zA-Z]")
  df$abc_version <- stringr::str_count(df$version, "[a-zA-Z]")

  df$sym_title <- stringr::str_count(df$title, "[^0-9a-zA-Z]")
  df$sym_vendor <- stringr::str_count(df$vendor, "[^0-9a-zA-Z]")
  df$sym_product <- stringr::str_count(df$product, "[^0-9a-zA-Z]")
  df$sym_version <- stringr::str_count(df$version, "[^0-9a-zA-Z]")

  df$dot_version <- stringr::str_count(df$version, "[\\.]")

  df <- dplyr::mutate(df, pct_version = ((num_version + dot_version)/len_version)-(abc_version/len_version))
  if (scale_log) {
    log_cpes <- dplyr::select(df, -c("title", "part", "vendor", "product", "version"))
    log_cpes <- as.data.frame.matrix(apply(log_cpes, 2, function(x) log(x + 1)))

    df <- cbind(dplyr::select(df, c("title", "part", "vendor", "product", "version")), log_cpes)
  }

  return(df)
}


#' Title
#'
#' @param seed integer
#' @param num_samples integer
#' @param mix_config list
#' @param ner_config list
#' @param save_path character
#' @param verbose logical
#' @param train_codename character
#'
#' @return data.frame
#' @export
nlp_cpe_build_ner_trainset <- function(seed = 42,
                                       num_samples = 10000,
                                       mix_config = list(keep_deprecated = FALSE,
                                                         only_wfn = FALSE,
                                                         scale_log = FALSE,
                                                         sample_weight = c("pca", "vendor", "none")[1],
                                                         pca_features = c("len_vendor", "abc_vendor", "num_version", "len_version", "abc_product"),
                                                         vendor_qntl = c(0, 0.8, 0.9, 0.99, 1)),
                                       ner_config = list(notation = c("rasa", "offset")[1],
                                                         vendor = TRUE,
                                                         product = TRUE,
                                                         version = TRUE),
                                       save_path = file.path("C:", "DEVEL", "code", "data", "ner_trainsets"),
                                       train_codename = "",
                                       verbose = FALSE) {

  if (verbose) print(paste0("[*] ", "Initializing creation..."))

  if (verbose) print(paste0("[|] ", "setting random seed ..."))
  set.seed(seed)

  if (train_codename == "") {
    train_name <- textclean::replace_hash(iconv(unlist(stopwords::data_stopwords_stopwordsiso),
                                                to = 'ASCII//TRANSLIT', sub = ""),
                                          pattern = "\\?", replacement = "")
    train_name <- train_name[train_name != ""]
    train_name <- train_name[stringr::str_detect(train_name, "^[a-z]+$", negate = F)]
    train_name <- sort(unique(train_name[nchar(train_name) > 2]))
    train_codename <- sample(train_name, 1)
  }

  p_version <- as.character(packageVersion("mitre"))
  # root path
  if (!dir.exists(save_path))
    dir.create(save_path)

  # version subpath
  save_path <- file.path(save_path, paste0("v", p_version))
  if (!dir.exists(save_path))
    dir.create(save_path)

  if (verbose) print(paste0("[|] ", "random name for trainset: ", train_codename))
  save_path <- file.path(save_path, train_codename)
  if (verbose) print(paste0("[|] ", " > save_path: ", save_path))
  if (!dir.exists(save_path))
    dir.create(save_path)

  p_type <- paste0(ifelse(ner_config$vendor, "v", ""),
                   ifelse(ner_config$product, "p", ""),
                   ifelse(ner_config$version, "v", ""))
  if (nchar(p_type) == 1) {
    p_type <- ifelse(ner_config$vendor, "vend",
                     ifelse(ner_config$product, "prod", "vers"))
  }
  if (verbose) print(paste0("[|] ", "identified type '", p_type, "'"))

  if (verbose) print(paste0("[|] ", "loading CPE data ..."))
  cpes <- mitre::cpe.nist
  cpes$id <- 1:nrow(cpes)


  if (mix_config$sample_weight == "pca") {
    if (verbose) print(paste0("[|] ", "Sampling mix with PCA option ..."))
    p_features <- mix_config$pca_features
    if (verbose) print(paste0("[|] > ", "selecting features ..."))
    if (verbose) print(paste0("[|] > ", paste(p_features, collapse = ", ")))

    if (verbose) print(paste0("[|] > ", "computing stats ..."))
    if (verbose & mix_config$scale_log) print(paste0("[|] > ", "logarithmic scaling ..."))
    df <- nlp_cpe_feateng(df = cpes, scale_log = mix_config$scale_log)
    sam_size <- floor((num_samples*4) / length(p_features))
    sam_size_extra <- (num_samples*4) %% length(p_features)
    if (verbose) print(paste0("[|] > ", "weighted sampling ..."))
    df_sample <- dplyr::sample_n(df, size = sam_size + sam_size_extra, weight = df[[p_features[1]]])
    if (length(p_features) > 1) {
      for (i in 2:length(p_features)) {
        df_sample <- rbind(df_sample,
                           dplyr::sample_n(df, size = sam_size, weight = df[[p_features[i]]]))
      }
    }
    df <- df_sample[, c("id", "title", "part", "vendor", "product", "version")]
    rm(df_sample)
    if (verbose) print(paste0("[|] > ", "PCA sampling done!"))
  }

  if (mix_config$sample_weight == "vendor") {
    if (verbose) print(paste0("[|] ", "Sampling mix by vendor fame ..."))
    p_features <- mix_config$vendor_qntl
    if (verbose) print(paste0("[|] > distribution by quantiles: ", paste(p_features, collapse = ", ")))
    df <- nlp_cpe_sample_dataset(df = cpes, num_samples = num_samples*4,
                                 seed = seed, quantiles = mix_config$vendor_qntl)
    df <- df[, c("id", "title", "part", "vendor", "product", "version")]
    if (verbose) print(paste0("[|] > ", "Vendor sampling done!"))
  }

  if (verbose) print(paste0("[|] ", "Shuffling data ..."))
  df <- dplyr::sample_n(df, nrow(df))

  if (ner_config$notation == "rasa") {
    if (verbose) print(paste0("[|] ", "Adding RASA notation ..."))
    # Load CPEs annotated with RASA
    df_sample <- nlp_cpe_annotate(df = df, type = p_type, kind = "RASA", strict = FALSE)
    df_sample <- dplyr::sample_n(df_sample, num_samples)
    if (num_samples >= 1000) {
      name_file <- file.path(save_path, paste0("cpes_rasa_", p_type,"_", round(num_samples / 1000, 1), "k.csv"))
    } else {
      name_file <- file.path(save_path,paste0("cpes_rasa_", p_type,"_", num_samples, ".csv"))
    }
    readr::write_csv(x = df_sample[, c("title", "annotated")],
                     file = name_file, col_names = FALSE)
  }

  if (ner_config$notation == "offset") {
    if (verbose) print(paste0("[|] ", "Adding offsets as json entities (scapy) ..."))
    # Load CPEs annotated with ENTITIES
    df_sample <- mitre::nlp_cpe_annotate(df = df, type = p_type, kind = "entities", strict = FALSE)
    df_sample <- dplyr::sample_n(df_sample, num_samples)

    if (num_samples >= 1000) {
      name_file <- file.path(save_path,paste0("cpes_entities_", p_type,"_", round(num_samples / 1000, 1), "k.csv"))
    } else {
      name_file <- file.path(save_path,paste0("cpes_entities_", p_type,"_", num_samples, ".csv"))
    }
    readr::write_csv(x = df_sample[, c("title", "annotated")],
                     file = name_file, col_names = FALSE)
  }
  if (verbose) print(paste0("[.] ", "done!"))

  return(df_sample)
}
