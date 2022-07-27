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
  encname <- iconv(name, to = 'ASCII//TRANSLIT')

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
  encname <- iconv(name, to = 'ASCII//TRANSLIT')

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

#' Transform WFN escaped name to common string
#'
#' @param name character vector
#'
#' @return character
#' @export
wfn2str <- function(name = character()) {
  return(name)
}


#' Remove rows containing non valid characters for CPE LSTM tokenization
#'
#' @param df data.frame default mitre cpes
#'
#' @return data.frame
#' @export
nlp_cpe_dataset <- function(df = mitre::cpe.nist) {
  df$id <- 1:nrow(df)
  df <- df[!df$deprecated, c("id", "title", "vendor", "product", "version")]

  df$valid <- stringr::str_detect(str73enc(df$title), "\\*", negate = T)
  df <- df[df$valid, ]
  df$valid <- stringr::str_detect(str49enc(df$vendor), "\\*", negate = T)
  df <- df[df$valid, ]
  df$valid <- stringr::str_detect(str49enc(df$product), "\\*", negate = T)
  df <- df[df$valid, ]
  df$valid <- stringr::str_detect(str49enc(df$version), "\\*", negate = T)
  df <- df[df$valid, c("id", "title", "vendor", "product", "version")]

  return(df)
}


#' Used by generic annotate cpes function
#'
#' @param df_ner data.frame
#' @param type character
#'
#' @return
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

  # Remove rows without version for types: vpv, pv, vv and version
  if (type %in% c("vpv", "pv", "vv", "version")) {
    print(paste0("[+] ", "removing empty versions..."))
    df_ner <- df_ner[stringr::str_detect(string = df_ner$version, pattern = "^\\-$", negate = T), ]
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
#' @param only_vendor logical
#' @param as_WFN logical
#'
#' @return data.frame
#' @export
getCPEstats <- function(only_vendor = TRUE, as_WFN = TRUE) {
  if (as_WFN) {
    df <- nlp_cpe_dataset()
  } else {
    df <- cpe.nist
  }
  if (only_vendor) {
    sts_cpes <- dplyr::count(df, vendor, sort = TRUE)
  } else {
    sts_cpes <- dplyr::count(df, vendor, product, sort = TRUE)
  }
  sts_cpes$log_n <- log(sts_cpes$n)

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

  sts_vend <- getCPEstats(only_vendor = TRUE, as_WFN = TRUE)
  sts_qntl <- quantile(sts_vend$log_n, probs = quantiles)

  df02 <- sts_vend[sts_vend$log_n <= sts_qntl[2], ]
  df02 <- dplyr::inner_join(df, df02, by = c("vendor" = "vendor"))
  df25 <- sts_vend[((sts_qntl[2] < sts_vend$log_n) & (sts_vend$log_n <= sts_qntl[3])), ]
  df25 <- dplyr::inner_join(df, df25, by = c("vendor" = "vendor"))
  df57 <- sts_vend[((sts_qntl[3] < sts_vend$log_n) & (sts_vend$log_n <= sts_qntl[4])), ]
  df57 <- dplyr::inner_join(df, df57, by = c("vendor" = "vendor"))
  df70 <- sts_vend[sts_vend$log_n > sts_qntl[4], ]
  df70 <- dplyr::inner_join(df, df70, by = c("vendor" = "vendor"))

  # Sampling using quantiles to select slices
  nbias <- num_samples %% 4

  df_sam <- dplyr::sample_n(df02, num_samples/4)
  df_sam <- dplyr::bind_rows(df_sam, dplyr::sample_n(df25, (num_samples/4) + nbias))
  df_sam <- dplyr::bind_rows(df_sam, dplyr::sample_n(df57, (num_samples/4)))
  df_sam <- dplyr::bind_rows(df_sam, dplyr::sample_n(df70, (num_samples/4)))

  df_sam <- dplyr::sample_n(df_sam, nrow(df_sam))
  df_sam <- df_sam[, c("id", "title", "vendor", "product", "version")]

  return(df_sam)
}

#' Title
#'
#' @param num_samples integer
#' @param type character, default vpv
#' @param kind character, default RASA
#' @param pydict logical, python offsets begins with 0, otherwise 1
#' @param rdataset character, path to RDS. nlp_cpe_annotate default if not exists
#' @param seed integer, used for reproducible research
#'
#' @return data.frame
#' @export
nlp.ner_cpe_trainset <- function(num_samples = 5000,
                                 type = c("vpv", "vp", "pv", "vv", "vend", "prod", "vers")[1],
                                 kind = c("RASA", "entities")[1],
                                 pydict = TRUE,
                                 rdataset = "data-raw/NLP/cpes_vpv_rasa.rds",
                                 seed = 42) {

  set.seed(seed)

  if (file.exists(rdataset)) {
    df <- readRDS(rdataset)
  } else {
    df <- nlp_cpe_annotate(type = type, kind = kind, pydict = pydict, seed = seed)
  }

  # TODO: Review notebook for normalize this kind of distribution
  sts_vend <- getCPEstats(only_vendor = TRUE, as_WFN = TRUE)
  sts_qntl <- quantile(sts_vend$log_n, probs = c(0, 0.8, 0.9, 0.99, 1))

  df02 <- sts_vend[sts_vend$log_n <= sts_qntl[2], ]
  df02 <- dplyr::inner_join(df, df02, by = c("vendor" = "vendor"))
  df25 <- sts_vend[((sts_qntl[2] < sts_vend$log_n) & (sts_vend$log_n <= sts_qntl[3])), ]
  df25 <- dplyr::inner_join(df, df25, by = c("vendor" = "vendor"))
  df57 <- sts_vend[((sts_qntl[3] < sts_vend$log_n) & (sts_vend$log_n <= sts_qntl[4])), ]
  df57 <- dplyr::inner_join(df, df57, by = c("vendor" = "vendor"))
  df70 <- sts_vend[sts_vend$log_n > sts_qntl[4], ]
  df70 <- dplyr::inner_join(df, df70, by = c("vendor" = "vendor"))

  # Sampling using quantiles to select slices
  nbias <- num_samples %% 4

  df_sam <- dplyr::sample_n(df02, num_samples/4)
  df_sam <- dplyr::bind_rows(df_sam, dplyr::sample_n(df25, (num_samples/4) + nbias))
  df_sam <- dplyr::bind_rows(df_sam, dplyr::sample_n(df57, (num_samples/4)))
  df_sam <- dplyr::bind_rows(df_sam, dplyr::sample_n(df70, (num_samples/4)))

  df_sam <- dplyr::sample_n(df_sam, nrow(df_sam))
  df_sam <- df_sam[, c("id", "title", "vendor", "product", "version", "annotated")]

  return(df_sam)
}

#' Title
#'
#' @param df
#'
#' @return
#' @export
nlp_cpe_feateng <- function(df = mitre::cpe.nist) {

  df <- df[, c("title", "part", "vendor", "product", "version")]
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

  log_cpes <- dplyr::select(df, -c("title", "part", "vendor", "product", "version"))
  log_cpes <- as.data.frame.matrix(apply(log_cpes, 2, function(x) log(x+1)))
  cpe_prcomp <- DataExplorer::plot_prcomp(log_cpes)
  cpe_prcomp <- cpe_prcomp[["page_1"]][["data"]]

  # My selected features from PCA for sampling and create better train sets
  # len_version, len_title, num_product, len_vendor, sym_vendor

  return(df)
}
