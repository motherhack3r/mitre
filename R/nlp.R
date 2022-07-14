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
#'
#' @return data.frame
#' @export
nlp_cpe_annotate <- function(df = nlp_cpe_dataset(),
                             type = c("vpv", "vp", "pv", "vv", "vend", "prod", "version")[1],
                             kind = c("RASA", "entities", "BILUO")[1]) {
  df_ner <- df[, c("id", "title", "vendor", "product", "version")]

  # Add tags
  df_ner$annotated <- df_ner$title

  # Remove rows without version for types: vpv, pv, vv and version
  if (type %in% c("vpv", "pv", "vv", "version")) {
    df_ner <- df_ner[stringr::str_detect(string = df$version, pattern = "^\\-$", negate = T), ]
  }


  # Check overlapping vendor-product
  if (type %in% c("vpv", "vp")) {
    df_ner <- df_ner[!stringr::str_detect(df_ner$vendor, stringr::fixed(df_ner$product)), ]
    df_ner <- df_ner[!stringr::str_detect(df_ner$product, stringr::fixed(df_ner$vendor)), ]
  }

  if (type == "vpv") {
    if (kind == "RASA") {
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
      # Keep only titles with all entities
      df_ner <- df_ner %>% filter(train_v & train_p & train_r) %>% select(-train_v, -train_p, -train_r)

      ## vendor + product + version
      df_ner$annotated <- stringr::str_replace_all(string = df_ner$annotated,
                                          pattern = paste0("(.*)(", df_ner$vendor,")(\\s.*)(", df_ner$product,")(.*)(", df_ner$version,")(.*)"),
                                          replacement = "\\1\\\\[\\2\\]\\(cpe_vendor\\)\\3\\[\\4\\]\\(cpe_product\\)\\5\\[\\6\\]\\(cpe_version\\)\\7\\")

      df_ner <- df_ner[which(grepl(pattern = ".*\\]\\(cpe_vendor\\).*\\]\\(cpe_product\\).*\\]\\(cpe_version\\).*", df_ner$annotated)), ]
    }
  }
  # if (type == "vend") {
  #   df_vendor <- df[, c("id", "title", "vendor")]
  #   df_vendor$title <- tolower(df_vendor$title)
  #   df_vendor$vendor <- stringr::str_replace_all(df_vendor$vendor, "\\\\", "")
  #   df_vendor$vendor <- stringr::str_replace_all(df_vendor$vendor, "_",  " ")
  #   tit2vend <- apply(df_vendor, 1,
  #                     function(x)
  #                       as.data.frame.list(stringdist::afind(x[2], x[3])))
  #   tit2vend <- dplyr::bind_rows(tit2vend)
  #   df_vendor <- dplyr::bind_cols(df_vendor, tit2vend)
  #
  #   # vendor entities candidates
  #   df_vendor$train_v <- rep(F, nrow(df_vendor))
  #   df_vendor$train_v <- stringr::str_detect(df_vendor$title, stringr::fixed(df_vendor$vendor))
  #
  #   df_vendor <- df_vendor[df_vendor$distance < 2, ]
  #   df_vendor$annotation <- paste0(substr(df_vendor$title, 1, df_vendor$location - 1),
  #                                  "[", df_vendor$match, "](cpe_vendor)",
  #                                  substr(df_vendor$title,
  #                                         df_vendor$location + nchar(df_vendor$match),
  #                                         nchar(df_vendor$title)))
  #   df_vendor <- df_vendor[, c("id", "title", "annotation")]
  #   df_vendor$title <- iconv(df_vendor$title, to = 'ASCII//TRANSLIT')
  #   df_vendor$annotation <- iconv(df_vendor$annotation, to = 'ASCII//TRANSLIT')
  # }



  return(df_ner)
}
