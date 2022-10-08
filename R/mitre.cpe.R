#' @import dplyr

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


#' Create a CPE for each inventory item.
#'
#' @param df_inventory data.frame with columns: vendor, name and version; title is optional
#' @param model_name huggingface reference, by default: Neurona/cpener-test
#'
#' @return data.frame
#' @export
predict_cpe <- function(df_inventory = mitre::getInventory(),
                        model_name = "Neurona/cpener-test") {
  if (!("title" %in% names(df_inventory))) {
    sw_title <- paste(cpe_wfn_vendor(df_inventory$vendor),
                      cpe_wfn_product(df_inventory$name), sep = " ")
    df_pred <- data.frame(title = sapply(sw_title,
                                         function(x)
                                           paste(unique(unlist(strsplit(x, " "))), collapse = " ")),
                          cpe = rep(NA, length(sw_title)))
    df_pred$title <- paste0(stringr::str_trim(paste(df_pred$title, df_inventory$version)),".")
  } else {
    df_pred <- df_inventory
  }

  predicted_cpes <- sapply(df_pred$title,
                           function(x) {
                             cpener2cpe23(embed2cpener(text::textNER(x = x,
                                                               model = model_name,
                                                               device = "gpu",
                                                               logging_level = "critical")))
                           }, USE.NAMES = F)
  df_inventory$ner_cpe <- names(predicted_cpes)
  df_inventory$ner_score <- as.numeric(predicted_cpes)

  return(df_inventory)
}


#####
##### CPE Cleansing

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




#####
##### NIST IMPLEMENTATIONS

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

wfn_compare <- function(source, target) {

}

#' Inspect each character in string s. Certain nonalpha
#' characters pass thru without escaping into the result,
#' but most retain escaping
#'
#' REF: NIST IR 7695 (Aug. 2011) section 6.2.2.2
#'
#' @param s character
#'
#' @return character
cpe_wfn_process_quoted_chars <- function(s = character()) {
  result <- ""
  idx <- 1
  while (idx <= nchar(s)) {
    thischar <- stringi::stri_sub(s, idx, idx)
    if (thischar != "\\") {
      result <- paste0(result, thischar)
    } else {
      nextchar <- stringi::stri_sub(s, idx + 1, idx + 1)
      if (nextchar %in% c(".", "-", "_")) {
        result <- paste0(result, nextchar)
      } else {
        result <- paste0(result, "\\", nextchar)
      }
      idx <- idx + 2
      next
    }
    idx <- idx + 1
  }

  return(result)
}

#' Inspect each character in string s. Copy quoted characters,
#' with their escaping, into the result. Look for unquoted non
#' alphanumerics and if not "*" or "?", add escaping.
#'
#' REF: NIST IR 7695 (Aug. 2011) section 6.2.3.2
#'
#' @param s character
#'
#' @return character
cpe_wfn_add_quoting <- function(s = character()) {
  result <- ""
  idx <- 1
  embedded <- FALSE
  while (idx <= nchar(s)) {
    thischar <- stringi::stri_sub(s, idx, idx)
    if ((thischar == "_") | grepl(pattern = "[[:alnum:]]", x = thischar)) {
      result <- paste0(result, thischar)
      idx <- idx + 1
      embedded <- TRUE
      next
    }
    if (thischar == "\\") {
      result <- paste0(result, stringi::stri_sub(s, idx, idx + 1))
      idx <- idx + 2
      embedded <- TRUE
      next
    }
    if (thischar == "*") {
      if ((idx == 1) | (idx == nchar(s) - 1)) {
        result <- paste0(result, thischar)
        idx <- idx + 1
        embedded <- TRUE
        next
      } else {
        cat(paste0("[!] ERROR adding quoting --> ", result))
      }
      result <- paste0(result, stringi::stri_sub(s, idx, idx + 1))
      idx <- idx + 2
      embedded <- TRUE
      next
    }
    result <- paste0(result, "\\", thischar)
    idx <- idx + 1
    embedded <- TRUE
  }

  return(result)
}

#' Transform WFN escaped name to common string
#' Ref: NIST IR 7695 (Aug. 2011) section 6.1.3.2, function decode
#'
#' This function scans the string s and returns a copy
#' with all percent-encoded characters decoded. This
#' function is the inverse of pct_encode(s) defined in
#' Section 6.1.2.3. Only legal percent-encoded forms are
#' decoded. Others raise an error.
#' Decode a blank to logical ANY, and hyphen to logical NA.

#'
#' @param name character vector
#'
#' @return character
cpe_wfn2str <- function(name = character()) {
  decode <- function(s = character()) {
    if (s == "") return("ANY")
    if (s == "-") return(NA)
    # Start the scanning loop.
    # Normalize: convert all uppercase letters to lowercase first.
    s <- tolower(s)
    result <- ""
    idx <- 1
    embedded <- FALSE
    while (idx <= nchar(s)) {
      thischar <- stringi::stri_sub(s, idx, idx)
      if (thischar %in% c(".", "-", "~")) {
        result <- paste0(result, "\\", thischar)
        idx <- idx + 1
        embedded <- TRUE #  a non-%01 encountered.
        next
      }
      if (thischar != "%") {
        result <- paste0(result, thischar)
        idx <- idx + 1
        embedded <- TRUE #  a non-%01 encountered.
        next
      }
      # we get here if we have a substring starting w/ '%'.
      char_form <- stringi::stri_sub(s, idx, idx + 2) #  get the 3-char sequence
      switch(char_form,
             "%01" = {
               if (
                 ((idx == 0) | (idx == nchar(s)-3)) |
                 (!embedded & ("%01" == stringi::stri_sub(s, idx-3, idx-1))) |
                 (embedded & (nchar(s) >= idx+6) & ("%01" == stringi::stri_sub(s, idx+3, idx+5)))
               ) {
                 # A percent-encoded question mark is found
                 # at the beginning or the end of the string,
                 # or embedded in sequence as required.
                 # Decode to unquoted form.
                 result <- paste0(result, "?")
                 idx <- idx + 3
                 next
               } else {
                 cat(paste("[!] wrong char encoded ->", char_form))
               }
             },
             "%02" = {
               if ((idx == 0) | (idx == nchar(s)-3)) {
                 # Percent-encoded asterisk is at the beginning
                 # or the end of the string, as required.
                 # Decode to unquoted form.
                 result <- paste0(result, "*")
               } else {
                 cat(paste("[!] wrong char encoded ->", char_form))
               }
             },
             "%21" = { result <- paste0(result, "\\!") },
             "%22" = { result <- paste0(result, "\\\"") },
             "%23" = { result <- paste0(result, "\\#") },
             "%24" = { result <- paste0(result, "\\$") },
             "%25" = { result <- paste0(result, "\\%") },
             "%26" = { result <- paste0(result, "\\&") },
             "%27" = { result <- paste0(result, "\\'") },
             "%28" = { result <- paste0(result, "\\(") },
             "%29" = { result <- paste0(result, "\\)") },
             "%2a" = { result <- paste0(result, "\\*") },
             "%2b" = { result <- paste0(result, "\\+") },
             "%2c" = { result <- paste0(result, "\\,") },
             "%2f" = { result <- paste0(result, "\\/") },
             "%3a" = { result <- paste0(result, "\\:") },
             "%3b" = { result <- paste0(result, "\\;") },
             "%3c" = { result <- paste0(result, "\\<") },
             "%3d" = { result <- paste0(result, "\\=") },
             "%3e" = { result <- paste0(result, "\\>") },
             "%3f" = { result <- paste0(result, "\\?") },
             "%40" = { result <- paste0(result, "\\@") },
             "%5b" = { result <- paste0(result, "\\[") },
             "%5c" = { result <- paste0(result, "\\\\") },
             "%5d" = { result <- paste0(result, "\\]") },
             "%5e" = { result <- paste0(result, "\\^") },
             "%60" = { result <- paste0(result, "\\`") },
             "%7b" = { result <- paste0(result, "\\{") },
             "%7c" = { result <- paste0(result, "\\|") },
             "%7d" = { result <- paste0(result, "\\}") },
             "%7e" = { result <- paste0(result, "\\~") },
             { cat(paste("[!] char not encoded ->", char_form))}
      )
      idx <- idx + 3
      embedded <- TRUE # a non-%01 encountered.
    }
    return(result)
  }

  if (length(name) > 1) {
    name <- sapply(name, decode)
  } else if (length(name) == 1) {
    name <- decode(name)
  } else {
    cat("[!] Wrong input length in function cpe_str2wfn.")
    name <- name
  }
  return(name)
}

#' Transform common string to WFN escaped name
#'
#' @param name character vector
#'
#' @return character
cpe_str2wfn <- function(name = c("Notepad++ v8.50", "CTS Projects & Software ClassAd 3.0")) {
  # Ref: NIST IR 7695 (Aug. 2011) section 6.1.2.3, function transform_for_uri
  transform_for_uri <- function(s = "Notepad++ v8.50") {
    s <- tolower(s)
    result <- ""
    idx <- 1
    while (idx <= nchar(s)) {
      thischar <- stringi::stri_sub(s, idx, idx)
      if (grepl(pattern = "[[:alnum:]]", x = thischar)) {
        result <- paste0(result, thischar)
        idx <- idx + 1
        next
      }
      if (grepl(pattern = "\\\\", x = thischar)) {
        idx <- idx + 1
        nxtchar <- stringi::stri_sub(s, idx, idx)
        result <- paste0(result, pct_encode(nxtchar))
        idx <- idx + 1
        next
      }
      if (thischar == " ") {
        thischar <- "_"
      }
      if (grepl(pattern = "\\?", x = thischar)) {
        thischar <- "%01"
      }
      if (grepl(pattern = "\\*", x = thischar)) {
        thischar <- "%02"
      }

      result <- paste0(result, thischar)
      idx <- idx + 1
    }

    return(result)
  }

  # Character encoding for WFN
  # Ref: NIST IR 7695 (Aug. 2011) section 6.1.2.3, function pct_encode
  pct_encode <- function(ch = character()) {
    if (ch == "!") return("%21")
    if (ch == '"') return("%22")
    if (ch == "#") return("%23")
    if (ch == "$") return("%24")
    if (ch == "%") return("%25")
    if (ch == "&") return("%26")
    if (ch == "'") return("%27")
    if (ch == "(") return("%28")
    if (ch == ")") return("%29")
    if (ch == "*") return("%2a")
    if (ch == "+") return("%2b")
    if (ch == ",") return("%2c")
    if (ch == "-") return(ch)
    if (ch == ".") return(ch)
    if (ch == "/") return("%2f")
    if (ch == ":") return("%3a")
    if (ch == ";") return("%3b")
    if (ch == "<") return("%3c")
    if (ch == "=") return("%3d")
    if (ch == ">") return("%3e")
    if (ch == "?") return("%3f")
    if (ch == "@") return("%40")
    if (ch == "[") return("%5b")
    if (ch == "\\") return("%5c")
    if (ch == "]") return("%5d")
    if (ch == "^") return("%5e")
    if (ch == "`") return("%60")
    if (ch == "{") return("%7b")
    if (ch == "|") return("%7c")
    if (ch == "}") return("%7d")
    if (ch == "~") return("%7e")

    return(ch)
  }

  if (length(name) > 1) {
    name <- sapply(name, transform_for_uri)
  } else if (length(name) == 1) {
    name <- transform_for_uri(name)
  } else {
    cat("[!] Wrong input length in function cpe_str2wfn.")
    name <- name
  }

  return(name)
}





