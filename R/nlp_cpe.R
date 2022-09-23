# ONLY NEEDED FOR VULNDIGGER

#' Load CPE data frame from local file or download latest
#'
#' @param local_path path to RDS file. NA value implies remote TRUE
#' @param remote logical
#' @param keep_deprecated logical
#' @param allcols logical
#' @param verbose logical
#'
#' @return data.frame
#' @export
#'
#' @examples
#' cpes <- mitre::cpe_latest_date(local_path = "inst/extdata/cpe.nist.rds")
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
  sts_cpes$popular <- datawizard::categorize(sts_cpes$n, split = "quantile", n_groups = 11)

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
    valid_chars <- DescTools::DecToHex(valid_chars)
  }

  return(valid_chars)
}

#' Title
#'
#' @param df data.frame
#' @param verbose
#'
#' @return data.frame
#' @export
cpe_lstm_dataset <- function(df = cpe_latest_data(), verbose = F) {

  if (verbose) print(paste0("[|] ", "Remove rows with escaped chars not compatibles"))
  # remove rows with escaped chars because of tagging regex
  df <- df[!grepl(pattern = "\\\\", df$vendor), ]
  df <- df[!grepl(pattern = "\\\\", df$product), ]
  df <- df[!grepl(pattern = "\\\\", df$version), ]
  if (verbose) print(paste0("[|] ", "replace WFN _ to space in vendor and product"))
  # replace WFN _ to space in vendor and product
  df$vendor <- stringr::str_replace_all(df$vendor, "_", " ")
  df$product <- stringr::str_replace_all(df$product, "_", " ")
  if (verbose) print(paste0("[|] ", "remove titles with equal vendor and product"))
  # remove titles with equal vendor and product
  df <- df[which(df$vendor != df$product), ]
  if (verbose) print(paste0("[|] ", "Detecting vendor candidates in titles"))
  # vendor candidates
  df$train_v <- rep(F, nrow(df))
  df$train_v <- stringr::str_detect(df$title, stringr::fixed(df$vendor, ignore_case = TRUE))
  if (verbose) print(paste0("[|] ", "Detecting product candidates in titles"))
  # product candidates
  df$train_p <- rep(F, nrow(df))
  df$train_p <- stringr::str_detect(df$title, stringr::fixed(df$product, ignore_case = TRUE))
  if (verbose) print(paste0("[|] ", "Detecting version candidates in titles"))
  # version candidates
  df$train_r <- rep(F, nrow(df))
  df$train_r <- stringr::str_detect(df$title, stringr::fixed(df$version, ignore_case = TRUE))

  if (verbose) print(paste0("[|> VERSION: ", "Keep only titles with version entities"))
  # Keep only titles with version entity
  df <- dplyr::filter(df, train_r)
  df <- dplyr::select(df, -train_v, -train_p, -train_r)

  if (verbose) print(paste0("[|] ", "Normalize to WFN strings"))
  df <- cpe_wfn_dataset(df)

  if (verbose) print(paste0("[.] Done."))
  return(df)
}

#' Title
#'
#' @param df data.frame
#'
#' @return data.frame
#' @export
cpe_wfn_dataset <- function(df = cpe_latest_data()) {
  df$valid <- stringr::str_detect(str73enc(df$title), "\\*", negate = T)
  df <- df[df$valid, ]
  df$valid <- stringr::str_detect(str49enc(df$vendor), "\\*", negate = T)
  df <- df[df$valid, ]
  df$valid <- stringr::str_detect(str49enc(df$product), "\\*", negate = T)
  df <- df[df$valid, ]
  df$valid <- stringr::str_detect(str49enc(df$version), "\\*", negate = T)
  df <- df[df$valid, ]

  # remove titles, vendor, product or versions with tabs
  df <- df[!(grepl("\\t", df$title)), ]

  # df <- df[, c("id", "title", "cpe.23", "part", "vendor", "product", "version")]

  return(df)
}


#' Used by generic annotate cpes function
#'
#' @param df data.frame
#' @param type character
#' @param verbose logical
#'
#' @return data.frame
#' @export
cpe_add_notation <- function(df = cpe_latest_data(),
                             type = c("vpv", "vp", "pv", "vv", "vend", "prod", "vers")[1],
                             verbose = FALSE) {
  if (verbose) print(paste0("[*] ", "RASA notation..."))
  df_ner <- df
  # Method: Use specific regex for each case and replace matches with RASA.
  if (verbose) print(paste0("[|] ", "Remove rows with escaped chars not compatibles"))
  # remove rows with escaped chars because of tagging regex
  df_ner <- df_ner[!grepl(pattern = "\\\\", df_ner$vendor), ]
  df_ner <- df_ner[!grepl(pattern = "\\\\", df_ner$product), ]
  df_ner <- df_ner[!grepl(pattern = "\\\\", df_ner$version), ]
  if (verbose) print(paste0("[|] ", "replace WFN _ to space in vendor and product"))
  # replace WFN _ to space in vendor and product
  df_ner$vendor <- stringr::str_replace_all(df_ner$vendor, "_", " ")
  df_ner$product <- stringr::str_replace_all(df_ner$product, "_", " ")
  if (verbose) print(paste0("[|] ", "remove titles with equal vendor and product"))
  # remove titles with equal vendor and product
  df_ner <- df_ner[which(df_ner$vendor != df_ner$product), ]
  if (verbose) print(paste0("[|] ", "Detecting vendor candidates in titles"))
  # vendor candidates
  df_ner$train_v <- rep(F, nrow(df_ner))
  df_ner$train_v <- stringr::str_detect(df_ner$title, stringr::fixed(df_ner$vendor, ignore_case = TRUE))
  if (verbose) print(paste0("[|] ", "Detecting product candidates in titles"))
  # product candidates
  df_ner$train_p <- rep(F, nrow(df_ner))
  df_ner$train_p <- stringr::str_detect(df_ner$title, stringr::fixed(df_ner$product, ignore_case = TRUE))
  if (verbose) print(paste0("[|] ", "Detecting version candidates in titles"))
  # version candidates
  df_ner$train_r <- rep(F, nrow(df_ner))
  df_ner$train_r <- stringr::str_detect(df_ner$title, stringr::fixed(df_ner$version, ignore_case = TRUE))

  if (type == "vpv") {
    if (verbose) print(paste0(" |> VPV: ", "Keep only titles with all entities"))
    # Keep only titles with all entities
    df_ner <- dplyr::filter(df_ner, train_v & train_p & train_r)
    df_ner <- dplyr::select(df_ner, -train_v, -train_p, -train_r)
    if (verbose) print(paste0(" |> VPV: ", "RASA notation for vendor + product + version"))
    ## vendor + product + version
    df_ner$annotated <- stringr::str_replace_all(string = df_ner$title,
                                                 pattern = paste0("(?i)(.*)(", df_ner$vendor,")(\\s.*)(", df_ner$product,")(.*)(", df_ner$version,")(.*)"),
                                                 replacement = "\\1\\[\\2\\]\\(cpe_vendor\\)\\3\\[\\4\\]\\(cpe_product\\)\\5\\[\\6\\]\\(cpe_version\\)\\7")
    df_ner <- df_ner[which(grepl(pattern = ".*\\(cpe_vendor\\).*\\(cpe_product\\).*\\(cpe_version\\).*", df_ner$annotated)), ]
  } else if (type == "vp") {
    if (verbose) print(paste0(" |> VP: ", "Keep only titles with vendor and product entities"))
    # Keep only titles with vendor and product entities
    df_ner <- dplyr::filter(df_ner, train_v & train_p)
    df_ner <- dplyr::select(df_ner, -train_v, -train_p, -train_r)
    if (verbose) print(paste0(" |> VP: ", "RASA notation for vendor + product"))
    ## vendor + product
    df_ner$annotated <- stringr::str_replace_all(string = df_ner$title,
                                                 pattern = paste0("(?i)(.*)(", df_ner$vendor,")(\\s.*)(", df_ner$product,")(.*)"),
                                                 replacement = "\\1\\[\\2\\]\\(cpe_vendor\\)\\3\\[\\4\\]\\(cpe_product\\)\\5")
    df_ner <- df_ner[which(grepl(pattern = ".*\\(cpe_vendor\\).*\\(cpe_product\\).*", df_ner$annotated)), ]
  } else if (type == "pv") {
    if (verbose) print(paste0(" |> PV: ", "Keep only titles with product and version entities"))
    # Keep only titles with product and version entities
    df_ner <- dplyr::filter(df_ner, train_p & train_r)
    df_ner <- dplyr::select(df_ner, -train_v, -train_p, -train_r)
    if (verbose) print(paste0(" |> PV: ", "RASA notation for product + version"))
    ## product + version
    df_ner$annotated <- stringr::str_replace_all(string = df_ner$title,
                                                 pattern = paste0("(?i)(.*)(", df_ner$product,")(\\s.*)(", df_ner$version,")(.*)"),
                                                 replacement = "\\1\\[\\2\\]\\(cpe_product\\)\\3\\[\\4\\]\\(cpe_version\\)\\5")
    df_ner <- df_ner[which(grepl(pattern = ".*\\(cpe_product\\).*\\(cpe_version\\).*", df_ner$annotated)), ]
  } else if (type == "vv") {
    if (verbose) print(paste0(" |> VV: ", "Keep only titles with vendor and version entities"))
    # Keep only titles with vendor and version entities
    df_ner <- dplyr::filter(df_ner, train_v & train_r)
    df_ner <- dplyr::select(df_ner, -train_v, -train_p, -train_r)
    if (verbose) print(paste0(" |> VV: ", "RASA notation for vendor + version"))
    ## vendor + version
    df_ner$annotated <- stringr::str_replace_all(string = df_ner$title,
                                                 pattern = paste0("(?i)(.*)(", df_ner$vendor,")(\\s.*)(", df_ner$version,")(.*)"),
                                                 replacement = "\\1\\[\\2\\]\\(cpe_vendor\\)\\3\\[\\4\\]\\(cpe_version\\)\\5")
    df_ner <- df_ner[which(grepl(pattern = ".*\\(cpe_vendor\\).*\\(cpe_version\\).*", df_ner$annotated)), ]
  } else if (type == "vend") {
    if (verbose) print(paste0(" |> VENDOR: ", "Keep only titles with vendor entities"))
    if (verbose) print(paste0(" |> VENDOR: ", "RASA notation for vendor"))
    # Keep only titles with vendor entity
    df_ner <- dplyr::filter(df_ner, train_v)
    df_ner <- dplyr::select(df_ner, -train_v, -train_p, -train_r)
    ## vendor
    df_ner$annotated <- stringr::str_replace_all(string = df_ner$title,
                                                 pattern = paste0("(?i)(.*)(", df_ner$vendor,")(\\s.*)(.*)"),
                                                 replacement = "\\1\\[\\2\\]\\(cpe_vendor\\)\\3")
    df_ner <- df_ner[which(grepl(pattern = ".*\\(cpe_vendor\\).*", df_ner$annotated)), ]
  } else if (type == "prod") {
    if (verbose) print(paste0(" |> PRODUCT: ", "Keep only titles with product entities"))
    # Keep only titles with product entity
    df_ner <- dplyr::filter(df_ner, train_p)
    df_ner <- dplyr::select(df_ner, -train_v, -train_p, -train_r)
    if (verbose) print(paste0(" |> PRODUCT: ", "RASA notation for product"))
    ## product
    df_ner$annotated <- stringr::str_replace_all(string = df_ner$title,
                                                 pattern = paste0("(?i)(.*)(", df_ner$product,")(\\s.*)(.*)"),
                                                 replacement = "\\1\\[\\2\\]\\(cpe_product\\)\\3")
    df_ner <- df_ner[which(grepl(pattern = ".*\\(cpe_product\\).*", df_ner$annotated)), ]
  } else if (type == "vers") {
    if (verbose) print(paste0(" |> VERSION: ", "Keep only titles with version entities"))
    # Keep only titles with version entity
    df_ner <- dplyr::filter(df_ner, train_r)
    df_ner <- dplyr::select(df_ner, -train_v, -train_p, -train_r)
    if (verbose) print(paste0(" |> VERSION: ", "RASA notation for version"))
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
  df <- dplyr::select(df, c("id", "title", "cpe.23", "part", "vendor", "product", "version"))

  df$len_title <- as.numeric(stringr::str_length(df$title))
  df$len_vendor <- as.numeric(stringr::str_length(df$vendor))
  df$len_product <- as.numeric(stringr::str_length(df$product))
  df$len_version <- as.numeric(stringr::str_length(df$version))

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


#' Title
#'
#' @param df data.frame
#' @param verbose logical
#' @param acum_prop numeric
#'
#' @return data.frame
#' @export
cpe_pca_stats <- function(df = cpe_add_features(), verbose = FALSE, acum_prop = 0.75) {

  # df_pca <- dplyr::select_if(df, is.numeric)
  # df_pca <- df_pca[, stringr::str_detect(names(df_pca), "(len_title|version)")]
  #
  # pca <- prcomp(df_pca, scale = T)
  # pca_sum <- summary(pca)
  # pc_selected  <- sum(as.data.frame(pca_sum$importance)[3,] < acum_prop) + 1
  # pca_sum
  #
  # if (verbose) print(paste0("[|] ", "Sampling mix with PCA option ..."))
  # p_features <- mix_config$pca_features
  # if (verbose) print(paste0("[|] > ", "selecting features ..."))
  # if (verbose) print(paste0("[|] > ", paste(p_features, collapse = ", ")))
  #
  # if (verbose) print(paste0("[|] > ", "computing stats ..."))
  # if (verbose & mix_config$scale_log) print(paste0("[|] > ", "logarithmic scaling ..."))
  # df <- nlp_cpe_feateng(df = cpes, scale_log = mix_config$scale_log)
  # sam_size <- floor((num_samples*4) / length(p_features))
  # sam_size_extra <- (num_samples*4) %% length(p_features)
  # if (verbose) print(paste0("[|] > ", "weighted sampling ..."))
  # df_sample <- dplyr::sample_n(df, size = sam_size + sam_size_extra, weight = df[[p_features[1]]])
  # if (length(p_features) > 1) {
  #   for (i in 2:length(p_features)) {
  #     df_sample <- rbind(df_sample,
  #                        dplyr::sample_n(df, size = sam_size, weight = df[[p_features[i]]]))
  #   }
  # }
  # df <- df_sample[, c("id", "title", "part", "vendor", "product", "version")]
  # rm(df_sample)
  # if (verbose) print(paste0("[|] > ", "PCA sampling done!"))

  return(df)
}


#' Title
#'
#' @param df data.frame
#' @param verbose logical
#'
#' @return data.frame
#' @export
cpe_local_inventory <- function(df = getInventory(), verbose = FALSE) {

  df_inv <- df
  df_inv$wfn_vendor <- cpe_wfn_vendor(df_inv$vendor)
  df_inv$wfn_product <- cpe_wfn_product(df_inv$name)
  df_inv$wfn_version <- stringr::str_replace_all(df_inv$name, "(.*)(\\s|,|-)+v*([\\d|\\.]+)(.*)$", "\\3")

  # Remove vendor if exists in product and not versions
  df_inv$wfn_product <- stringr::str_replace_all(string = df_inv$wfn_product, pattern = paste0("^(", df_inv$wfn_vendor,")( .+$|$)"), "\\2")
  df_inv$wfn_version <- stringr::str_replace_all(df_inv$name, "(.*)(\\s|,|-)+v*([\\d|\\.]+)(.*)$", "\\3")
  df_inv$wfn_version[!grepl("\\d+(\\.\\d+)+", df_inv$name)] <- df_inv$version[!grepl("\\d+(\\.\\d+)+", df_inv$name)]

  df$title = paste(df_inv$wfn_vendor, df_inv$wfn_product, df_inv$wfn_version)
  df$title <- stringr::str_replace_all(df$title, "\\s+", " ")
  df$title <- stringr::str_trim(df$title)

  df <- dplyr::rename(df, product = name)
  df <- df[, c("title", "vendor", "product", "version")]

  return(df)
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
    if (verbose) print(paste0("[!] ", "Adding missing 'id' column."))
    df_sccm$id <- 1:nrow(df_sccm)
  }
  df <- df_sccm
  if (verbose) print(paste0(" |> ", "Input rows: ", nrow(df_sccm)))

  # Clean vendor strings
  if (verbose) print(paste0("[|] ", "Clean vendor strings"))
  df_sccm$wfn_vendor <- iconv(df_sccm$vendor, from = "UTF-8", to = 'ASCII//TRANSLIT')
  df_sccm$wfn_vendor <- stringr::str_replace_all(df_sccm$wfn_vendor, "\\?", "")
  df_sccm$wfn_vendor <- stringr::str_trim(df_sccm$wfn_vendor)
  df_sccm$bad_vendor <- (stringr::str_count(df_sccm$wfn_vendor, "[^a-zA-Z0-9 \\.]")
                         / sapply(df_sccm$wfn_vendor, nchar)
                         ) > 0.2
  df_vendor <- df_sccm[!df_sccm$bad_vendor, c("id", "wfn_vendor")]
  df_vendor <- dplyr::rename(df_vendor, vendor = wfn_vendor)
  df_vendor$wfn_vendor <- mitre::cpe_wfn_vendor(df_vendor$vendor)
  df_vendor <- df_vendor[stringr::str_length(df_vendor$wfn_vendor) > 1, c("id", "wfn_vendor")]
  df_vendor <- dplyr::rename(df_vendor, vendor = wfn_vendor)
  if (verbose) print(paste0(" |> ", "Good vendor rows: ", nrow(df_vendor)))

  # Clean product strings
  if (verbose) print(paste0("[|] ", "Clean product strings"))
  df_sccm$wfn_product <- iconv(df_sccm$product, from = "UTF-8", to = 'ASCII//TRANSLIT')
  df_sccm$wfn_product <- stringr::str_replace_all(df_sccm$wfn_product, "\\?", "")
  df_sccm$wfn_product <- stringr::str_trim(df_sccm$wfn_product)
  df_sccm$bad_product <- (stringr::str_count(df_sccm$wfn_product, "[^a-zA-Z0-9 \\.]")
                         / sapply(df_sccm$wfn_product, nchar)
                         ) > 0.2
  df_product <- df_sccm[!df_sccm$bad_product, c("id", "wfn_product")]
  df_product <- dplyr::rename(df_product, product = wfn_product)
  df_product$wfn_product <- mitre::cpe_wfn_product(df_product$product)
  df_product <- df_product[stringr::str_length(df_product$wfn_product) > 1, c("id", "wfn_product")]
  df_product <- dplyr::rename(df_product, product = wfn_product)
  if (verbose) print(paste0(" |> ", "Good product rows: ", nrow(df_product)))

  # Clean version strings
  if (verbose) print(paste0("[|] ", "Clean version strings"))
  df_sccm$wfn_version <- df_sccm$version
  df_sccm$wfn_version[(grepl("\\d+(, \\d+)+", df_sccm$wfn_version))] <-
    gsub("\\, ", "\\.", df_sccm$wfn_version[(grepl("\\d+(, \\d+)+", df_sccm$wfn_version))])
  df_sccm$wfn_version[(grepl("\\d+(,\\d+)+", df_sccm$wfn_version))] <-
    gsub("\\,", "\\.", df_sccm$wfn_version[(grepl("\\d+(,\\d+)+", df_sccm$wfn_version))])
  df_sccm$wfn_version[(grepl("\\d+(\\. \\d+)+", df_sccm$wfn_version))] <-
    gsub("\\. ", "\\.", df_sccm$wfn_version[(grepl("\\d+(\\. \\d+)+", df_sccm$wfn_version))])

  df_sccm$wfn_version <- stringr::str_extract(df_sccm$wfn_version, "\\d+(\\.\\d+)*")
  # df_sccm$wfn_version <- stringr::str_replace_all(df_sccm$wfn_version, "(.*)(\\s|,|-)+[vers\\.]*([\\d|\\.]+)(.*)$", "\\3")
  df_version <- df_sccm[, c("id", "wfn_version")]
  df_version <- dplyr::rename(df_version, version = wfn_version)
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
  if (verbose) print(paste0(" |> ", "Good rows: ", nrow(df_inv)))

  if (verbose) print(paste0("[|] ", "Removing duplicated vendor in title"))
  df_inv <- dplyr::mutate(df_inv,
                          title = ifelse(stringr::str_starts(product, stringr::fixed(paste0(vendor, " "))),
                                         paste(product, version),
                                         paste(vendor, product, version)))

  df_inv$title <- stringr::str_replace_all(df_inv$title, "\\s+", " ")
  df_inv$title <- stringr::str_replace_all(df_inv$title, "\\b(\\w+\\s)\\1\\b(.*)", "\\1\\2")
  df_inv$title <- stringr::str_trim(df_inv$title)

  df <- dplyr::left_join(df, dplyr::select(df_inv, "id", "title"), by = "id")
  df <- df[, c("id", "title", "vendor", "product", "version")]

  # Final cleansing
  if (verbose) print(paste0("[|] ", "Final cleansing..."))
  df <- df[!is.na(df$title), ]
  df$title <- iconv(df$title, to = 'ASCII//TRANSLIT')
  df$version <- iconv(df$version, to = 'ASCII//TRANSLIT')
  df <- df[!is.na(df$title), ]
  df$valid <- stringr::str_detect(str73enc(df$title), "\\*", negate = T)
  df <- df[df$valid, c("id", "title", "vendor", "product", "version")]
  if (verbose) print(paste0(" |> ", "Good rows: ", nrow(df)))

  return(df)
}

#' Title
#'
#' @param x character
#'
#' @return character
#' @export
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
#' @export
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

#' Inspect each character in string s. Certain nonalpha
#' characters pass thru without escaping into the result,
#' but most retain escaping
#'
#' REF: NIST IR 7695 (Aug. 2011) section 6.2.2.2
#'
#' @param s character
#'
#' @return character
#' @export
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
#' @export
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
#' @export
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
#' @export
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


