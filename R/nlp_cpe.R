cpe_for_ner <- function(df = cpe.nist) {

  return(df)
}

#' Title
#'
#' @param df data.frame
#'
#' @return data.frame
#' @export
cpe_add_features <- function(df = cpe.nist) {
# cpe_add_features <- function(df = cpe.nist, scale_log = FALSE) {
    if (!("id" %in% names(df))) {
    df$id <- 1:nrow(df)
    # df$uuid <- uuid::UUIDgenerate(n = nrow(df))
  }
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

  # if (scale_log) {
  #   log_cpes <- dplyr::select(df, -c("title", "part", "vendor", "product", "version"))
  #   log_cpes <- as.data.frame.matrix(apply(log_cpes, 2, function(x) log(x + 1)))
  #
  #   df <- cbind(dplyr::select(df, c("title", "part", "vendor", "product", "version")), log_cpes)
  # }

  df$train_vendor <- F | ((df$sym_vendor < 0.05) & ((df$abc_vendor + df$dot_vendor) > 0.5))
  df$train_product <- F | ((df$sym_product < 0.2) & ((df$abc_product + df$dot_product) > 0.8))
  df$train_version <- F | ((df$abc_version < 0.3) & ((df$num_version + df$dot_version) > 0.5))

  return(df)
}
