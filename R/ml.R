ner_offsets_cpe <- function(cpes = mitre::cpe.nist, overlap = FALSE) {
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

    # Annotate Vendor
    pos_vend <- stringr::str_locate_all(df$title[i], stringr::fixed(df$vendor[i]))[[1]]
    a_vend <- NA
    if (length(pos_vend) > 0) {
      a_vend <- cbind(as.data.frame(pos_vend - 1), label = "CPE_VENDOR")
    }
    # Annotate Product
    pos_prod <- stringr::str_locate_all(df$title[i], stringr::fixed(df$product[i]))[[1]]
    a_prod <- NA
    if (length(pos_prod) > 0) {
      a_prod <- cbind(as.data.frame(pos_prod - 1), label = "CPE_PRODUCT")[nrow(pos_prod), ]
    }
    # Annotate Version
    pos_vers <- stringr::str_locate_all(df$title[i], stringr::fixed(df$version[i]))[[1]]
    a_vers <- NA
    if (length(pos_vers) > 0) {
      a_vers <- cbind(as.data.frame(pos_vers - 1), label = "CPE_VERSION")
    }

    if (all(!is.na(a_vend))) entities <- rbind(entities, a_vend)
    if (all(!is.na(a_prod))) entities <- rbind(entities, a_prod)
    if (all(!is.na(a_vers))) entities <- rbind(entities, a_vers)

    df$annotations[i] <- jsonlite::toJSON(entities)

    if ((i %% 1000) == 0) print(paste0("[.] step ", i, " ..."))
  }

  df <- df[!is.na(df$annotations), ]

  return(df)
}

df <- ner_offsets_cpe()
write.csv(df[, c("title", "annotations")], "inst/extdata/cpes_notoverlap.csv",
          row.names = FALSE, fileEncoding = "UTF-8")
