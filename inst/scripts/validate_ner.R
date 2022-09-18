library(dplyr)

train_ner <- mitre::cpe_add_notation(verbose = T)
df_anno <- train_ner[!is.na(train_ner$annotated),]
readr::write_csv(x = df_anno, file = "../../data/annotation/20220917_cpe_rasa.csv", col_names = TRUE)

# Run python notebook

cpes <- mitre::cpe_latest_data(remote = T)
df <- readr::read_csv("inst/extdata/predictions.csv")

df <- dplyr::left_join(cpes[cpes$id %in% df$id, ],
                       df[, c("id", "ner_vendor", "scr_vendor", "ner_product",
                              "scr_product", "ner_version", "scr_version")],
                       by = "id", keep = FALSE)
df$annotated <- NULL
df$ner_vendor <- tolower(df$ner_vendor)
df$ner_product <- tolower(df$ner_product)
df$ner_version <- tolower(df$ner_version)
df$ner_vendor <- stringr::str_replace_all(df$ner_vendor, " ", "_")
df$ner_product <- stringr::str_replace_all(df$ner_product, " ", "_")
df$ner_version <- stringr::str_replace_all(df$ner_version, " ", ".")
df$ner_vendor <- stringr::str_replace_all(df$ner_vendor, "_-_", "-")
df$ner_product <- stringr::str_replace_all(df$ner_product, "_-_", "-")
df$ner_version <- stringr::str_replace_all(df$ner_version, "\\.+", ".")
df$ner_version <- stringr::str_replace_all(df$ner_version, "\\.-\\.", "-")
df$match_vendor <- df$vendor == df$ner_vendor
df$match_product <- df$product == df$ner_product
df$match_version <- df$version == df$ner_version
df$matched <- df$match_vendor & df$match_product & df$match_version

df <- df %>%
  bind_cols(
    bind_rows(apply(df, 1,
                    function(x) as.data.frame(stringdist::afind(x["vendor"], x["ner_vendor"]))))) %>%
  mutate(vendor_acc = 1 - (distance/nchar(ner_vendor))) %>%
  select(-distance, -location, -match) %>%
  bind_cols(
    bind_rows(apply(df, 1,
                    function(x) as.data.frame(stringdist::afind(x["ner_vendor"], x["vendor"]))))) %>%
  mutate(vendor_acc = pmin(vendor_acc, 1 - (distance/nchar(vendor)))) %>%
  select(-distance, -location, -match)

df <- df %>%
  bind_cols(
    bind_rows(apply(df, 1,
                    function(x) as.data.frame(stringdist::afind(x["product"], x["ner_product"]))))) %>%
  mutate(product_acc = 1 - (distance/nchar(ner_product))) %>%
  select(-distance, -location, -match) %>%
  bind_cols(
    bind_rows(apply(df, 1,
                    function(x) as.data.frame(stringdist::afind(x["ner_product"], x["product"]))))) %>%
  mutate(product_acc = pmin(product_acc, 1 - (distance/nchar(product)))) %>%
  select(-distance, -location, -match)

df <- df %>%
  bind_cols(
    bind_rows(apply(df, 1,
                    function(x) as.data.frame(stringdist::afind(x["version"], x["ner_version"]))))) %>%
  mutate(version_acc = 1 - (distance/nchar(ner_version))) %>%
  select(-distance, -location, -match) %>%
  bind_cols(
    bind_rows(apply(df, 1,
                    function(x) as.data.frame(stringdist::afind(x["ner_version"], x["version"]))))) %>%
  mutate(version_acc = pmin(version_acc, 1 - (distance/nchar(version)))) %>%
  select(-distance, -location, -match)

df <- df %>%
  mutate(predict_cpe = paste(ner_vendor, ner_product, ner_version, sep = ":")) %>%
  mutate(cpe_lite = paste(vendor, product, version, sep = ":"))

df <- df %>%
  bind_cols(
    bind_rows(apply(df, 1,
                    function(x) as.data.frame(stringdist::afind(x["predict_cpe"], x["cpe_lite"]))))) %>%
  mutate(accuracy = 1 - (distance/nchar(cpe_lite))) %>%
  select(-distance, -location, -match)

