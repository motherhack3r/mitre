col2predict <- "version"
num_samples <- 200000
seed <- 42
verbose <- TRUE

library(mitre)

cpes <- mitre::cpe_latest_data(remote = T, keep_deprecated = T, verbose = verbose)
cpe_stats <- cpe_stats(df = cpes)
cpe_stats <- dplyr::select(cpe_stats, -n)
cpes <- dplyr::left_join(cpes, cpe_stats, by = "vendor")

df <- mitre::cpe_lstm_dataset(df = cpes, verbose = verbose)
df <- dplyr::left_join(df, cpes[, c("id","popular")], by = "id")

df_train <- dplyr::sample_n(df, size = num_samples, weight = df$popular)
df_train <- df_train[, c("title", col2predict, "cpe.23")]

readr::write_delim(x = df_train, file = "../../datos/lstm_version_trainset.csv",
                   delim = "\t", quote = "none")


# --------------
# rx_noversion <- "[^\\s\\-\\.\\da-z]"
# selected_rows <- stringr::str_detect(cpes$version, rx_noversion)
#
# rx_notitle <- "[^\\s\\!\\&\\(\\)\\+\\,\\-\\.\\/\\:\\da-zA-Z]"
# selected_rows <- !selected_rows & !stringr::str_detect(cpes$title, rx_notitle)
#
# df_lstm <- mitre::cpe_lstm_dataset(df = cpes[selected_rows, ])
# df <- mitre::cpe_add_features(df = df_lstm)
#
#
# df_train <- df[df$train_version, c("title", col2predict, "cpe.23")]
# readr::write_delim(x = df_train, file = "C:/DEVEL/code/data/lstm_version_trainset.csv", delim = "\t", quote = "none")
