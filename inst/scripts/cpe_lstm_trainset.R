col2predict <- "version"
num_samples <- 200000
seed <- 42

load("inst/extdata/cpe.nist.rda")

df_lstm <- cpe.nist
df_lstm$id <- 1:nrow(df_lstm)

# remove titles, vendor, product or versions with tabs
df_lstm <- df_lstm[!(grepl("\\t", df_lstm$title)), ]
df_lstm <- df_lstm[!(grepl("\\t", df_lstm$cpe.23)), ]
df_lstm <- df_lstm[!(grepl("\\t", df_lstm$vendor)), ]
df_lstm <- df_lstm[!(grepl("\\t", df_lstm$product)), ]
df_lstm <- df_lstm[!(grepl("\\t", df_lstm$version)), ]

df_lstm <- df_lstm[, c("id", "title", "cpe.23", "part", "vendor", "product", "version", "deprecated")]

df <- mitre::cpe_lstm_dataset(df = df_lstm)
df <- mitre::cpe_add_features(df)

df_train <- df[df$train_version, c("title", col2predict, "cpe.23")]
readr::write_delim(x = df_train, file = "C:/DEVEL/code/data/lstm_version_trainset.csv", delim = "\t", quote = "none")
