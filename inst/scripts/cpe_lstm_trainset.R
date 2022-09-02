col2predict <- "version"
num_samples <- 100000
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

df_lstm <- df_lstm[, c("id", "title", "cpe.23", "part", "vendor", "product", "version")]

df <- mitre::nlp_cpe_dataset(df = df_lstm, keep_deprecated = T)
df <- df[, c("title", col2predict, "id")]


# remove rows with escaped chars because of tagging regex
# df <- df[!grepl(pattern = "\\\\", df[, col2predict]), ]
# replace WFN _ to space
# df[, col2predict] <- stringr::str_replace_all(df[, col2predict], "_", " ")

# cpes <- mitre::nlp_cpe_sample_dataset(df = df, num_samples = num_samples, seed = seed)
# cpes <- cpes[, c("title", col2predict)]

readr::write_delim(x = df, file = "C:/DEVEL/code/data/lstm_version_trainset.csv", delim = "\t", quote = "none")
