library(mitre)
suppressPackageStartupMessages(library(dplyr))
library(tokenizers)

seed <- 42
num_samples <- Inf
num_samples <- 10000

set.seed(seed)


cpe_add_tokenstats <- function(df = mitre::cpe_lstm_dataset(), num_samples = Inf) {
  valchars <- paste0("char_", mitre::cpe_valid_chars(type = "output", taste = "hex", add_underline = T))
  cpe_chars <- rep(0, length(valchars))
  names(cpe_chars) <- valchars
  cpe_chars <- data.frame(t(cpe_chars))

  if (num_samples != Inf) {
    df <- sample_n(df, num_samples)
  }

  df <- df %>%
    select(id, title) %>%
    bind_cols(cpe_chars)

  dfids <- df %>% select(id, title)
  df <- lapply(tokenizers::tokenize_characters(iconv(x = df$title, to = "ASCII//TRANSLIT"),
                                               strip_non_alphanum = F, simplify = T),
               function(x) {
                 y <- table(x)
                 names(y) <- paste0("char_", DescTools::DecToHex(DescTools::CharToAsc(names(y))))
                 as.data.frame(t(as.matrix(y)))
               })
  names(df) <- dfids$id

  df <- df %>%
    bind_rows(.id = "id")
  df$id <- as.integer(df$id)

  df <- left_join(dfids,
                  df, by = "id") %>%
    mutate_all(~replace(., is.na(.), 0)) %>%
    select(id, title, names(cpe_chars))

  return(df)
}

# Get CPE title and all char counts

# df <- cpe_add_tokenstats()

cpes <- cpe_latest_data()

df <- cpes %>% select(id, title, version)

selected_rows <- grepl("^(\\d|\\.)+$", df$version)
df_nums <- df[selected_rows, ]
df <- df[!selected_rows, ]

selected_rows <- grepl("^\\-$", df$version)
df_nul <- df[selected_rows, ]
df <- df[!selected_rows, ]

selected_rows <- grepl("^(\\d|\\.|\\-|\\_)+$", df$version)
df_nums_minus <- df[selected_rows, ]
df <- df[!selected_rows, ]

selected_rows <- grepl("^(\\d|\\.|\\-|\\_|x)+$", df$version)
df_numx_minus <- df[selected_rows, ]
df <- df[!selected_rows, ]

selected_rows <- grepl("^(\\d|\\.|\\-|\\_|x)+\\w+$", df$version)
df_numxm_chars <- df[selected_rows, ]
df <- df[!selected_rows, ]

selected_rows <- grepl("^(\\d|[[:punct:]])+$", df$version)
df_nums_symb <- df[selected_rows, ]
df <- df[!selected_rows, ]

selected_rows <- grepl("^([[:alpha:]]|[[:punct:]])+$", df$version)
df_notnum <- df[selected_rows, ]
df <- df[!selected_rows, ]

selected_rows <- grepl("^(\\.|\\-|\\_|x|[[:xdigit:]])+$", df$version)
df_hexa <- df[selected_rows, ]
df_other <- df[!selected_rows, ]

df <- cpes %>% select(id, title, version)

df$type_version <- rep("other", nrow(df))
df$type_version[df$id %in% df_hexa$id] <- "hexa"
df$type_version[df$id %in% df_notnum$id] <- "notnum"
df$type_version[df$id %in% df_nul$id] <- "nul"
df$type_version[df$id %in% df_nums$id] <- "nums"
df$type_version[df$id %in% df_nums_minus$id] <- "nums_minus"
df$type_version[df$id %in% df_nums_symb$id] <- "nums_symb"
df$type_version[df$id %in% df_numx_minus$id] <- "numx_minus"
df$type_version[df$id %in% df_numxm_chars$id] <- "numxm_chars"

num_samples <- 10000



df_train <- df
df_train$type_version <- as.factor(df_train$type_version)

df_train1 <- df_train %>%
  filter(type_version == "nums") %>%
  mutate(vers_len = nchar(version)) %>%
  sample_n(num_samples/2, weight = vers_len)

df_train2 <- df_train %>%
  filter(type_version != "nums") %>%
  sample_n((num_samples/2) + (num_samples %% 2), weight = type_version)

df_train <- bind_rows(df_train1 %>% select(id, title, version),
                      df_train2 %>% select(id, title, version)) %>%
  sample_n(num_samples)




######### BACKLOG
#########
# valchars <- paste0("char_", mitre::cpe_valid_chars(type = "output", taste = "hex", add_underline = T))
# cpe_chars <- rep(0, length(valchars))
# names(cpe_chars) <- valchars
# cpe_chars <- data.frame(t(cpe_chars))
#
# df <- mitre::cpe_lstm_dataset()
#
# if (num_samples != Inf) {
#   df <- sample_n(df, num_samples)
# }
#
# df <- df %>%
#   select(id, title) %>%
#   bind_cols(cpe_chars)
#
# # df_tk <- lapply(tokenizers::tokenize_characters(iconv(x = df$title, to = "ASCII//TRANSLIT"),
# #                                                 strip_non_alphanum = F, simplify = T), table)
#
# dfids <- df %>% select(id, title)
# df <- lapply(tokenizers::tokenize_characters(iconv(x = df$title, to = "ASCII//TRANSLIT"),
#                                              strip_non_alphanum = F, simplify = T),
#              function(x) {
#                y <- table(x)
#                names(y) <- paste0("char_", DescTools::DecToHex(DescTools::CharToAsc(names(y))))
#                as.data.frame(t(as.matrix(y)))
#              })
# names(df) <- dfids$id
#
# df <- df %>%
#   bind_rows(.id = "id")
# df$id <- as.integer(df$id)
#
# df <- left_join(dfids,
#                 df, by = "id") %>%
#   mutate_all(~replace(., is.na(.), 0)) %>%
#   select(id, title, names(cpe_chars))
#
#
# df_tk <- df %>% select(names(cpe_chars)) %>%
#   colSums()
#
# # df_tk <- df_tk[, names(df)]
# # df <- NULL
#
# # df_tokens <- df_tk
#
#
# # df_tk <- bind_rows(df %>% select(-title), df_tk)
# #
# # df_tk <- df_tk %>%
# #   mutate_all(~replace(., is.na(.), 0)) %>%
# #   group_by(id) %>%
# #   summarise_all(sum) %>%
# #   select(sort(names(.)))
# #
# # df_tk <- left_join(df %>% select(id, title),
# #                 df_tk, by = "id")
#
