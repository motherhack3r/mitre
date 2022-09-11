library(mitre)
suppressPackageStartupMessages(library(dplyr))
library(tokenizers)

seed <- 42
num_samples <- Inf

set.seed(seed)

valchars <- paste0("char_", mitre::cpe_valid_chars(type = "output", taste = "hex"))
cpe_chars <- rep(0, length(valchars))
names(cpe_chars) <- valchars
cpe_chars <- data.frame(t(cpe_chars))

cpes <- mitre::cpe_lstm_dataset(keep_deprecated = T)

if (num_samples == Inf) {
  df <- cpes
} else {
  df <- sample_n(cpes, num_samples)
}

df <- df %>%
  select(id, title) %>%
  bind_cols(cpe_chars)

# df_tk <- lapply(tokenizers::tokenize_characters(iconv(x = df$title, to = "ASCII//TRANSLIT"),
#                                                 strip_non_alphanum = F, simplify = T), table)

df_tk <- lapply(tokenizers::tokenize_characters(iconv(x = df$title, to = "ASCII//TRANSLIT"),
                                                       strip_non_alphanum = F, simplify = T),
                function(x) {
                  y <- table(x)
                  names(y) <- paste0("char_", DescTools::DecToHex(DescTools::CharToAsc(names(y))))
                  as.data.frame(t(as.matrix(y)))
                })
names(df_tk) <- df$id

df_tk <- df_tk %>%
  bind_rows(.id = "id")
df_tk$id <- as.integer(df_tk$id)

df_tk <- left_join(df %>% select(id, title),
                   df_tk, by = "id") %>%
  mutate_all(~replace(., is.na(.), 0))

df_tk <- df_tk[, names(df)]

df_tk <- bind_rows(df %>% select(-title), df_tk)

df_tk <- df_tk %>%
  mutate_all(~replace(., is.na(.), 0)) %>%
  group_by(id) %>%
  summarise_all(sum) %>%
  select(sort(names(.)))

df_tk <- left_join(df %>% select(id, title),
                df_tk, by = "id")

