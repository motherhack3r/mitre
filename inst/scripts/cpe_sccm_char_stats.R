library(ggplot2)
library(dplyr)
library(tidyr)

# # Chars accepted by avstring
# valid_chars <- " *-\\!\"#$%&'()+.,/:;<=>@[]^`{|}~0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"
# avs_chars <- data.frame(char = sapply(DescTools::CharToAsc(valid_chars), DescTools::AscToChar),
#                         dec = DescTools::CharToAsc(valid_chars),
#                         hex = as.character(DescTools::DecToHex(DescTools::CharToAsc(valid_chars))),
#                         stringsAsFactors = FALSE)

regex_wfn <- "[ \\*\\-\\!\"\\#\\$\\%\\&\\'\\(\\)\\+\\.\\,\\/\\:\\;\\<\\=\\>\\@\\[\\]\\^\\`\\{\\|\\}\\~0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz]"
regex_nonwfn <- "[^ \\*\\-\\!\"\\#\\$\\%\\&\\'\\(\\)\\+\\.\\,\\/\\:\\;\\<\\=\\>\\@\\[\\]\\^\\`\\{\\|\\}\\~0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz]"

cpes <- mitre::getLatestDataSet(name = "cpe")
cpes <- cpes$cpe$cpe.nist
cpes <- cpes[, c("title", "vendor", "product", "version")]

cpe_titles <- cpes$title

titles_chars <- tokenizers::tokenize_characters(cpe_titles)
cpe_chars <- as.data.frame(table(unlist(titles_chars)),
                           stringsAsFactors = FALSE)
names(cpe_chars) <- c("char", "freq")
cpe_chars$wfn <- sapply(cpe_chars$char, function(x) stringr::str_detect(x, regex_wfn))
cpe_chars$valid <- ifelse(cpe_chars$wfn, "Yes", "No")
cpe_chars$dec <- DescTools::CharToAsc(cpe_chars$char)
cpe_chars$dec <- sapply(cpe_chars$dec, sum)

g <- ggplot(cpe_chars) +
  aes(x = reorder(char, dec), y = freq, fill = valid, colour = valid) +
  geom_col() +
  scale_fill_hue(direction = 1) +
  scale_color_hue(direction = 1) +
  scale_fill_manual(
    values = c(No = "#67001F",
               Yes = "#053061")
  ) +
  scale_color_manual(
    values = c(No = "#67001F",
               Yes = "#053061")
  ) +
  theme_light() +
  theme(
    axis.title.x = element_text(size = 0)
  )
g


sccm_chars <- readRDS("C:/Users/humbe/OneDrive/DOCENCIA/POLIMI/TFM/05.Data Exploratory/sccm_chars.rds")
sccm_chars$dec <- sapply(sccm_chars$dec, sum)
sccm_chars[c(72,123,171,131), "wfn"] <- F
sccm_chars[c(72,123,171,131), "valid"] <- "No"

g <- sccm_chars %>% filter(wfn) %>% arrange(desc(freq)) %>%
  ggplot() +
  aes(x = reorder(char, dec), y = freq, fill = valid, colour = valid) +
  geom_col() +
  scale_fill_hue(direction = 1) +
  scale_color_hue(direction = 1) +
  scale_fill_manual(
    values = c(No = "#67001F",
               Yes = "#053061")
  ) +
  scale_color_manual(
    values = c(No = "#67001F",
               Yes = "#053061")
  ) +
  theme_light() +
  theme(
    axis.title.x = element_text(size = 0)
  )
g


g <- sccm_chars %>% filter(!wfn) %>% arrange(desc(freq)) %>% head(45) %>%
  ggplot() +
  aes(x = reorder(char, -freq), y = freq, fill = valid, colour = valid) +
  geom_col() +
  scale_fill_hue(direction = 1) +
  scale_color_hue(direction = 1) +
  scale_fill_manual(
    values = c(No = "#67001F",
               Yes = "#053061")
  ) +
  scale_color_manual(
    values = c(No = "#67001F",
               Yes = "#053061")
  ) +
  theme_light() +
  theme(
    axis.title.x = element_text(size = 0)
  )
g


#
# g <- ggplot(sccm_chars) +
#   aes(x = char, y = freq, fill = valid, colour = valid) +
#   geom_col() +
#   scale_fill_manual(
#     values = c(No = "#67001F",
#                Yes = "#053061")
#   ) +
#   scale_color_manual(
#     values = c(No = "#67001F",
#                Yes = "#053061")
#   ) +
#   theme_minimal()
# g
#
# g <- ggplot(cpe_chars) +
#   aes(x = reorder(char, dec), y = freq, fill = valid, colour = valid) +
#   geom_col() +
#   scale_fill_brewer(palette = "RdBu", direction = 1) +
#   scale_color_brewer(palette = "RdBu", direction = 1) +
#   theme_minimal()
# g
#
# tit <- cpe_titles[1]
# tit <- "Puçal·lim 1.0"
# tit2 <- c(tit, cpe_titles[1])
#
# tit_tok <- tokenizers::tokenize_characters(tit)
# which(sapply(tit_tok, function(x) stringr::str_detect(x, regex_wfn)))
# which(sapply(tit_tok, function(x) stringr::str_detect(x, regex_nonwfn)))
#
# chars_notwfn <- unlist(stringr::str_extract_all(tit, regex_nonwfn, F))
#
# chars_notwfn <- as.character(unlist(sapply(cpe_titles,
#                                            function(x)
#                                              unlist(stringr::str_extract_all(x, regex_nonwfn, F)))))
# chars_notwfn <- as.data.frame(sort(table(chars_notwfn)))
#
#
#
# valid_hexachars <- paste0("[", paste0(sapply(DescTools::DecToHex(sort(DescTools::CharToAsc(valid_chars))),
#                                              function(x) paste0("\x5c","x",x)), collapse = ""), "]")
# valid_hexachars <- "([\x20-\x2d]|[\x2f-\x3e]|[\x40-\x5e]|[\x60-\x7e])"
#
# regex_notvalidhexa <- stringr::fixed(paste0("[^", paste0(valid_hexachars, collapse = ""), "]"))
#
#
# selected_rows <- stringr::str_detect(cpe_chars$char, regex_notvalid)
#
#
#
# stringr::str_detect(tokenizers::tokenize_characters("askjdgask"), valid_hexachars)
#
# tfm_cpes <- cpes[which(sapply(tokenizers::tokenize_characters(cpe_titles),
#                               function(x)
#                                 any(stringr::str_detect(string = x,
#                                                         pattern = regex_nonwfn,
#                                                         negate = TRUE)))), ]
