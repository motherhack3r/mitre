# CREATE TRAIN SET FOR NER
library(mitre)

p_type <- "vpv"
p_num_samples <- 10000
p_seed <- 42

set.seed(p_seed)

cpes <- mitre::cpe_latest_data(remote = T, keep_deprecated = T)

cpes4ner <- mitre::cpe_add_notation(df = cpes, type = p_type)
cpes4ner <- cpes4ner[!is.na(cpes4ner$annotated), ]

cpes_sts <- mitre::cpe_stats(df = cpes)
cpes4ner <- dplyr::left_join(cpes4ner, cpes_sts[, c("vendor", "popular")], by = "vendor")

df <- dplyr::slice_sample(cpes4ner, weight_by = popular, n = p_num_samples)
