library(dplyr, warn.conflicts = FALSE)
library(tidyr, warn.conflicts = FALSE)

# cpes data frame
# load("data/cpe.nist.rda")
# cpes <- cpe.nist
# rm(cpe.nist)
cpes <- mitre::cpe.nist

# cpe vendors data frame
vend_prod <- cpes %>%
  group_by(vendor, product) %>%
  summarise(num_vers = n()) %>%
  ungroup()

vendors <- vend_prod %>%
  group_by(vendor) %>%
  summarise(num_prods = sum(num_vers)) %>%
  ungroup()

# Inventory data frame
df <- mitre::getInventory()
df_copy <- df
# df <- df_copy


str2wfn_str <- function(x = "Nuxtjs @nuxt/devalue 1.2.0 for Node.js") {
  x <- textclean::replace_non_ascii(x)
  x <- iconv(x, "UTF-8", "ASCII", sub = "")
  x <- stringr::str_replace_all(x, "[^a-zA-Z|\\'|\\-|\\!|\\$|\\(|\\)|\\,|\\.|\\/|\\;|\\?|\\@|\\[|\\]|\\\\|_|\\||\\+|\\d| ]", "")
  x <- stringr::str_trim(x)

  return(x)
}

# cpe_vendor_chars <- sort(unique(unlist(lapply(unique(cpes$vendor), function(s) strsplit(s, split="")[[1]]))))
# scm_vendor_chars <- sort(unique(unlist(lapply(unique(df$wfn_vendor), function(s) strsplit(s, split="")[[1]]))))

normal_wfn_vendor <- function(x = "Microsoft Corporation") {
  # encode vendor string to WFN
  x <- tolower(x)
  x <- stringr::str_replace_all(x, "(\\s|,)+(corporation|ltd|llc|inc).{0,1}$", "")
  x <- stringr::str_replace_all(x, "(\\s|,)+software(\\s|,){0,1}", " ")
  x <- stringr::str_trim(x)
  x <- stringr::str_replace_all(x, "(\\s|,)+foundation(\\s|,){0,1}", " ")
  x <- stringr::str_trim(x)
  x <- stringr::str_replace_all(x, "([^[:alnum:]])", "\\\\\\1")
  x <- stringr::str_replace_all(x, "\\\\\\s", "_")
  x <- str2wfn_str(x)

  return(x)
}

normal_wfn_product <- function(x = "Oracle VM VirtualBox 6.1.34") {
  # encode product string to WFN
  x <- tolower(x)
  x <- stringr::str_replace_all(x, "\\(.*$", "")
  x <- stringr::str_trim(x)
  x <- stringr::str_replace_all(x, "(\\s|,|-)+v*(\\d+\\.{0,1})+\\.{0,1}\\d*$", "")
  x <- stringr::str_replace_all(x, "\\s", "_")
  x <- stringr::str_replace_all(x, "_\\-_.*$", "")
  x <- stringr::str_replace_all(x, "_\\d+\\.\\d+.*$", "")
  x <- stringr::str_replace_all(x, "_(x|amd)(32|64|86).*$", "")
  x <- stringr::str_replace_all(x, "_for_.*$", "")
  x <- stringr::str_replace_all(x, "\\(\\)", "")
  x <- stringr::str_trim(x)

  x <- str2wfn_str(x)

  return(x)
}

df$wfn_vendor <- normal_wfn_vendor(df$vendor)
df$wfn_product <- normal_wfn_product(df$name)
df$wfn_product <- stringr::str_replace_all(string = df$wfn_product, pattern = paste0("^", df$wfn_vendor,"_"), "")
df$wfn_product <- stringr::str_replace_all(df$wfn_product, "([\\'|\\\"|\\!|\\$|\\(|\\)|\\,|\\/|\\?|\\@|\\[|\\]|\\\\|\\||\\+])", "\\\\\\1")


# XXX: Needs more cleansing

sort(unique(df$wfn_product))

#############
# 1. get perfect match
# 2. match >= .99 with osa
# 3. match >= .90 with best method
# 4. match products knowing candidates

# Add perfect match with accuracy 1; others as 0
df$wfn_vendor_acc <- as.numeric(df$wfn_vendor %in% vend_prod$vendor)

# Not perfect match as raw_vendors
raw_vendors <- df %>%
  filter(wfn_vendor_acc < 1) %>%
  filter(wfn_vendor != "") %>%
  select(wfn_vendor) %>%
  unique()
raw_vendors <- sort(raw_vendors$wfn_vendor)
df_raw_vendors <- data.frame(wfn_vendor = raw_vendors)

# compare raw_vendors with CPE vendors using OSA
vcands_osa <- stringdist::stringsimmatrix(raw_vendors, vendors$vendor, method = "osa")
vcands_osa <- as.data.frame(vcands_osa)
names(vcands_osa) <- vendors$vendor
row.names(vcands_osa) <- raw_vendors
df_raw_vendors$cpe_osa <- vendors$vendor[apply(vcands_osa, 1, which.max)]
df_raw_vendors$cpe_osa_acc <- apply(vcands_osa, 1, max)

np_match <- df_raw_vendors %>% filter(cpe_osa_acc > 0.90)
if (nrow(np_match) > 0) {
  # Add findings to df
  np_match$cpe_vendor <- apply(np_match, 1,
                               function(x)
                                 names(tail(sort(table(as.character(x))),1)))
  for (i in 1:nrow(np_match)) {
    df$wfn_vendor_acc[df$wfn_vendor == np_match$wfn_vendor[i]] <- np_match$cpe_osa_acc[i]
    df$wfn_vendor[df$wfn_vendor == np_match$wfn_vendor[i]] <- np_match$cpe_vendor[i]
  }
}

vcands_lcs <- stringdist::stringsimmatrix(raw_vendors, vendors$vendor, method = "lcs")
vcands_lcs <- as.data.frame(vcands_lcs)
names(vcands_lcs) <- vendors$vendor
row.names(vcands_lcs) <- raw_vendors
df_raw_vendors$cpe_lcs <- vendors$vendor[apply(vcands_lcs, 1, which.max)]
df_raw_vendors$cpe_lcs_acc <- apply(vcands_lcs, 1, max)

vcands_jw <- stringdist::stringsimmatrix(raw_vendors, vendors$vendor, method = "jw")
vcands_jw <- as.data.frame(vcands_jw)
names(vcands_jw) <- vendors$vendor
row.names(vcands_jw) <- raw_vendors
df_raw_vendors$cpe_jw <- vendors$vendor[apply(vcands_jw, 1, which.max)]
df_raw_vendors$cpe_jw_acc <- apply(vcands_jw, 1, max)

vcands_sx <- stringdist::stringsimmatrix(raw_vendors, vendors$vendor, method = "soundex")
vcands_sx <- as.data.frame(vcands_sx)
names(vcands_sx) <- vendors$vendor
row.names(vcands_sx) <- raw_vendors
df_raw_vendors$cpe_sx <- vendors$vendor[apply(vcands_sx, 1, which.max)]
df_raw_vendors$cpe_sx_acc <- apply(vcands_sx, 1, max)

np_match <- df_raw_vendors %>%
  filter((cpe_osa_acc > 0.9) | (cpe_lcs_acc > 0.9) | (cpe_jw_acc > 0.9)) %>%
  mutate(wfn_vendor_acc = max(cpe_osa_acc, cpe_lcs_acc, cpe_jw_acc)) %>%
  select(wfn_vendor, cpe_osa, cpe_lcs, cpe_jw, cpe_sx, wfn_vendor_acc)
if (nrow(np_match) > 0) {
  np_match$cpe_vendor <- apply(np_match, 1,
                               function(x)
                                 names(tail(sort(table(as.character(x))),1)))
  for (i in 1:nrow(np_match)) {
    df$wfn_vendor_acc[df$wfn_vendor == np_match$wfn_vendor[i]] <- np_match$wfn_vendor_acc[i]
    df$wfn_vendor[df$wfn_vendor == np_match$wfn_vendor[i]] <- np_match$cpe_vendor[i]
  }
}

#############

View(t(vcands_lv))




which(complete.cases(vcands))

kk <- vcands
kk <- kk %>%
  replace(is.na(.), 0) %>%
  rowwise() %>%
  mutate(
    strdist_max = max(c_across(names(kk))),
    strdist_cut = quantile(c_across(names(kk)), probs = c(0,0.6,0.97,1))[3]
  )

kkk <- kk %>% select(-strdist_cut, -strdist_max)
kk <- kk %>% select(strdist_cut, strdist_max)

k <<- character()
for (i in 1:nrow(kkk)) {
  can <- names(kkk[i, which(kkk[i,] >= kk$strdist_max[i])])
  k <<- c(k, can[1])
}

kk$vendpred <- k
kk$topred <- row.names(vcands)

# median(as.numeric(kk[3,1:ncol(kk)]))
# mean(as.numeric(kk[3,1:ncol(kk)]))
# max(as.numeric(kk[2,1:ncol(kk)]))
# quantile(as.numeric(kk[4,1:ncol(kk)]), probs = c(0,0.6,0.95,1))[3]
# boxplot(as.numeric(kk[4,1:ncol(kk)]))


# pct_encode(invents[16])
# str2wfn_encode(invents[16])

