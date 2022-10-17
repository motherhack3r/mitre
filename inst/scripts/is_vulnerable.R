library(dplyr)
library(tidyr)

cpe_is_version <- function(x = c("")) {
  if (is.na(x)) return(FALSE)
  return(grepl(pattern = "^\\d+([\\.\\-]\\d+)*$", x))
}

cpe_is_vulnerable <- function(x = c(""), cves = standards$cve$cve.nist) {
  cpes_iv <- rep(FALSE, length(x))

  return(cpes_iv)
}

cve_flatten_cpematch_or <- function(cpe_match = "", cve_id = "", numid = 1, parentid = 1) {
  df <- dplyr::bind_rows(lapply(cpe_match,
                                function(x) {
                                  y <- data.frame(vulnerable = x$vulnerable,
                                                  cpe23Uri = x$cpe23Uri,
                                                  versionStartExcluding = ifelse("versionStartExcluding" %in% names(x), x$versionStartExcluding, as.character(NA)),
                                                  versionStartIncluding = ifelse("versionStartIncluding" %in% names(x), x$versionStartIncluding, as.character(NA)),
                                                  versionEndExcluding = ifelse("versionEndExcluding" %in% names(x), x$versionEndExcluding, as.character(NA)),
                                                  versionEndIncluding = ifelse("versionEndIncluding" %in% names(x), x$versionEndIncluding, as.character(NA)),
                                                  cpe_name = ifelse("cpe_name" %in% names(x),
                                                                    ifelse(length(x$cpe_name) > 0,
                                                                           jsonlite::toJSON(x$cpe_name),
                                                                           as.character(NA)),
                                                                    as.character(NA)),
                                                  stringsAsFactors = FALSE)
                                  y$vc_id <- paste("vulnconf", cve_id, parentid, paste0("OR", numid), sep = ":")
                                  numid <<- numid + 1
                                  y
                                }))
  df$cve <- rep(cve_id, nrow(df))
  return(df)
}

cve_flatten_vulnconf <- function(vulnconf = "", cve_id = "", idnum = 1) {
  vcs <- RJSONIO::fromJSON(vulnconf)
  df <- dplyr::bind_rows(lapply(vcs,
                                function(vc) {
                                  vc.op <- vc$operator
                                  if (vc.op == "AND") {
                                    idand <- 1
                                    vc.cm <- lapply(vc$children,
                                                    function(x) {
                                                      y <- cve_flatten_cpematch_or(x$cpe_match, cve_id, parentid = idand)
                                                      idand <<- idand + 1
                                                      y
                                                    })
                                    vc.cm <- dplyr::bind_rows(vc.cm)
                                    vc.cm$vc_id <- paste(vc.cm$vc_id, paste0("AND", idnum), sep = ":")
                                  } else if (vc.op == "OR") {
                                    vc.cm <- cve_flatten_cpematch_or(vc$cpe_match, cve_id, parentid = idnum)
                                  } else {
                                    vc.cm <- data.frame()
                                  }
                                  idnum <<- idnum + 1
                                  vc.cm
                                }))
  return(df)
}

cpelite_check_vers <- function(x_vers, versionStartExcluding, versionStartIncluding, versionEndExcluding, versionEndIncluding) {
  k_sex <- if (cpe_is_version(versionStartExcluding)) numeric_version(versionStartExcluding) else versionStartExcluding
  k_sin <- if (cpe_is_version(versionStartIncluding)) numeric_version(versionStartIncluding) else versionStartIncluding
  k_eex <- if (cpe_is_version(versionEndExcluding)) numeric_version(versionEndExcluding) else versionEndExcluding
  k_ein <- if (cpe_is_version(versionEndIncluding)) numeric_version(versionEndIncluding) else versionEndIncluding

  if (is.na(k_sex)) {
    if (is.na(k_sin)) {
      if (is.na(k_eex)) {
        if (is.na(k_ein)) {
          # NA: Start Excluding, End Excluding, End Including, Start Including
          TRUE
        } else {
          # NA: Start Excluding, End Excluding, End Including, Start Including
          # OK: End Including
          ifelse(test = is.numeric_version(k_ein),
                 yes = (x_vers <= k_ein),
                 no = NaN)
        }
      } else {
        if (is.na(k_ein)) {
          # NA: Start Excluding, End Including
          # OK: Start Including, End Excluding
          ifelse(test = is.numeric_version(k_sin) & is.numeric_version(k_eex),
                 yes = (k_sin <= x_vers) & (x_vers < k_eex),
                 no = NaN)
        } else {
          # NA: Start Excluding
          # OK: Start Including, End Excluding, End Including
          NaN
        }
      }
    } else {
      # NA: Start Excluding
      # OK: Start Including
      if (is.na(k_eex)) {
        # NA: Start Excluding, End Excluding
        # OK: Start Including
        if (is.na(k_ein)) {
          # NA: Start Excluding, Start Including, End Excluding, End Including
          TRUE
        } else {
          # NA: Start Excluding, Start Including, End Excluding
          # OK: End Including
          ifelse(test = is.numeric_version(k_ein),
                 yes = (x_vers <= k_ein),
                 no = NaN)
        }
      } else {
        # NA: Start Excluding
        # OK: Start Including, End Excluding
        if (is.na(k_ein)) {
          # NA: Start Excluding, End Including
          # OK: Start Including, End Excluding
          ifelse(test = is.numeric_version(k_sin) & is.numeric_version(k_eex),
                 yes = (k_sin <= x_vers) & (x_vers < k_eex),
                 no = NaN)
        } else {
          # NA: Start Excluding
          # OK: Start Including, End Excluding, End Including
          NaN
        }
      }
    }
  } else {
    # OK: Start Excluding
    if (is.na(k_sin)) {
      # NA: Start Including
      # OK: Start Excluding
      if (is.na(k_eex)) {
        # NA: Start Including, End Excluding
        # OK: Start Excluding
        if (is.na(k_ein)) {
          # NA: Start Including, End Excluding, End Including
          # OK: Start Excluding
          ifelse(test = is.numeric_version(k_sin),
                 yes = (k_sin <= x_vers),
                 no = NaN)
        } else {
          # NA: Start Including, End Excluding
          # OK: Start Excluding, End Including
          ifelse(test = is.numeric_version(k_sex) & is.numeric_version(k_ein),
                 yes = (k_sex < x_vers) & (x_vers <= k_ein),
                 no = NaN)
        }
      } else {
        # NA: Start Including
        # OK: Start Excluding, End Excluding
        if (is.na(k_ein)) {
          # NA: Start Including, End Including
          # OK: Start Excluding, End Excluding
          ifelse(test = is.numeric_version(k_sex) & is.numeric_version(k_eex),
                 yes = (k_sex < x_vers) & (x_vers < k_eex),
                 no = NaN)
        } else {
          # NA: Start Including
          # OK: Start Excluding, End Excluding, End Including
          NaN
        }
      }
    } else {
      # NA: -
      # OK: Start Excluding, Start Including
      NaN
    }
  }
}

cpelite_vulnerable_configs <- function(x, x_vers, cves) {
  if (length(x) > 1) {
    rx <- paste0("(", paste(x, collapse = "|"), ")")
  } else {
    rx <- x
  }

  x_vc <- cves[which(grepl(pattern = rx, x = cves$vulnerable.configuration)), c("cve.id", "vulnerable.configuration")]

  if (nrow(x_vc) == 0) return(jsonlite::toJSON(NA))

  vulnerableconfigurations <- data.frame()
  for (i in 1:nrow(x_vc)) {
    vulnconf <- cve_flatten_vulnconf(x_vc$vulnerable.configuration[i],
                                     x_vc$cve.id[i])
    vulnerableconfigurations <- dplyr::bind_rows(vulnerableconfigurations, vulnconf)
  }

  k <- vulnerableconfigurations %>%
    filter(grepl(pattern = rx, x = cpe23Uri)) %>%
    filter(vulnerable) %>%
    select(vc_id, versionStartExcluding, versionStartIncluding, versionEndExcluding, versionEndIncluding)

  x_vers <- if (cpe_is_version(x_vers)) numeric_version(x_vers) else x_vers
  k_selected <- k %>%
    rowwise() %>%
    mutate(mark = cpelite_check_vers(x_vers,
                                     versionStartExcluding, versionStartIncluding,
                                     versionEndExcluding, versionEndIncluding)) %>%
    filter(mark > 0) %>% select(-mark) %>%
    mutate(cve = stringr::str_extract(string = vc_id, pattern = "CVE\\-\\d+\\-\\d+")) %>%
    mutate(conditional = stringr::str_detect(string = vc_id, pattern = "\\:AND\\d+")) %>%
    ungroup() %>%
    select(cve, conditional) %>%
    unique()

  return(jsonlite::toJSON(k_selected))
}

df_inventory <- readRDS("inst/scripts/inventory.rds")
mdata <- mitre::getLatestDataSet()
cves <- mdata$cve$cve.nist
rm(mdata)

df <- left_join(df_inventory,
                df_inventory %>%
                  filter(cpe_score > 0.5) %>%
                  separate(col = cpe , sep = ":", extra = "merge",
                           into = c("std", "v", "part", "vendor", "product", "version", "tail")) %>%
                  select(id, vendor, product, version, vendor, product, version) %>%
                  mutate(cpelite = paste0(":", paste(vendor, product, sep = ":"), ":")) %>%
                  select(id, cpelite, version) %>%
                  rowwise() %>%
                  mutate(cves = cpelite_vulnerable_configs(cpelite, version, cves)) %>%
                  ungroup() %>% select(id, cves),
                by = "id")
