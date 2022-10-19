#' Return data.frame with installed software name, version and vendor.
#' Set predict_cpes as TRUE to predict CPEs using ML.
#' Set predict_cves as TRUE to predict CPEs and its vulnerabilities as CVEs
#'
#' @param verbose logical
#' @param predict_cpes logical
#' @param predict_cves logical
#'
#' @return data.frame
#' @export
#'
#' @examples
#' inventory <- getInventory()
getInventory <- function(verbose = FALSE, predict_cpes = FALSE, predict_cves = FALSE) {
  if (verbose) print(paste0("[*] ", "Checking Operating System ..."))
  switch(Sys.info()[['sysname']],
         Windows = {
           if (verbose) print(paste0("[-] Windows detected."))
           df_inventory <- get_win_inventory(verbose = verbose,
                                             predict_cpes = predict_cpes,
                                             predict_cves = predict_cves)
           },
         Linux   = {
           if (verbose) print(paste0("[-] Linux detected."))
           df_inventory <- get_unix_inventory(verbose = verbose,
                                             predict_cpes = predict_cpes,
                                             predict_cves = predict_cves)
           },
         Darwin  = {
           if (verbose) print(paste0("[-] Mac detected."))
           df_inventory <- get_mac_inventory(verbose = verbose,
                                             predict_cpes = predict_cpes,
                                             predict_cves = predict_cves)
           })

  if (predict_cpes) {
    df_inventory <- cpe_generate(df = df_inventory, verbose = verbose)
    if (predict_cves) {
      df_inventory <- left_join(df_inventory,
                                df_inventory %>%
                                  filter(cpe_score > 0.5) %>%
                                  separate(col = cpe , sep = ":", extra = "merge",
                                           into = c("std", "v", "part", "vendor", "product", "version", "tail")) %>%
                                  select(id, vendor, product, version, vendor, product, version) %>%
                                  mutate(cpelite = paste0(":", paste(vendor, product, sep = ":"), ":")) %>%
                                  select(id, cpelite, version) %>%
                                  rowwise() %>%
                                  mutate(cves = cpelite_vulnerable_configs(x = cpelite, x_vers = version, verbose = verbose)) %>%
                                  ungroup() %>% select(id, cves),
                                by = "id")
    }
  }

  return(df_inventory)
}


#' Return data.frame with installed software name, version and vendor.
#' Set predict_cpes as TRUE to predict CPEs using ML.
#' Set predict_cves as TRUE to predict CPEs and its vulnerabilities as CVEs
#'
#' @param verbose logical
#' @param predict_cpes logical
#' @param predict_cves logical
#'
#' @return data.frame
get_mac_inventory <- function(verbose = FALSE, predict_cpes = FALSE, predict_cves = FALSE) {
  print("Only tested on Windows 10 and Debian. Sorry.")
  return(NA)
}


#' Return data.frame with installed software name, version and vendor.
#' Set predict_cpes as TRUE to predict CPEs using ML.
#' Set predict_cves as TRUE to predict CPEs and its vulnerabilities as CVEs
#'
#' @param verbose logical
#' @param predict_cpes logical
#' @param predict_cves logical
#'
#' @return data.frame
get_unix_inventory <- function(verbose = FALSE, predict_cpes = FALSE, predict_cves = FALSE) {
  if (.Platform$OS.type == "unix") {
    # Debian
    if (verbose) print(paste0("[-] ", "Running dpkg ..."))
    sw <- system("dpkg-query -W -f='${binary:Package}\\;${Architecture}\\;${Version}\\;${Maintainer}\\n'", intern = T)
    df.sw <- utils::read.csv(text = sw, sep = ";", header = F,
                             col.names = c("name", "architecture", "version", "mantainer"))
    if (verbose) print(paste0("[-] ", "Inventory normalization..."))
    df.sw$name <- sapply(df.sw$name, function(x) stringr::str_split(x, ":")[[1]][1])
    df.sw$product <- stringr::str_conv(df.sw$name, "UTF-8")
    df.sw$vendor <- df.sw$mantainer
    df.sw$id <- 1:nrow(df.sw)
    df.sw <- df.sw[, c("id", "vendor", "product", "version")]

    return(df.sw)
  }


}


#' Return data.frame with installed software name, version and vendor.
#' Set predict_cpes as TRUE to predict CPEs using ML.
#' Set predict_cves as TRUE to predict CPEs and its vulnerabilities as CVEs
#'
#' @param verbose logical
#' @param predict_cpes logical
#' @param predict_cves logical
#'
#' @return data.frame
get_win_inventory <- function(verbose = FALSE, predict_cpes = FALSE, predict_cves = FALSE){
  NewSWEntry <- function(name = "", version = "", vendor = "") {
    return(data.frame(name = name,
                      version = version,
                      vendor = vendor,
                      stringsAsFactors = F))}

  # SW1
  if (verbose) print(paste0("[-] ", "Searching installed software in SCCM registry ..."))
  sw1 <- system("powershell.exe \"Get-ItemProperty HKLM:\\Software\\Wow6432Node\\Microsoft\\Windows\\CurrentVersion\\Uninstall\\* | Select-Object DisplayName, DisplayVersion, Publisher, InstallDate | Format-List\"", intern = T)
  sw1 <- stringi::stri_conv(sw1, from = "CP850", to = "UTF-8")

  df.sw1 <- NewSWEntry()
  for (linia in sw1) {
    new.sw <- NewSWEntry()
    new.row <- lapply(stringr::str_split(linia, ":"), stringr::str_trim)[[1]]
    switch(new.row[1],
           "DisplayName" = {
             new.sw.name <- new.row[2]
           },
           "DisplayVersion" = {
             new.sw.version <- new.row[2]
           },
           "Publisher" = {
             new.sw.publisher <- new.row[2]
           },
           "InstallDate" = {
             new.sw <- NewSWEntry(new.sw.name, new.sw.version, new.sw.publisher)
             new.sw.name <- new.sw.version <- new.sw.publisher <- ""
             df.sw1 <- dplyr::bind_rows(df.sw1, new.sw)
           },
           {
             # Default
           }
    )
  }
  df.sw1 <- df.sw1[!apply(df.sw1 == "", 1, all),]

  # SW2
  if (verbose) print(paste0("[-] ", "Searching installed software in CurrentVersion registry ..."))
  sw2 <- system("powershell.exe \"Get-ItemProperty HKLM:\\SOFTWARE\\Microsoft\\Windows\\CurrentVersion\\Uninstall\\* | Select-Object DisplayName, DisplayVersion, Publisher, InstallDate | Format-List\"", intern = T)
  sw2 <- stringi::stri_conv(sw2, from = "CP850", to = "UTF-8")

  df.sw2 <- NewSWEntry()
  for (linia in sw2) {
    new.sw <- NewSWEntry()
    new.row <- lapply(stringr::str_split(linia, ":"), stringr::str_trim)[[1]]
    switch(new.row[1],
           "DisplayName" = {
             new.sw.name <- new.row[2]
           },
           "DisplayVersion" = {
             new.sw.version <- new.row[2]
           },
           "Publisher" = {
             new.sw.publisher <- new.row[2]
           },
           "InstallDate" = {
             new.sw <- NewSWEntry(new.sw.name, new.sw.version, new.sw.publisher)
             new.sw.name <- new.sw.version <- new.sw.publisher <- ""
             df.sw2 <- dplyr::bind_rows(df.sw2, new.sw)
           },
           {
             # Default
           }
    )
  }
  df.sw2 <- df.sw2[!apply(df.sw2 == "", 1, all),]

  # SW3
  if (verbose) print(paste0("[-] ", "Searching installed software in WMI objects ..."))
  sw3 <- system("powershell.exe \"Get-WmiObject Win32_Product | Sort-Object Name | Format-List Name, Version, Vendor\"", intern = T)
  sw3 <- stringi::stri_conv(sw3, from = "CP850", to = "UTF-8")

  df.sw3 <- NewSWEntry()
  for (linia in sw3) {
    new.sw <- NewSWEntry()
    new.row <- lapply(stringr::str_split(linia, ":"), stringr::str_trim)[[1]]
    switch(new.row[1],
           "Name" = {
             new.sw.name <- new.row[2]
           },
           "Version" = {
             new.sw.version <- new.row[2]
           },
           "Vendor" = {
             new.sw.publisher <- new.row[2]
             new.sw <- NewSWEntry(new.sw.name, new.sw.version, new.sw.publisher)
             new.sw.name <- new.sw.version <- new.sw.publisher <- ""
             df.sw3 <- dplyr::bind_rows(df.sw3, new.sw)
           },
           {
             # Default
           }
    )
  }
  df.sw3 <- df.sw3[!apply(df.sw3 == "", 1, all),]

  if (verbose) print(paste0("[-] ", "Merge and normalize inventory ..."))
  df.sw <- dplyr::bind_rows(df.sw1, df.sw2, df.sw3)
  df.sw <- df.sw[!duplicated(df.sw),]
  df.sw <- dplyr::arrange(df.sw, "name")

  df.sw$product <- stringr::str_conv(df.sw$name, "UTF-8")
  df.sw$id <- 1:nrow(df.sw)
  df.sw <- df.sw[, c("id", "vendor", "product", "version")]

  return(df.sw)
}


