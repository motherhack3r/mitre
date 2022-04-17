#' Return data.frame with installed software name, version and vendor.
#'
#' @return data.frame
#' @export
#'
#' @examples
#' inventory <- getInventary()
getInventary <- function(){
  if (.Platform$OS.type == "windows") {
    # Windows with powershell
    sw1 <- system("powershell.exe \"Get-ItemProperty HKLM:\\Software\\Wow6432Node\\Microsoft\\Windows\\CurrentVersion\\Uninstall\\* | Select-Object DisplayName, DisplayVersion, Publisher, InstallDate | Format-List\"", intern = T)
    sw2 <- system("powershell.exe \"Get-ItemProperty HKLM:\\SOFTWARE\\Microsoft\\Windows\\CurrentVersion\\Uninstall\\* | Select-Object DisplayName, DisplayVersion, Publisher, InstallDate | Format-List\"", intern = T)
    sw3 <- system("powershell.exe \"Get-WmiObject Win32_Product | Sort-Object Name | Format-List Name, Version, Vendor\"", intern = T)

    NewSWEntry <- function(name = "", version = "", vendor = ""){return(data.frame(name = name, version = version, vendor = vendor, stringsAsFactors = F))}

    # SW1
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

    df.sw <- dplyr::bind_rows(df.sw1, df.sw2, df.sw3)
    df.sw <- df.sw[!duplicated(df.sw),]
    df.sw <- dplyr::arrange(df.sw, "name")

    return(df.sw)
  }

  if (.Platform$OS.type == "unix") {
    # Debian
    sw <- system("dpkg-query -W -f='${binary:Package}\\;${Architecture}\\;${Version}\\;${Maintainer}\\n'", intern = T)
    df.sw <- utils::read.csv(text = sw, sep = ";", header = F,
                      col.names = c("name", "architecture", "version", "mantainer"))
    df.sw$name <- sapply(df.sw$name, function(x) stringr::str_split(x, ":")[[1]][1])
    return(df.sw)
  }

  print("Only tested on Windows 10 and Debian. Sorry.")
  return(NA)
}


matchCPE <- function(name = "", version = "", vendor = "") {

  # Match part and product
  if (nchar(name) > 0) {
    name2title <- stringdist::stringdist(tolower(name), tolower(cpe.nist$title), method = "lv")
    score <- round(min(name2title)/nchar(name),2)
    if (score >= 0.1) {
      candidates <- cpe.nist[name2title <= min(name2title), ]
      product <- names(sort(table(candidates$product), T)[1])
      part <- names(sort(table(cpe.nist$part[cpe.nist$product == product]), T)[1])
    } else {
      # name2product <- stringdist::stringdist(tolower(name), tolower(cpe.nist$product), method = "lv")
      # score2 <- round((min(name2product) + 1)/nchar(name),2)
      # candidates2 <- cpe.nist[name2product <= min(name2product) + 1, ]
      #
      # name2vendor <- stringdist::stringdist(tolower(name), tolower(cpe.nist$vendor), method = "lv")
      # score3 <- round((min(name2vendor) + 1)/nchar(name),2)
      # candidates3 <- cpe.nist[name2vendor <= min(name2vendor) + 1, ]
      product <- janitor::make_clean_names(name)
      part <- "a"
    }
  } else {
    product <- "*"
    part <- "*"
  }

  # Match version
  # Ref: https://semver.org/#is-there-a-suggested-regular-expression-regex-to-check-a-semver-string
  if (nchar(version) > 0) {
    svs <- '^(0|[1-9]\\d*)\\.(0|[1-9]\\d*)(\\.(0|[1-9]\\d*))*(?:-((?:0|[1-9]\\d*|\\d*[a-zA-Z-][0-9a-zA-Z-]*)(?:\\.(?:0|[1-9]\\d*|\\d*[a-zA-Z-][0-9a-zA-Z-]*))*))?(?:\\+([0-9a-zA-Z-]+(?:\\.[0-9a-zA-Z-]+)*))?$'
    versvs <- stringr::str_extract(string = version, pattern = svs)
    if (!is.na(versvs)) {
      version <- versvs
    } else {
      version <- janitor::make_clean_names(version)
    }
  } else {
    version <- "*"
  }

  # Match vendor
  if (nchar(vendor) > 0 ) {
    if (product != janitor::make_clean_names(name)) {
      vendor <- names(sort(table(cpe.nist$vendor[cpe.nist$product == product]), T)[1])
    } else {
      vendor <- janitor::make_clean_names(vendor)
    }
  } else {
    if (product != janitor::make_clean_names(name)) {
      ven <- names(sort(table(cpe.nist$vendor[cpe.nist$product == product]), T)[1])
      if (!is.na(ven)) {
        vendor <- ven
      }
    } else {
      vendor <- "*"
    }
  }

  wfn <- newWFN(part = part, product = product, vendor = vendor, version = version)

  return(wfn)
}

newWFN <- function(part = "*", vendor = "*", product = "*", version = "*",
                   update = "*", edition = "*", language = "*", sw_edition = "*",
                   target_sw = "*", target_hw = "*", other = "*") {
  cpe <- data.frame(part = part,
                    vendor = vendor,
                    product = product,
                    version = version,
                    update = update,
                    edition = edition,
                    language = language,
                    sw_edition = sw_edition,
                    target_sw = target_sw,
                    target_hw = target_hw,
                    other = other)
  wfn <- paste("cpe", "2.3", paste(cpe, collapse = ":"), sep = ":")
  return(wfn)
}

# k <- apply(dplyr::sample_n(df, 10), 1,
#       function(x) {
#         print(x)
#         matchCPE(x["name"], x["version"], x["vendor"])
#       })
#
# df$cpe2 <- apply(df, 1,
#            function(x) {
#              print(x)
#              matchCPE(x["name"], x["version"], x["vendor"])
#            })
