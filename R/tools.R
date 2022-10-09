#' Return data.frame with installed software name, version and vendor.
#'
#' @return data.frame
#' @export
#'
#' @examples
#' inventory <- getInventory()
getInventory <- function(){
  if (.Platform$OS.type == "windows") {
    # Windows with powershell equivalet to Microsoft SCCM inventory
    sw1 <- system("powershell.exe \"Get-ItemProperty HKLM:\\Software\\Wow6432Node\\Microsoft\\Windows\\CurrentVersion\\Uninstall\\* | Select-Object DisplayName, DisplayVersion, Publisher, InstallDate | Format-List\"", intern = T)
    sw2 <- system("powershell.exe \"Get-ItemProperty HKLM:\\SOFTWARE\\Microsoft\\Windows\\CurrentVersion\\Uninstall\\* | Select-Object DisplayName, DisplayVersion, Publisher, InstallDate | Format-List\"", intern = T)
    sw3 <- system("powershell.exe \"Get-WmiObject Win32_Product | Sort-Object Name | Format-List Name, Version, Vendor\"", intern = T)

    sw1 <- stringi::stri_conv(sw1, from = "CP850", to = "UTF-8")
    sw2 <- stringi::stri_conv(sw2, from = "CP850", to = "UTF-8")
    sw3 <- stringi::stri_conv(sw3, from = "CP850", to = "UTF-8")

    NewSWEntry <- function(name = "", version = "", vendor = "") {
      return(data.frame(name = name,
                        version = version,
                        vendor = vendor,
                        stringsAsFactors = F))}

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

    df.sw$product <- stringr::str_conv(df.sw$name, "UTF-8")
    df.sw$id <- 1:nrow(df.sw)
    df.sw <- df.sw[, c("id", "vendor", "product", "version")]

    return(df.sw)
  }

  if (.Platform$OS.type == "unix") {
    # Debian
    sw <- system("dpkg-query -W -f='${binary:Package}\\;${Architecture}\\;${Version}\\;${Maintainer}\\n'", intern = T)
    df.sw <- utils::read.csv(text = sw, sep = ";", header = F,
                      col.names = c("name", "architecture", "version", "mantainer"))
    df.sw$name <- sapply(df.sw$name, function(x) stringr::str_split(x, ":")[[1]][1])
    df.sw$product <- stringr::str_conv(df.sw$name, "UTF-8")
    df.sw$vendor <- df.sw$mantainer
    df.sw$id <- 1:nrow(df.sw)
    df.sw <- df.sw[, c("id", "vendor", "product", "version")]

    return(df.sw)
  }

  print("Only tested on Windows 10 and Debian. Sorry.")
  return(NA)
}


