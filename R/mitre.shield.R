#' Load SHIELD data frame from local file or download latest
#'
#' @param local_path path to RDS file. NA value implies remote TRUE
#' @param remote logical
#' @param verbose logical
#'
#' @return data.frame
shield_latest_data <- function(local_path = NA, remote = F, verbose = F) {
  if (is.na(local_path) | remote) {
    local_path <- tempfile(fileext = ".rds")
    download.file(url = "https://github.com/motherhack3r/mitre-datasets/raw/master/latest/simple/shield.rds",
                  destfile = local_path, quiet = !verbose)
  }
  shield <- readRDS(local_path)

  return(shield)
}

