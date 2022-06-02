source("data-raw/build_data.R")

rm(dsrc, dsrl)

dir.create(file.path(getwd(), "inst", "extdata", Sys.Date()))
file.copy(from = file.path(getwd(),"data"),
          to = file.path(getwd(), "inst", "extdata", Sys.Date()),
          recursive = T)
