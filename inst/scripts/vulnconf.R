
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


# LOAD CVE DATASET

mdata <- mitre::getLatestDataSet()
cves <- mdata$cve$cve.nist
rm(mdata)
# cves <- mitre::standards$cve$cve.nist

# PREPARE PARALLEL
parallel::detectCores()
n.cores <- parallel::detectCores() - 2
## create the cluster
my.cluster <- parallel::makeCluster(
  n.cores,
  type = "PSOCK"
)
## check cluster definition (optional)
print(my.cluster)
## register it to be used by %dopar%
doParallel::registerDoParallel(cl = my.cluster)
## check if it is registered (optional)
foreach::getDoParRegistered()
## how many workers are available? (optional)
foreach::getDoParWorkers()

library("doParallel")

vulnerableconfigurations <- foreach(i = 1:nrow(cves), .combine = "rbind") %dopar% {
  cve_flatten_vulnconf(cves$vulnerable.configuration[i],
                       cves$cve.id[i])
}

parallel::stopCluster(cl = my.cluster)

# numrows <- 1
# numconf <- 0
# vulnerableconfigurations <- data.frame()
# for (i in 1:nrow(cves)) {
#   vulnconf <- cve_flatten_vulnconf(cves$vulnerable.configuration[i],
#                                    cves$cve.id[i])
#   vulnerableconfigurations <<- dplyr::bind_rows(vulnerableconfigurations, vulnconf)
#   numrows <<- numrows + 1
#   numconf <<- numconf + nrow(vulnconf)
#   if (numrows %% 1000 == 0) cat(paste0("[-] ", numrows / 1000, "k processed cves --> ", numconf, " vulnerable configurations."), "\n")
# }


# i <- 38817
# vcs <- RJSONIO::fromJSON(cves$vulnerable.configuration[i])



# i <- 63
#
# vcs <- cves$vulnerable.configuration[i]
# vcs <- RJSONIO::fromJSON(vcs)
# j <<- 1
# kkk <- dplyr::bind_rows(lapply(vcs,
#               function(vc) {
#                 vc.op <- vc$operator
#                 if (vc.op == "AND") {
#                   vc_id <- paste("vulnconf", "AND", i, j, sep = ":")
#                   vc.cm <- lapply(vc$children,
#                                   function(x)
#                                     cve_flatten_cpematch_or(x$cpe_match, cves$cve.id[i]))
#                   vc.cm <- dplyr::bind_rows(vc.cm)
#                   vc.cm$vc_id <- rep(vc_id, nrow(vc.cm))
#
#                 } else if (vc.op == "OR") {
#                   vc_id <- paste("vulnconf", "OR", i, j, sep = ":")
#                   vc.cm <- cve_flatten_cpematch_or(vc$cpe_match, cves$cve.id[i])
#                   vc.cm$vc_id <- rep(vc_id, nrow(vc.cm))
#                 } else {
#                   vc.cm <- data.frame()
#                 }
#                 j <<- j + 1
#                 vc.cm
#               }))
#
# kk <- data.frame()
# for (j in 1:length(vcs)) {
#   vc <- vcs[[j]]
#   vc.op <- vc$operator
#   if (vc.op == "AND") {
#     vc_id <- paste("vulnconf", "AND", i, j, sep = ":")
#     vc.cm <- lapply(vc$children,
#                     function(x)
#                       cve_flatten_cpematch_or(x$cpe_match, cves$cve.id[i]))
#     vc.cm <- dplyr::bind_rows(vc.cm)
#     vc.cm$vc_id <- rep(vc_id, nrow(vc.cm))
#
#   } else if (vc.op == "OR") {
#     vc_id <- paste("vulnconf", "OR", i, j, sep = ":")
#     vc.cm <- cve_flatten_cpematch_or(vc$cpe_match, cves$cve.id[i])
#     vc.cm$vc_id <- rep(vc_id, nrow(vc.cm))
#   } else {
#     vc.cm <- data.frame()
#   }
#   kk <- dplyr::bind_rows(kk, vc.cm)
# }
#
#
# j <- 1
# vc <- vcs[[j]]
# vc.op <- vc$operator
# if (vc.op == "AND") {
#   vc_id <- paste("vulnconf", cve_id, "AND", j, sep = ":")
#   vc.cm <- lapply(vc$children,
#                   function(x) {
#                     y <- cve_flatten_cpematch_or(x$cpe_match, cves$cve.id[i], j)
#                     y
#                   })
#
#   vc.cm <- dplyr::bind_rows(vc.cm)
#   vc.cm$vc_id <- rep(vc_id, nrow(vc.cm))
#
# } else if (vc.op == "OR") {
#   vc_id <- paste("vulnconf", "OR", i, j, sep = ":")
#   vc.cm <- cve_flatten_cpematch_or(vc$cpe_match, cves$cve.id[i])
#   vc.cm$vc_id <- rep(vc_id, nrow(vc.cm))
# } else {
#
# }
#
#
# df.vc <- vc.cm
# df.vc$cve <- rep(vcv, nrow(vc.cm))
