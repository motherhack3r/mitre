NULL

#' Common Platform Enumeration.
#'
#' Full data set provided by NIST.
#'
#' @format A data frame with 16 variables: \code{title}, \code{cpe.22},
#'   \code{cpe.23}, and all separated values.
"cpe.nist"

#' Common Vulnerability Enumeration.
#'
#' Full data set provided by NIST.
#'
#' @format A data frame with 34 variables: \code{cve.id}, \code{problem.type} which is related to CWE,
#'   \code{description}, \code{vulnerable.configuration} which is related to CPE,
#'   \code{references}, \code{cvss3}, \code{cvss2} and all separated values.
"cve.nist"
