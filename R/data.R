#' MITRE ATT&CK framework data set
#'
#' A data set containing ATT&CK object models as data frames. It also contains
#' a graph network with the relations between objects.
#'
#' @format A list of 5 data frames and one visNetwork object:
#' \describe{
#'   \item{tactics}{a data frame with 28 rows and 13 columns.}
#'   \item{techniques}{a data frame with 611 rows and 16 columns.}
#'   \item{groups}{a data frame with 111 rows and 13 columns.}
#'   \item{software}{a data frame with 513 rows and 14 columns.}
#'   \item{mitigation}{a data frame with 55 rows and 14 columns.}
#'   \item{attcknet}{a visNetwork with 1310 nodes and 9539 edges.}
#' }
#' @source \url{https://github.com/mitre/cti}
"attck"

#' MITRE SHIELD framework data set
#'
#' A data set containing SHIELD object models as data frames. It also contains
#' a graph network with the relations between objects.
#'
#' @format A list of 5 data frames and one visNetwork object:
#' \describe{
#'   \item{tactics}{a data frame with 8 rows and 4 columns.}
#'   \item{techniques}{a data frame with 33 rows and 4 columns.}
#'   \item{opportunities}{a data frame with 79 rows and 2 columns.}
#'   \item{procedures}{a data frame with 66 rows and 2 columns.}
#'   \item{usecases}{a data frame with 189 rows and 2 columns.}
#'   \item{shieldnet}{a visNetwork with 375 nodes and 835 edges.}
#' }
#' @source \url{https://github.com/MITRECND/mitrecnd.github.io/tree/master/_data}
"shield"
