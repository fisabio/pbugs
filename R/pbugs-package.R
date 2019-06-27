
#' @title Runs WinBUGS or OpenBUGS Models in Parallel
#'
#' @description Call WinBUGS or OpenBUGS from your R session running multiple
#'   chains in parallel. Results are returned in an object of class \code{bugs}
#'   and \code{pbugs}.
#'
#' @details
#'   Mantainer: Carlos Vergara-Hernández \email{vergara_car@@gva.es}
#'
#' @name pbugs-package
#'
#' @aliases pbugs-package
#'
#' @docType package
#'
#' @keywords package
#'
#' @encoding UTF-8
#'
"_PACKAGE"


#' @title Sample data for a logistic regression
#'
#' @description Simulated data for testing/illustrating purposes. This data set
#'   can be used to test the package with a logistic regression model, as
#'   illustrated in the help of the pbugs function.
#'
#' @details The data creation script is in the /data-raw directory.
#'
#' @name sample_df
#'
#' @docType data
#'
#' @format A \code{data.frame} with 1000 observations and 4 variables:
#'   \describe{ \item{x1}{Binomial with prob = 0.7.} \item{x2}{Binomial with
#'   prob = 0.4.} \item{x3}{Normal with mean 40 and sd 10 (mean centered).}
#'   \item{y}{Binomial with prob \code{plogis(tcrossprod(cbind(rep(1, N), x1,
#'   x2, x3), t(c(-2, 1.3, 1.05, .04))))}.} }
#'
#' @keywords datasets
#'
#' @examples
#' data(sample_df)
"sample_df"
