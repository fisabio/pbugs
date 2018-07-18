
#' @title Run WinBUGS Chains in Parallel
#'
#' @description Call WinBUGS from your R session running multiple chains
#'   in parallel. Results are returned in an object of class \code{bugs} and
#'   \code{pbugs}.
#'
#' @details
#'   Mantainer: Miguel Ángel Martínez-Beneito \email{martinez_mig@@gva.es}
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
#' @description The data creation script is in /data-raw directory.
#'
#' @name sample_df
#'
#' @docType data
#'
#' @format A \code{data.frame} with 1000 observations and 4 variables:
#'   \describe{
#'     \item{x1}{Binomial with prob = 0.7.}
#'     \item{x2}{Binomial with prob = 0.4.}
#'     \item{x3}{Normal with mean 40 and sd 10 (mean centered).}
#'     \item{y}{Response.}
#'   }
#'
#' @keywords datasets
#'
#' @examples
#' data(sample_df)
"sample_df"
