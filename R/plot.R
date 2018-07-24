
select_pars <- function(x, cutoff_neff = 100, cutoff_rhat = 1.1) {
  x_neff <- x$summary[, "n.eff"] <= cutoff_neff
  x_rhat <- x$summary[, "Rhat"] >= cutoff_rhat

  return(names(which(x_neff | x_rhat)))
}


#' @title Trace plot of MCMC draws
#'
#' @description Trace plot of MCMC draws that did not converge following Neff & Rhat criteria
#'
#' @param x Object of class \code{bugs} or \code{pbugs}.
#' @param cutoff_neff Maximum threshold of effective sample size to select parameters.
#' @param cutoff_rhat Minimum threshold of Rhat to select parameters.
#' @param max_pars Maximum number of parameter to show. Default: 9.
#' @param ... Additional arguments to be passed to \code{\link[bayesplot]{mcmc_trace}} function.
#'
#' @usage  traceplot(x, cutoff_neff = 100, cutoff_rhat = 1.1, max_pars = 9, ...)
#'
#' @seealso \code{\link[bayesplot]{mcmc_trace}}
#'
#' @export
traceplot <- function(x, cutoff_neff = 100, cutoff_rhat = 1.1, max_pars = 9, ...) {
  stopifnot(any(c("pbugs", "bugs") %in% class(x)))
  pars       <- select_pars(x, cutoff_neff, cutoff_rhat)
  facet_args <- list(nrow = 3, ncol = 3)

  if (length(pars) == 0) {
    return(message("All parameters converged!\nIf you want to plot the traceplot,",
                   " you can use bayesplot::mcmc_trace(x$sims.array)"))
  } else if (length(pars) > max_pars) {
    warning(length(pars), " parameters did not converge, but 'max_pars' argument was set to ",
            max_pars, ".\nOnly the first ", max_pars, " are shown.\n",
            "If you want to plot all of them, modify 'max_pars' to ", length(pars),
            ".", call. = FALSE)
    bayesplot::mcmc_trace(x$sims.array, pars = pars[seq_len(max_pars)], facet_args = facet_args, ...)
  } else {
    n_plots  <- ceiling(length(pars) / 9)
    seq_ini  <- seq(1, 1e+6, 9)[seq_len(n_plots)]
    seq_last <- seq(9, 1e+6, 9)[seq_len(n_plots)]
    for (i in seq_len(n_plots)) {
      pars_tmp <- pars[seq_ini[i]:seq_last[i]]
      pars_tmp <- pars_tmp[!is.na(pars_tmp)]
      print(
        bayesplot::mcmc_trace(x$sims.array, pars = pars_tmp, facet_args = facet_args, ...)
      )
    }
  }
}

