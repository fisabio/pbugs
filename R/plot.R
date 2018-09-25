
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
#' @param mfrow Number of rows and columns in the plot.
#' @param ... Additional arguments to be passed to \code{\link[coda]{traceplot}} function.
#'
#' @usage  traceplot(x, cutoff_neff = 100, cutoff_rhat = 1.1, max_pars = 9, mfrow = c(3, 3), ...)
#'
#' @seealso \code{\link[bayesplot]{mcmc_trace}}
#'
#' @export
traceplot <- function(x, cutoff_neff = 100, cutoff_rhat = 1.1, max_pars = 9, mfrow = c(3, 3), ...) {
  stopifnot(any(c("pbugs", "bugs") %in% class(x)))
  pars   <- select_pars(x, cutoff_neff, cutoff_rhat)
  oldpar <- graphics::par(mfrow = mfrow)
  if (length(pars) == 0) {
    return(message("All parameters converged!\nIf you want to plot the traceplot,",
                   " you can use coda::traceplot(coda::as.mcmc.list(x))"))
  } else if (length(pars) > max_pars) {
    warning(length(pars), " parameters did not converge, but 'max_pars' argument was set to ",
            max_pars, ".\nOnly the first ", max_pars, " are shown.\n",
            "If you want to plot all of them, modify 'max_pars' to ", length(pars),
            ".", call. = FALSE)
    coda::traceplot(lapply(coda::as.mcmc.list(x), function(sim) sim[, pars[seq_len(max_pars)], drop = FALSE]), ...)
  } else {
    n_plots  <- ceiling(length(pars) / 9)
    seq_ini  <- seq(1, 1e+6, 9)[seq_len(n_plots)]
    seq_last <- seq(9, 1e+6, 9)[seq_len(n_plots)]
    for (i in seq_len(n_plots)) {
      pars_tmp <- pars[seq_ini[i]:seq_last[i]]
      pars_tmp <- pars_tmp[!is.na(pars_tmp)]
      coda::traceplot(lapply(coda::as.mcmc.list(x), function(sim) sim[, pars_tmp, drop = FALSE]), ...)
    }
  }
  on.exit(graphics::par(oldpar))
}

