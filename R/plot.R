
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
#' @param parameter_name Literal name of the parameters to select.
#' @param parameter_pattern Regular expression (as used in \code{\link[base]{grep}}) for parameter selection.
#' @param only_not_converged Select only parameters that did not converged? Dafault = FALSE.
#' @param max_pars Maximum number of parameter to show. Default: 9.
#' @param ... Additional arguments to be passed to \code{\link[graphics]{plot}} function.
#'
#' @usage traceplot(x, parameter_name = character(), parameter_pattern = character(),
#' only_not_converged = FALSE,  max_pars = 9, ...)
#'
#' @export
traceplot <- function(x, parameter_name = character(), parameter_pattern = character(),
                      only_not_converged = FALSE, max_pars = 9, ...) {
  stopifnot(any(c("pbugs", "bugs") %in% class(x)))

  unique_pars <- dimnames(x$sims.array)[[3]]

  if (length(parameter_name) == 0 & length(parameter_pattern) == 0) {
    params <- unique_pars
  } else {
    params <- character()
    if (length(parameter_name) > 0) {
      if (!all(parameter_name %in% unique_pars)) {
        stop("Some parameters do not match parameter names: '",
             paste(parameter_name[which(!parameter_name %in% unique_pars)], collapse = "', '"), "'")
      }
      params <- sort(unique(c(params, parameter_name)))
    }

    if (length(parameter_pattern) > 0) {
      regex_params <- unlist(
        lapply(
          seq_along(parameter_pattern), function(x) {
            grep(parameter_pattern[x], unique_pars, value = TRUE, perl = TRUE)
          }
        )
      )
      params <- sort(unique(c(params, regex_params)))
    }
  }

  if (only_not_converged) {
    params <- select_pars(x, cutoff_neff = 100, cutoff_rhat = 1.1)
    if (length(params) == 0) {
      return(message("All parameters converged!\nTo see the traceplot",
                     " set the argument 'only_not_converged' to FALSE"))
    }
  }
  n_pars  <- length(params)


  if (n_pars > max_pars) {
    warning(n_pars, " parameters found, but 'max_pars' argument was set to ",
            max_pars, ".\nOnly the first ", max_pars, " are shown.\n",
            "If you want to plot all of them, modify 'max_pars' to ", n_pars,
            ".", call. = FALSE)
    oldpar <- graphics::par(
      mfrow = c(3, 3),
      oma   = c(0, 0, 2, 0),
      mar   = c(4, 3, 1, 1),
      mgp   = c(1.7, .5, 0)
    )
    on.exit(graphics::par(oldpar), add = TRUE)
    for (j in seq_len(max_pars)) {
      ylims <- range(x$sims.array[, ,  params[j]])
      graphics::plot(
        x    = x$sims.array[, 1, params[j]],
        col  = 1,
        type = "l",
        xlab = "Iteration",
        ylab = params[j],
        ylim = ylims,
        axes = FALSE#,
        # ...
      )
      graphics::axis(side = 1)
      graphics::axis(side = 2)
      for (k in seq_len(dim(x$sims.array)[2])) {
        graphics::lines(x$sims.array[, k,  params[j]], col = k + 1)
      }
    }
    tmp_par <- graphics::par(mfrow = c(1, 1), oma = rep(0, 4), mar = rep(0, 4), new = TRUE)
    graphics::plot(0, type = "n", xlab = "", ylab = "", axes = FALSE)
    graphics::legend(
      x      = "top",
      legend = paste("Chain", seq_len(dim(x$sims.array)[2])),
      col    = seq_len(dim(x$sims.array)[2]),
      lwd    = 3,
      cex    = 1,
      horiz  = TRUE,
      bty    = "n"
    )
    graphics::par(tmp_par)
  } else {
    oldpar <- graphics::par(
      mfrow = c(3, 3),
      oma   = c(0, 0, 2, 0),
      mar   = c(4, 3, 1, 1),
      mgp   = c(1.7, .5, 0)
    )
    on.exit(graphics::par(oldpar), add = TRUE)
    secuencia <- c(seq(0, n_pars * 2, 9)[-1], n_pars)
    for (j in seq_len(n_pars)) {
      ylims <- range(x$sims.array[, ,  params[j]])
      graphics::plot(
        x    = x$sims.array[, 1, params[j]],
        col  = 1,
        type = "l",
        xlab = "Iteration",
        ylab = params[j],
        ylim = ylims,
        axes = FALSE#,
        # ...
      )
      graphics::axis(side = 1)
      graphics::axis(side = 2)
      for (k in seq_len(dim(x$sims.array)[2])) {
        graphics::lines(x$sims.array[, k,  params[j]], col = k + 1)
      }
      if (j %in% secuencia) {
        tmp_par <- graphics::par(mfrow = c(1, 1), oma = rep(0, 4), mar = rep(0, 4), new = TRUE)
        graphics::plot(0, type = "n", xlab = "", ylab = "", axes = FALSE)
        graphics::legend(
          x      = "top",
          legend = paste("Chain", seq_len(dim(x$sims.array)[2])),
          col    = seq_len(dim(x$sims.array)[2]),
          lwd    = 3,
          cex    = 1,
          horiz  = TRUE,
          bty    = "n"
        )
        graphics::par(tmp_par)
      }
    }
    graphics::par(oldpar)
  }
}

