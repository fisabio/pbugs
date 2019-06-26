
select.pars <- function(x, cutoff.neff = 100, cutoff.rhat = 1.1) {
  x.neff <- x$summary[, "n.eff"] <= cutoff.neff
  x.rhat <- x$summary[, "Rhat"] >= cutoff.rhat

  return(names(which(x.neff | x.rhat)))
}


#' @title Trace plot of MCMC draws
#'
#' @description Trace plot of MCMC draws that did not converge following Neff & Rhat criteria
#'
#' @param x Object of class \code{bugs} or \code{pbugs}.
#' @param parameter.name Literal name of the parameters to select.
#' @param parameter.pattern Regular expression (as used in \code{\link[base]{grep}}) for parameter selection.
#' @param only.not.converged Select only parameters that did not converged? Dafault = FALSE.
#' @param max.pars Maximum number of parameter to show. Default: 9.
#' @param ... Additional arguments to be passed to \code{\link[graphics]{plot}} function.
#'
#' @usage traceplot(x, parameter.name = character(), parameter.pattern = character(),
#' only.not.converged = FALSE,  max.pars = 9, ...)
#'
#' @export
traceplot <- function(x, parameter.name = character(), parameter.pattern = character(),
                      only.not.converged = FALSE, max.pars = 9, ...) {
  stopifnot(any(c("pbugs", "bugs") %in% class(x)))

  unique.pars <- dimnames(x$sims.array)[[3]]

  if (length(parameter.name) == 0 & length(parameter.pattern) == 0) {
    params <- unique.pars
  } else {
    params <- character()
    if (length(parameter.name) > 0) {
      if (!all(parameter.name %in% unique.pars)) {
        stop("Some parameters do not match parameter names: '",
             paste(parameter.name[which(!parameter.name %in% unique.pars)], collapse = "', '"), "'")
      }
      params <- sort(unique(c(params, parameter.name)))
    }

    if (length(parameter.pattern) > 0) {
      regex.params <- unlist(
        lapply(
          seq_along(parameter.pattern), function(x) {
            grep(parameter.pattern[x], unique.pars, value = TRUE, perl = TRUE)
          }
        )
      )
      params <- sort(unique(c(params, regex.params)))
    }
  }

  if (only.not.converged) {
    params <- select.pars(x, cutoff.neff = 100, cutoff.rhat = 1.1)
    if (length(params) == 0) {
      return(message("All parameters converged!\nTo see the traceplot",
                     " set the argument 'only.not.converged' to FALSE"))
    }
  }
  n.pars  <- length(params)


  if (n.pars > max.pars) {
    warning(n.pars, " parameters found, but 'max.pars' argument was set to ",
            max.pars, ".\nOnly the first ", max.pars, " are shown.\n",
            "If you want to plot all of them, modify 'max.pars' to ", n.pars,
            ".", call. = FALSE)
    oldpar <- graphics::par(
      mfrow = c(3, 3),
      oma   = c(0, 0, 2, 0),
      mar   = c(4, 3, 1, 1),
      mgp   = c(1.7, .5, 0)
    )
    on.exit(graphics::par(oldpar), add = TRUE)
    for (j in seq_len(max.pars)) {
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
    tmp.par <- graphics::par(mfrow = c(1, 1), oma = rep(0, 4), mar = rep(0, 4), new = TRUE)
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
    graphics::par(tmp.par)
  } else {
    oldpar <- graphics::par(
      mfrow = c(3, 3),
      oma   = c(0, 0, 2, 0),
      mar   = c(4, 3, 1, 1),
      mgp   = c(1.7, .5, 0)
    )
    on.exit(graphics::par(oldpar), add = TRUE)
    secuencia <- c(seq(0, n.pars * 2, 9)[-1], n.pars)
    for (j in seq_len(n.pars)) {
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
      for (k in seq_len(dim(x$sims.array)[2])[-1]) {
        graphics::lines(x$sims.array[, k,  params[j]], col = k)
      }
      if (j %in% secuencia) {
        tmp.par <- graphics::par(mfrow = c(1, 1), oma = rep(0, 4), mar = rep(0, 4), new = TRUE)
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
        graphics::par(tmp.par)
      }
    }
    graphics::par(oldpar)
  }
}

