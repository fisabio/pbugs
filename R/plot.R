
select.pars <- function(x, cutoff.neff = 100, cutoff.rhat = 1.1) {
  x.neff <- x$summary[, "n.eff"] <= cutoff.neff
  x.rhat <- x$summary[, "Rhat"] >= cutoff.rhat

  return(names(which(x.neff | x.rhat)))
}


#' @title History Plot of Selected Variables in the Model
#'
#' @description History plot for some variables in a \code{bugs} or \code{pbugs}
#'   object. This function allows also automatically plotting those variables
#'   with deficient converge according to the effective sample size (Neff) &
#'   Brooks-Gelman-Rubin (Rhat) criteria.
#'
#' @details If \code{length(var.names) == 0 & length(var.pattern) == 0 &
#'   !not.converged}, history plots will be plotted for all the variables in the
#'   corresponding \code{bugs} or \code{pbugs} object.
#'
#' @param x Object of class \code{bugs} or \code{pbugs}.
#' @param var.pattern Character. Regular expression (as used in
#'   \code{\link[base]{grep}}) for selecting the variables to be plotted.
#' @param var.names Character. Vector with the literal names of the variables to
#'   be plotted.
#' @param not.converged Logical, default: FALSE. Plot just those variables with
#'   deficient convergence?
#' @param convergence.criteria Numeric vector of length 2 with thresholds for
#'   the Neff and Rhat criteria. If \code{not.converged} is set to TRUE, all
#'   those variables with Neff below or Rhat above the set thresholds will be
#'   plotted. The default thresholds are fixed to 100 for Neff and 1.1 for the
#'   Rhat criteria.
#' @param mfrow A vector of length 2, specifying the number of rows and columns
#'   on each device. Default NULL (tries to accommodate the number of parameters
#'   using mfrow values from c(1, 1) to c(4, 4)).
#' @param max.devices Maximum number of devices to generate containing the
#'   requested history plots. Equal to 10 by default.
#' @param ... Additional arguments to be passed to \code{\link[graphics]{plot}}
#'   function.
#'
#' @usage traceplot(x, var.pattern = character(), var.names = character(),
#'   not.converged = FALSE, convergence.criteria = c(100, 1.1), mfrow = NULL,
#'   max.devices = 10, ...)
#'
#'
#' @examples
#'   \dontrun{
#'     library(pbugs)
#'     data(sample_df)
#'     bugs_model <- function() {
#'       for (i in 1:N) {
#'         y[i] ~ dbern(pi[i])
#'         logit(pi[i]) <- beta[1] + beta[2] * x1[i] + beta[3] * x2[i] + beta[4] * x3[i]
#'       }
#'       for (j in 1:4) {
#'         beta[j] ~ dflat()
#'       }
#'     }
#'     bugs_data <- with(
#'       sample_df,
#'       list(y = y, x1 = x1, x2 = x2, x3 = x3, N = length(y))
#'     )
#'     bugs_init <- function() list(beta = rnorm(4, sd = .5))
#'     bugs_pars <- c("beta", "pi")
    # result    <- pbugs(
    #   data               = bugs_data,
    #   inits              = bugs_init,
    #   parameters.to.save = bugs_pars,
    #   model.file         = bugs_model,
    #   n.thin             = 1,
    #   n.chains           = 4
    # )
#'     traceplot(result, var.names = c("beta[1]", "beta[3]"))
#'     traceplot(result, var.pattern = "beta")
#'     traceplot(result, var.pattern = "beta|pi\\[1\\d?\\]")
#'   }
#'
#' @export
traceplot <- function(x, var.pattern = character(), var.names = character(),
                      not.converged = FALSE, convergence.criteria = c(100, 1.1),
                      mfrow = NULL, max.devices = 10, ...) {
  stopifnot(any(c("pbugs", "bugs") %in% class(x)))
  stopifnot(length(convergence.criteria) == 2 & is.numeric(convergence.criteria))
  stopifnot(is.null(mfrow) | length(mfrow) == 2 & (is.numeric(mfrow) | is.integer(mfrow)))
  stopifnot(length(max.devices) == 1 & (is.numeric(max.devices) | is.integer(max.devices)))
  stopifnot(!is.null(var.names))
  stopifnot(is.character(var.names))
  stopifnot(is.character(var.pattern))
  stopifnot(is.logical(not.converged))
  unique.pars <- dimnames(x$sims.array)[[3]]

  if (length(var.names) == 0 & length(var.pattern) == 0) {
    params <- unique.pars
  } else {
    params <- character()
    if (length(var.names) > 0) {
      if (!all(var.names %in% unique.pars)) {
        stop("Some parameters do not match parameter names: '",
             paste(var.names[which(!var.names %in% unique.pars)], collapse = "', '"), "'")
      }
      params <- sort(unique(c(params, var.names)))
    }

    if (length(var.pattern) > 0) {
      regex.params <- unlist(
        lapply(
          seq_along(var.pattern), function(x) {
            grep(var.pattern[x], unique.pars, value = TRUE, perl = TRUE)
          }
        )
      )
      params <- sort(unique(c(params, regex.params)))
    }
  }
  if (not.converged) {
    aux <- select.pars(
      x           = x,
      cutoff.neff = convergence.criteria[1],
      cutoff.rhat = convergence.criteria[2]
    )
    aux <- aux[aux %in% params]
    if (length(aux) == 0) {
      message("All parameters converged!\n")
      return()
    } else {
      mi_sel <- params %in% aux
      params <- params[mi_sel]

      if (length(aux) == length(mi_sel)) {
        message("No parameter among those selected has converged.\n")
      } else {
        message(
          paste(
            sum(mi_sel),
            "of the",
            length(mi_sel),
            "selected parameters have not converged.\n"
          )
        )
      }
    }
  }

  n.pars   <- length(params)
  if (is.null(mfrow)) {
    mfrow <- if (n.pars < 10) {
      c(1, 1)
    } else if (n.pars < 20) {
      c(1, 2)
    } else if (n.pars < 30) {
      c(1, 3)
    } else if (n.pars < 40) {
      c(1, 4)
    } else if (n.pars < 80) {
      c(2, 4)
    } else if (n.pars < 120) {
      c(3, 4)
    } else c(4, 4)
  }

  max.pars <- mfrow[1] * mfrow[2] * max.devices

  if (n.pars > max.pars) {
    warning(
      n.pars,
      " parameters to plot, but with the current configuration\n",
      " ('mfrow' & 'max.devices') only the first ",
      max.pars,
      " will be plotted.\n",
      call. = FALSE
    )
    n.pars <- max.pars
  }

  new_devices <- seq(1, by = mfrow[1] * mfrow[2], length.out = max.devices)
  for (j in seq_len(n.pars)) {
    if (j %in% new_devices) {
      grDevices::X11()
      oldpar <- graphics::par(
        mfrow = mfrow,
        oma   = c(0, 0, 2, 0),
        mar   = c(4, 3, 1, 1),
        mgp   = c(1.7, .5, 0)
      )
    }
    ylims <- range(x$sims.array[, ,  params[j]])
    graphics::plot(
      x    = x$sims.array[, 1, params[j]],
      col  = 1,
      type = "l",
      xlab = "Iteration",
      ylab = params[j],
      ylim = ylims,
      axes = FALSE,
      ...
    )
    graphics::axis(side = 1)
    graphics::axis(side = 2)
    for (k in seq_len(dim(x$sims.array)[2])) {
      graphics::lines(x$sims.array[, k,  params[j]], col = k + 1)
    }
  }
}
