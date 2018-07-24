
#' @title Print pbugs objects
#'
#' @description S3 Method to print pbugs objects.
#'
#' @param x Object of class \code{pbugs}.
#' @param digits.summary Number of digits to print.
#' @param order_results Should summary results be ordered by Rhat or n.eff?
#' @param ... Further arguments to use in \code{print}.
#'
#' @export
print.pbugs <- function(x, digits.summary = 2, order_results = c("none", "Rhat", "n.eff"), ...) {
  order_results <- match.arg(order_results)
  if (!is.null(x$model.file))
    cat("Inference for Bugs model at \"", x$model.file, "\", ", sep = "")
  if (!is.null(x$program))
    cat("fit using ", x$program, ",", sep = "")
  cat("\n ", x$n.chains, " chains, each with ", x$n.iter, " iterations (first ",
      x$n.burnin, " discarded)", sep = "")
  if (x$n.thin > 1)
    cat(", n.thin =", x$n.thin)
  cat("\n n.sims =", x$n.sims, "iterations saved\n")
  cat("Using ", x$n_cores, " cores, the simulation time was ", round(as.numeric(x$exec_time), 2),
      " ", attr(x$exec_time, "units"), ".\n", sep = "")
  if (!is.null(x$seed))
    cat("\nReproducible seed:", x$seed, "\n")
  if (order_results == "n.eff") {
    print(round(x$summary[order(x$summary[, "n.eff"]), ], digits.summary), ...)
  } else if (order_results == "Rhat") {
    print(round(x$summary[order(x$summary[, "Rhat"], decreasing = TRUE), ], digits.summary), ...)
  } else {
    print(round(x$summary, digits.summary), ...)
  }
  if (x$n.chains > 1) {
    cat("\nFor each parameter, n.eff is a crude measure of effective sample size,")
    cat("\nand Rhat is the potential scale reduction factor (at convergence, Rhat=1).\n")
  }
  if (x$isDIC) {
    msgDICRule <- ifelse(x$DICbyR, "(using the rule, pD = var(deviance)/2)",
                         "(using the rule, pD = Dbar-Dhat)")
    cat(paste("\nDIC info ", msgDICRule, "\n", sep = ""))
    if (length(x$DIC) == 1) {
      cat("pD =", fround(x$pD, 1), "and DIC =", fround(x$DIC,
                                                       1))
    } else if (length(x$DIC) > 1) {
      print(round(x$DIC, 1))
    }
    cat("\nDIC is an estimate of expected predictive error (lower deviance is better).\n")
  }
  return(invisible(x))
}


#' @export
summary.pbugs <- function(object, digits.summary = 2, order_results = c("none", "Rhat", "n.eff"), ...) {
  order_results <- match.arg(order_results)
  if (!is.null(object$model.file))
    cat("Inference for Bugs model at \"", object$model.file, "\", ", sep = "")
  if (!is.null(object$program))
    cat("fit using ", object$program, ",", sep = "")
  cat("\n ", object$n.chains, " chains, each with ", object$n.iter, " iterations (first ",
      object$n.burnin, " discarded)", sep = "")
  if (object$n.thin > 1)
    cat(", n.thin =", object$n.thin)
  cat("\n n.sims =", object$n.sims, "iterations saved\n")
  cat("Using ", object$n_cores, " cores, the simulation time was ", round(as.numeric(object$exec_time), 2),
      " ", attr(object$exec_time, "units"), ".\n", sep = "")
  if (!is.null(object$seed))
    cat("\nReproducible seed:", object$seed, "\n")
  if (order_results == "n.eff") {
    print(round(object$summary[order(object$summary[, "n.eff"]), ], digits.summary), ...)
  } else if (order_results == "Rhat") {
    print(round(object$summary[order(object$summary[, "Rhat"], decreasing = TRUE), ], digits.summary), ...)
  } else {
    print(round(object$summary, digits.summary), ...)
  }
  if (object$n.chains > 1) {
    cat("\nFor each parameter, n.eff is a crude measure of effective sample size,")
    cat("\nand Rhat is the potential scale reduction factor (at convergence, Rhat=1).\n")
  }
  if (object$isDIC) {
    msgDICRule <- ifelse(object$DICbyR, "(using the rule, pD = var(deviance)/2)",
                         "(using the rule, pD = Dbar-Dhat)")
    cat(paste("\nDIC info ", msgDICRule, "\n", sep = ""))
    if (length(object$DIC) == 1) {
      cat("pD =", fround(object$pD, 1), "and DIC =", fround(object$DIC, 1))
    } else if (length(object$DIC) > 1) {
      print(round(object$DIC, 1))
    }
    cat("\nDIC is an estimate of expected predictive error (lower deviance is better).\n")
  }
  return(invisible(object))
}


