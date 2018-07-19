
bugs.sims <- function(parameters.to.save, n.chains, n.iter, n.burnin,
                      n.thin, error_ch) {

  if (length(error_ch) == 0) {
    real_chains <- seq_len(n.chains)
  } else {
    if (length(error_ch) == n.chains) {
      stop("No chain ran properly.")
    }
    real_chains <- seq_len(n.chains)[-error_ch]
    n.chains    <- n.chains - length(error_ch)
  }
  sims.files      <- paste("coda", real_chains, ".txt", sep = "")
  index           <- data.table::fread(file = "codaIndex.txt")
  parameter.names <- index[[1]]
  n.keep          <- as.numeric(index[1, 3] - index[1, 2] + 1)
  n.parameters    <- length(parameter.names)
  n.sims          <- n.keep * n.chains
  sims            <- matrix(NA, n.sims, n.parameters)
  sims.array      <- array(NA, c(n.keep, n.chains, n.parameters))
  root.long       <- character(n.parameters)
  indexes.long    <- vector(n.parameters, mode = "list")
  for (i in seq_len(n.parameters)) {
    temp              <- decode.parameter.name(parameter.names[i])
    root.long[i]      <- temp$root
    indexes.long[[i]] <- temp$indexes
  }
  n.roots             <- length(parameters.to.save)
  left.bracket.short  <- as.vector(regexpr("[[]", parameters.to.save))
  right.bracket.short <- as.vector(regexpr("[]]", parameters.to.save))
  root.short <- ifelse(left.bracket.short == -1, parameters.to.save,
                       substring(parameters.to.save, 1, left.bracket.short - 1))
  dimension.short <- rep(0, n.roots)
  indexes.short   <- vector(n.roots, mode = "list")
  n.indexes.short <- vector(n.roots, mode = "list")
  long.short      <- vector(n.roots, mode = "list")
  length.short    <- numeric(n.roots)
  for (j in seq_len(n.roots)) {
    long.short[[j]] <- (seq_len(n.parameters))[root.long == root.short[j]]
    length.short[j] <- length(long.short[[j]])
    if (length.short[j] == 0)
      stop(paste("parameter", root.short[[j]], "is not in the model"))
    else if (length.short[j] > 1) {
      dimension.short[j]   <- length(indexes.long[[long.short[[j]][1]]])
      n.indexes.short[[j]] <- numeric(dimension.short[j])
      for (k in seq_len(dimension.short[j]))
        n.indexes.short[[j]][k] <- length(
          unique(unlist(lapply(indexes.long[long.short[[j]]], .subset, k)))
        )
      length.short[j]    <- prod(n.indexes.short[[j]])
      indexes.short[[j]] <- as.list(numeric(length.short[j]))
      for (k in seq_len(length.short[j]))
        indexes.short[[j]][[k]] <- indexes.long[[long.short[[j]][k]]]
    }
  }
  rank.long <- unlist(long.short)
  for (i in seq_len(n.chains)) {
    sims.i <- data.table::fread(file = sims.files[i])[[2]][seq_len(n.keep * n.parameters)]
    sims[(n.keep * (i - 1) + 1):(n.keep * i), ] <- sims.i
    sims.array[, i, ] <- sims.i
  }
  dimnames(sims)       <- list(NULL, parameter.names)
  dimnames(sims.array) <- list(NULL, NULL, parameter.names)
  summary              <- R2WinBUGS::monitor(sims.array, n.chains, keep.all = TRUE)
  last.values <- as.list(numeric(n.chains))
  for (i in seq_len(n.chains)) {
    n.roots.0               <- n.roots
    last.values[[i]]        <- as.list(numeric(n.roots.0))
    names(last.values[[i]]) <- root.short[1:n.roots.0]
    for (j in 1:n.roots.0) {
      if (dimension.short[j] <= 1) {
        last.values[[i]][[j]] <- sims.array[n.keep, i, long.short[[j]]]
        names(last.values[[i]][[j]]) <- NULL
      }
      else last.values[[i]][[j]] <- aperm(
        array(sims.array[n.keep, i, long.short[[j]]], rev(n.indexes.short[[j]])),
        dimension.short[j]:1
      )
    }
  }
  sims      <- sims[sample(n.sims), , drop = FALSE]
  sims.list <- summary.mean <- summary.sd <- summary.median <- vector(n.roots, mode = "list")
  names(sims.list) <- names(summary.mean) <- names(summary.sd) <- names(summary.median) <- root.short
  for (j in seq_len(n.roots)) {
    if (length.short[j] == 1) {
      sims.list[[j]]      <- sims[, long.short[[j]]]
      summary.mean[[j]]   <- summary[long.short[[j]], "mean"]
      summary.sd[[j]]     <- summary[long.short[[j]], "sd"]
      summary.median[[j]] <- summary[long.short[[j]], "50%"]
    } else {
      temp2               <- dimension.short[j]:1
      sims.list[[j]]      <- aperm(
        array(sims[, long.short[[j]]], c(n.sims, rev(n.indexes.short[[j]]))),
        c(1, (dimension.short[j] + 1):2)
      )
      summary.mean[[j]]   <- aperm(
        array(summary[long.short[[j]], "mean"], rev(n.indexes.short[[j]])),
        temp2
      )
      summary.sd[[j]]     <- aperm(
        array(summary[long.short[[j]], "sd"], rev(n.indexes.short[[j]])),
        temp2
      )
      summary.median[[j]] <- aperm(
        array(summary[long.short[[j]], "50%"], rev(n.indexes.short[[j]])),
        temp2
      )
    }
  }
  summary <- summary[rank.long, ]
  all     <- list(n.chains = n.chains, n.iter = n.iter, n.burnin = n.burnin,
                  n.thin = n.thin, n.keep = n.keep, n.sims = n.sims,
                  sims.array = sims.array[, , rank.long, drop = FALSE], sims.list = sims.list,
                  sims.matrix = sims[, rank.long], summary = summary, mean = summary.mean,
                  sd = summary.sd, median = summary.median, root.short = root.short,
                  long.short = long.short, dimension.short = dimension.short,
                  indexes.short = indexes.short, last.values = last.values)
  all     <- c(all, isDIC = FALSE)

  return(all)
}
