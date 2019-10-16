
findUnixBinary <- function(x) {
  # Function from the 'R2WinBUGS' package

  tmp <- Sys.getenv(toupper(x))
  if (nchar(tmp) != 0 && file.exists(tmp))
    return(tmp)
  tmp <- paste("/usr/bin", x, sep = "")
  if (file.exists(tmp))
    return(tmp)
  tmp <- system(paste("which ", x, sep = ""), intern = TRUE)
  if (length(tmp) != 0 && file.exists(tmp))
    return(tmp)
  tmp <- system(paste("locate ", x, " | grep bin/", x, "$",
                      sep = ""), intern = TRUE)
  tmp <- tmp[length(tmp)]
  if (length(tmp) > 0 && file.exists(tmp))
    return(tmp)
  stop(paste("couldn't find", x, "binary file"))
}


bugs.log <- function(file) {
  # Function from the 'R2WinBUGS' package

  if (!file.exists(file))
    stop("Log file", file, "does not exist")
  log.txt <- readLines(file, warn = FALSE)
  extract <- function(m, line.match, skip = 0, empty.left.col = TRUE) {
    start <- (skip + which(m == line.match)[1])
    if (is.na(start))
      return(NA)
    if (length(start) < 1)
      return(NA)
    mx <- strsplit(m[-(1:start)], "\t")
    n.cols <- length(mx[[1]])
    if (n.cols < 1)
      return(NA)
    mxlen <- sapply(mx, length)
    end <- which(mxlen != n.cols)[1] - 1
    mx <- mx[1:end]
    cm <- matrix(unlist(mx), ncol = n.cols, byrow = TRUE)
    if (empty.left.col)
      cm <- cm[, -1]
    col.names <- cm[1, -1]
    row.names <- cm[, 1][-1]
    col.names <- gsub("[[:space:]]+", "", col.names)
    cm <- cm[-1, -1]
    m <- matrix(as.numeric(cm), nrow = nrow(cm))
    dimnames(m) <- list(row.names, col.names)
    return(m)
  }
  stats <- extract(log.txt, "Node statistics")
  DIC <- extract(log.txt, "DIC", skip = 1, empty.left.col = FALSE)
  list(stats = stats, DIC = DIC)
}


bugs.inits <- function(inits, n.chains, digits, inits.files = paste("inits", seq_len(n.chains), ".txt", sep = "")) {
  # Function from the 'R2WinBUGS' package

  if (!is.null(inits)) {
    for (i in seq_len(n.chains)) {
      if (is.function(inits))
        write.datafile(lapply(inits(), formatC, digits = digits, format = "E"), inits.files[i])
       else
        write.datafile(lapply(inits[[i]], formatC, digits = digits, format = "E"), inits.files[i])
    }
  }
  return(inits.files)
}


write.datafile <- function(datalist, towhere, fill = TRUE) {
  # Function from the 'R2WinBUGS' package

  if (!is.list(datalist) || is.data.frame(datalist))
    stop("First argument to write.datafile must be a list.")
  cat(formatdata(datalist), file = towhere, fill = fill)
}


formatdata <- function(datalist) {
  # Function from the 'R2WinBUGS' package

  if (!is.list(datalist) || is.data.frame(datalist))
    stop("Argument to formatdata() must be a list.")
  n <- length(datalist)
  datalist.string <- vector(n, mode = "list")
  for (i in 1:n) {
    if (length(datalist[[i]]) == 1)
      datalist.string[[i]] <- paste(names(datalist)[i], "=", as.character(datalist[[i]]), sep = "")
    if (is.vector(datalist[[i]]) && length(datalist[[i]]) > 1)
      datalist.string[[i]] <- paste(
        names(datalist)[i], "=c(", paste(as.character(datalist[[i]]), collapse = ", "), ")", sep = "")
    if (is.array(datalist[[i]]))
      datalist.string[[i]] <- paste(
        names(datalist)[i], "= structure(.Data= c(",
        paste(as.character(as.vector(aperm(datalist[[i]]))), collapse = ", "),
        "), .Dim=c(", paste(as.character(dim(datalist[[i]])), collapse = ", "), "))", sep = ""
      )
  }
  datalist.tofile <- paste("list(", paste(unlist(datalist.string), collapse = ", "), ")", sep = "")
  datalist.tofile
}


winbugs.script <- function(parameters.to.save, n.chains, n.iter, n.burnin, n.thin,
                        model.file, debug = FALSE, is.inits, bin, DIC = FALSE,
                        useWINE = .Platform$OS.type != "windows", newWINE = TRUE,
                        WINEPATH = NULL, bugs.seed = NULL, summary.only = FALSE,
                        save.history = TRUE, bugs.data.file, bugs.inits.files,
                        over.relax = FALSE) {
  # Function from the 'R2WinBUGS' package

  if ((ceiling(n.iter/n.thin) - ceiling(n.burnin/n.thin)) < 2)
    stop("(n.iter-n.burnin)/n.thin must be at least 2")
  working.directory <- getwd()
  script <- "script.txt"
  model <- if (length(grep("\\\\", model.file)) || length(grep("/", model.file))) {
    gsub("\\\\", "/", model.file)
  } else file.path(working.directory, model.file)
  model <- native2win(model, useWINE = useWINE, newWINE = newWINE, WINEPATH = WINEPATH)
  history <- file.path(working.directory, "history.odc")
  history <- native2win(history, useWINE = useWINE, newWINE = newWINE, WINEPATH = WINEPATH)
  data <- file.path(working.directory, bugs.data.file)
  data <- native2win(data, useWINE = useWINE, newWINE = newWINE, WINEPATH = WINEPATH)
  coda <- file.path(working.directory, "coda")
  coda <- native2win(coda, useWINE = useWINE, newWINE = newWINE, WINEPATH = WINEPATH)
  logFile <- file.path(working.directory, "log.odc")
  logFile <- native2win(logFile, useWINE = useWINE, newWINE = newWINE, WINEPATH = WINEPATH)
  logFileTxt <- file.path(working.directory, "log.txt")
  logFileTxt <- native2win(logFileTxt, useWINE = useWINE, newWINE = newWINE, WINEPATH = WINEPATH)
  inits <- paste(working.directory, "/", bugs.inits.files, sep = "")
  inits <- sapply(inits, useWINE = useWINE, newWINE = newWINE,
                  WINEPATH = WINEPATH, function(x, useWINE, newWINE, WINEPATH) {
                    native2win(x, useWINE = useWINE, newWINE = newWINE,
                               WINEPATH = WINEPATH)
                  })
  initlist <- paste("inits (", seq_len(n.chains), ", '", inits, "')\n", sep = "")
  savelist <- paste("set (", parameters.to.save, ")\n", sep = "")
  redo <- ceiling((n.iter - n.burnin)/(n.thin * bin))
  bugs.seed.cmd <- ""
  if (!is.null(bugs.seed)) {
    bugs.seed.cmd <- paste("set.seed(", bugs.seed, ")\n", sep = "")
  }
  thinUpdate <- paste("thin.updater (", n.thin, ")\n", "update (",
                      ceiling(n.burnin/n.thin), ")\n", sep = "")
  cat("display ('log')\n", "check ('", model, "')\n", "data ('",
      data, "')\n", bugs.seed.cmd, "compile (", n.chains, ")\n",
      if (is.inits)
        initlist, "gen.inits()\n", if (over.relax)
          "over.relax(\"yes\")\n", thinUpdate, savelist, if (DIC)
            "dic.set()\n", rep(c("update (", formatC(ceiling(bin), format = "d"),
                                 ")\n", c("coda (*, '", coda, "')\n")), redo),
      "stats (*)\n", if (DIC) "dic.stats()\n", if (save.history) c("history (*, '",
                                                                   history, "')\n"),
      "save ('", logFile, "')\n", "save ('", logFileTxt, "')\n", file = script,
      sep = "", append = FALSE)
  if (!debug)
    cat("quit ()\n", file = script, append = TRUE)
  sims.files <- paste("coda", seq_len(n.chains), ".txt", sep = "")
  for (i in seq_len(n.chains)) cat("WinBUGS did not run correctly.\n",
                            file = sims.files[i], append = FALSE)
}


bugs.update.settings <- function(n.burnin, bugs.directory) {
  # Function from the 'R2WinBUGS' package

  .fileCopy <- file.copy
  .regexpr <- function(...) regexpr(..., useBytes = TRUE)
  .sub <- function(...) sub(..., useBytes = TRUE)
  .writeBin <- if (getRversion() >= "2.10") {
    function(...) writeBin(..., useBytes = TRUE)
  } else writeBin
  char.burnin <- as.character(n.burnin - 1)
  registry <- readBin(file.path(bugs.directory, "System/Rsrc/Registry.odc"),
                      "character", 400, size = 1, endian = "little")
  locale <- Sys.getlocale("LC_CTYPE")
  Sys.setlocale("LC_CTYPE", "C")
  .fileCopy(file.path(bugs.directory, "System/Rsrc/Registry.odc"),
            file.path(bugs.directory, "System/Rsrc/Registry_Rsave.odc"),
            overwrite = TRUE)
  info <- registry[.regexpr("Int", registry, fixed = TRUE) > 0]
  while (.regexpr("\r", info) > 0) {
    newline <- .regexpr("\r", info)
    info <- substring(info, newline + 1)
    line <- substring(info, 1, .regexpr("\r", info) - 1)
    if (.regexpr("AdaptivePhase", line) > 0) {
      numpos <- .regexpr("Int", line, fixed = TRUE) + 4
      num <- substring(line, numpos)
      if (as.numeric(num) > n.burnin) {
        blanks <- rep(" ", nchar(num, type = "chars") -
                        nchar(char.burnin, type = "chars"))
        num.new <- paste(paste(blanks, collapse = ""),
                         char.burnin, sep = "")
        line.new <- .sub(num, num.new, line)
        registry <- .sub(line, line.new, registry)
      }
    }
  }
  Sys.setlocale("LC_CTYPE", locale)
  .writeBin(registry, file.path(bugs.directory, "System/Rsrc/Registry.odc"),
            endian = "little")
}


decode.parameter.name <- function(a) {
  # Function from the 'R2WinBUGS' package

  left.bracket <- regexpr("[[]", a)
  if (left.bracket == -1) {
    root <- a
    dimension <- 0
    indexes <- NA
  } else {
    root <- substring(a, 1, left.bracket - 1)
    right.bracket <- regexpr("[]]", a)
    a <- substring(a, left.bracket + 1, right.bracket - 1)
    indexes <- as.numeric(unlist(strsplit(a, ",")))
    dimension <- length(indexes)
  }
  list(root = root, dimension = dimension, indexes = indexes)
}


native2win <- function(x, useWINE = .Platform$OS.type != "windows", newWINE = TRUE, WINEPATH = NULL) {
  # Function from the 'R2WinBUGS' package

  if (useWINE) {
    if (newWINE) {
      if (is.null(WINEPATH))
        WINEPATH <- findUnixBinary(x = "winepath")
      x <- system(paste(WINEPATH, "-w", x), intern = TRUE)
      gsub("\\\\", "/", x)
    } else {
      winedriveRTr(x)
    }
  } else {
    x
  }
}


win2native <- function(x, useWINE = .Platform$OS.type != "windows", newWINE = TRUE, WINEPATH = NULL) {
  # Function from the 'R2WinBUGS' package

  if (useWINE) {
    if (newWINE) {
      if (is.null(WINEPATH))
        WINEPATH <- findUnixBinary(x = "winepath")
      system(paste(WINEPATH, " \"", x, "\"", sep = ""),
             intern = TRUE)
    }
    else {
      winedriveTr(x)
    }
  }
  else {
    x
  }
}


winedriveTr <- function(windir, DriveTable = winedriveMap()) {
  # Function from the 'R2WinBUGS' package

  win.dr <- substr(windir, 1, 2)
  ind <- pmatch(toupper(win.dr), DriveTable$drive)
  native.dr <- DriveTable$path[ind]
  sub(win.dr, native.dr, windir)
}


winedriveRTr <- function(unixpath, DriveTable = winedriveMap()) {
  # Function from the 'R2WinBUGS' package

  blocks <- strsplit(unixpath, "/")[[1]]
  cblocks <- c(
    "/",
    sapply(1 + seq(along = blocks[-1]), function(n) paste(blocks[1:n], collapse = "/"))
  )
  path <- match(cblocks, DriveTable$path)
  if (any(!is.na(path))) {
    unixdir <- cblocks[which.min(path)]
    windrive <- paste(DriveTable$drive[min(path, na.rm = TRUE)], "/", sep = "")
    winpath <- sub("//", "/", sub(unixdir, windrive, unixpath))
  }
  else {
    stop("can't find equivalent Windows path: file may be inaccessible")
  }
  winpath
}


winedriveMap <- function(config = "~/.wine/config") {
  # Function from the 'R2WinBUGS' package

  if (!file.exists(config))
    return(NULL)
  con <- readLines(config)
  con <- con[-grep("^;", con)]
  drive <- con[grep("^\\[Drive ", con)]
  drive <- substr(drive, 8, 8)
  drive <- paste(drive, ":", sep = "")
  path <- con[grep("Path", con)]
  len <- length(drive)
  path <- path[1:len]
  dir <- sapply(path, function(x) {
    foo <- unlist(strsplit(x, "\""))
    foo[length(foo)]
  })
  dir <- sub("%HOME%", tools::file_path_as_absolute("~"), dir)
  data.frame(drive = I(drive), path = I(dir), row.names = NULL)
}


fround <- function(x, digits) {
  # Function from the 'R2WinBUGS' package

  format(round(x, digits), nsmall = digits)
}


conv.par <- function(x, n.chains, Rupper.keep = TRUE) {
  # Function from the 'R2WinBUGS' package

  m         <- ncol(x)
  n         <- nrow(x)
  xdot      <- apply(x, 2, mean)
  muhat     <- mean(xdot)
  s2        <- apply(x, 2, stats::var)
  W         <- mean(s2)
  quantiles <- stats::quantile(as.vector(x), probs = c(0.025, 0.25, 0.5, 0.75, 0.975))

  if ((W > 1e-08) && (n.chains > 1)) {
    B          <- n * stats::var(xdot)
    varW       <- stats::var(s2) / m
    varB       <- B^2 * 2 / (m - 1)
    covWB      <- (n/m) * (stats::var(s2, xdot^2) - 2 * muhat * stats::var(s2, xdot))
    sig2hat    <- ((n - 1) * W + B)/n
    postvar    <- sig2hat + B/(m * n)
    varpostvar <- max(
      0,
      (((n - 1)^2) * varW + (1 + 1 / m)^2 * varB + 2 * (n - 1) * (1 + 1 / m) * covWB) / n^2
    )

    post.df          <- min(2 * (postvar^2 / varpostvar), 1000)
    confshrink.range <- postvar / W

    if (Rupper.keep) {
      varlo.df <- 2 * (W^2 / varW)
      confshrink.range <- c(
        confshrink.range,
        (n - 1) / n + (1 + 1 / m) * (1 / n) * (B / W) * stats::qf(0.975, m - 1, varlo.df)
      )
    }
    confshrink.range <- sqrt(confshrink.range * (post.df + 3) / (post.df + 1))
    n.eff            <- m * n * min(sig2hat / B, 1)

    return(list(quantiles = quantiles, confshrink = confshrink.range, n.eff = n.eff))
  } else {
    return(list(quantiles = quantiles, confshrink = rep(1, Rupper.keep + 1), n.eff = 1))
  }
}


monitor <- function(a, n.chains = dim(a)[2], trans = NULL, keep.all = FALSE, Rupper.keep = FALSE) {
  # Function from the 'R2WinBUGS' package

  invlogit <- function(x) 1 / (1 + exp(-x))
  logit    <- function(x) log(x/(1 - x))

  nparams  <- if (length(dim(a)) < 3) 1 else dim(a)[length(dim(a))]
  output   <- matrix(
    data = NA,
    ncol = if (n.chains > 1) {if (Rupper.keep) 10 else 9} else 7,
    nrow = nparams
  )
  if (length(dim(a)) == 2)
    a <- array(a, c(dim(a), 1))
  if (!keep.all) {
    n <- floor(dim(a)[1]/2)
    a <- a[(n + 1):(2 * n), , , drop = FALSE]
  }
  if (is.null(trans))
    trans <- ifelse((apply(a <= 0, 3, sum)) == 0, "log", "")
  for (i in 1:nparams) {
    ai <- a[, , i, drop = FALSE]
    if (trans[i] == "log") {
      conv.p <- conv.par(log(ai), n.chains, Rupper.keep = Rupper.keep)
      conv.p <- list(quantiles = exp(conv.p$quantiles),
                     confshrink = conv.p$confshrink, n.eff = conv.p$n.eff)
    } else if (trans[i] == "logit") {
      conv.p <- conv.par(logit(ai), n.chains, Rupper.keep = Rupper.keep)
      conv.p <- list(quantiles = invlogit(conv.p$quantiles),
                     confshrink = conv.p$confshrink, n.eff = conv.p$n.eff)
    } else {
      conv.p <- conv.par(ai, n.chains, Rupper.keep = Rupper.keep)
    }
    output[i, ] <- c(
      mean(ai),
      stats::sd(as.vector(ai)),
      conv.p$quantiles,
      if (n.chains > 1) conv.p$confshrink,
      if (n.chains > 1) round(conv.p$n.eff, min(0, 1 - floor(log10(conv.p$n.eff))))
    )
  }
  if (n.chains > 1) {
    dimnames(output) <- list(
      dimnames(a)[[3]],
      c("mean", "sd", "2.5%", "25%", "50%", "75%", "97.5%", "Rhat", if (Rupper.keep) "Rupper", "n.eff")
    )
  } else {
    dimnames(output) <- list(
      dimnames(a)[[3]],
      c("mean", "sd", "2.5%", "25%", "50%", "75%", "97.5%")
    )
  }

  return(output)
}


bugs.data <- function(data, dir = getwd(), digits = 5, data.file = "data.txt") {
  # Function from the 'R2WinBUGS' package

  if (is.numeric(unlist(data))) {
    write.datafile(
      lapply(data, formatC, digits = digits, format = "E"),
      file.path(dir, data.file)
    )
  } else {
    data.list        <- lapply(as.list(data), get, pos = parent.frame(2))
    names(data.list) <- as.list(data)
    write.datafile(
      lapply(data.list, formatC, digits = digits, format = "E"),
      file.path(dir, data.file)
    )
  }

  return(data.file)
}


replaceScientificNotation <- function(bmodel, digits = 5) {
  # Function from the 'R2WinBUGS' package

  env <- new.env()
  assign("rSNRidCounter", 0, envir = env)
  replaceID <- function(bmodel, env, digits = 5) {
    for (i in seq_along(bmodel)) {
      if (length(bmodel[[i]]) == 1) {
        if (as.character(bmodel[[i]]) %in% c(":", "[", "[["))
          return(bmodel)
        if ((typeof(bmodel[[i]]) %in% c("double", "integer")) &&
            ((abs(bmodel[[i]]) < 0.001) || (abs(bmodel[[i]]) > 10000))) {
          counter <- get("rSNRidCounter", envir = env) + 1
          assign("rSNRidCounter", counter, envir = env)
          id <- paste("rSNRid", counter, sep = "")
          assign(id, formatC(bmodel[[i]], digits = digits, format = "E"), envir = env)
          bmodel[[i]] <- id
        }
      } else {
        bmodel[[i]] <- replaceID(bmodel[[i]], env, digits = digits)
      }
    }
    bmodel
  }
  bmodel <- deparse(replaceID(bmodel, env, digits = digits), control = NULL)

  for (i in ls(env)) {
    bmodel <- gsub(
      paste("\"", i, "\"", sep = ""),
      get(i, envir = env),
      bmodel,
      fixed = TRUE
    )
  }

  return(bmodel)
}


write.model <- function(model, con = "model.bug", digits = 5) {
  # Function from the 'R2WinBUGS' package

  model.text <- c("model", replaceScientificNotation(body(model), digits = digits))
  model.text <- gsub("%_%", "", model.text)
  writeLines(model.text, con = con)
}


openbugs.log <- function(file) {
  # Function from the 'R2OpenBUGS' package

  if (!file.exists(file))
    stop("Log file", file, "does not exist")
  log.txt <- readLines(file, warn = FALSE)
  extract <- function(m, line.match, skip = 0, empty.left.col = TRUE) {
    start <- (skip + which(m == line.match)[1])
    if (is.na(start))
      return(NA)
    if (length(start) < 1)
      return(NA)
    mx <- strsplit(m[-(1:(start - 1))], "\t")
    n.cols <- length(mx[[1]])
    if (n.cols < 1)
      return(NA)
    mxlen <- sapply(mx, length)
    end <- which(mxlen != n.cols)[1] - 1
    if (!is.na(end)) {
      mx <- mx[1:end]
    }
    cm <- matrix(unlist(mx), ncol = n.cols, byrow = TRUE)
    if (empty.left.col)
      cm <- cm[, -1]
    col.names <- cm[1, -1]
    row.names <- cm[, 1][-1]
    col.names <- gsub("[[:space:]]+", "", col.names)
    cm <- cm[-1, -1]
    m <- matrix(as.numeric(cm), nrow = nrow(cm))
    dimnames(m) <- list(row.names, col.names)
    return(m)
  }
  stats <- extract(log.txt, "OpenBUGS> \t\tmean\tsd\tval2.5pc\tmedian\tval97.5pc\tsample")
  DIC   <- extract(log.txt, "OpenBUGS> \tDbar\tDhat\tDIC\tpD\t", skip = 0,
                   empty.left.col = FALSE)
  list(stats = stats, DIC = DIC)
}


openbugs.script <- function(parameters.to.save, n.chains, n.iter, n.burnin, n.thin,
                            saveExec, restart, model.file.bug, model.file, debug,
                            is.inits, DIC, useWINE, newWINE, WINEPATH, bugs.seed,
                            summary.only, save.history, bugs.data.file,
                            bugs.inits.files, over.relax) {
  # Function from the 'R2OpenBUGS' package

  if ((ceiling(n.iter/n.thin) - ceiling(n.burnin/n.thin)) < 2)
    stop("(n.iter-n.burnin) must be at least 2")
  working.directory <- getwd()
  script <- "script.txt"
  model <- if (length(grep("\\\\", model.file)) || length(grep("/", model.file))) {
    gsub("\\\\", "/", model.file)
  }
  else file.path(working.directory, model.file)
  model <- native2win(model, useWINE = useWINE, newWINE = newWINE, WINEPATH = WINEPATH)
  data <- file.path(working.directory, bugs.data.file)
  data <- native2win(data, useWINE = useWINE, newWINE = newWINE, WINEPATH = WINEPATH)
  coda <- file.path(working.directory, "/")
  coda <- native2win(coda, useWINE = useWINE, newWINE = newWINE, WINEPATH = WINEPATH)
  model.file.bug <- file.path(working.directory, model.file.bug)
  model.file.bug <- native2win(model.file.bug, useWINE = useWINE,
                               newWINE = newWINE, WINEPATH = WINEPATH)
  logFile <- file.path(working.directory, "log.odc")
  logFile <- native2win(logFile, useWINE = useWINE, newWINE = newWINE, WINEPATH = WINEPATH)
  logFileTxt <- file.path(working.directory, "log.txt")
  logFileTxt <- native2win(logFileTxt, useWINE = useWINE, newWINE = newWINE, WINEPATH = WINEPATH)
  inits <- paste0(working.directory, "/", bugs.inits.files)
  inits <- sapply(inits, useWINE = useWINE, newWINE = newWINE,
                  WINEPATH = WINEPATH, function(x, useWINE, newWINE, WINEPATH) {
                    native2win(x, useWINE = useWINE, newWINE = newWINE, WINEPATH = WINEPATH)
                  })
  initlist <- paste0("modelInits(", "'", inits, "',", seq_len(n.chains), ")\n")
  savelist <- paste0("samplesSet(", parameters.to.save, ")\n")
  summarylist <- paste0("summarySet(", parameters.to.save, ")\n")
  bugs.seed.cmd <- ""
  if (!is.null(bugs.seed)) {
    bugs.seed.cmd <- paste0("modelSetRN(", bugs.seed, ")\n")
  }
  thinUpdate <- paste0("modelUpdate(", formatC(n.burnin, format = "d"),
                       ",", n.thin, ",", formatC(n.burnin, format = "d"), ")\n")
  cat(if (.Platform$OS.type == "windows" | useWINE)
    "modelDisplay('log')\n", if (restart)
      c("modelInternalize('", model.file.bug, "')\n"), if (restart && n.burnin > 0)
        c("samplesClear('*')\n", "summaryClear('*')\n"), if (!restart)
          c("modelCheck('", model, "')\n", "modelData('", data,
            "')\n", "modelCompile(", n.chains, ")\n"), if (!restart)
              bugs.seed.cmd, if (!restart && is.inits)
                initlist, if (!restart)
                  "modelGenInits()\n", if (!restart && over.relax)
                    "over.relax(\"yes\")\n", if ((!restart) || (n.burnin > 0))
                      c(thinUpdate, savelist, summarylist),
    if (((!restart) || (n.burnin > 0)) && DIC)
      "dicSet()\n", "modelUpdate(", formatC(n.iter - n.burnin, format = "d"),
    ",", n.thin, ",", formatC(n.iter - n.burnin, format = "d"), ")\n",
    "samplesCoda('*', '", coda, "')\n", "summaryStats('*')\n", if (DIC)
      "dicStats()\n", if (save.history) "samplesHistory('*')\n", if (saveExec)
        c("modelExternalize('", model.file.bug, "')\n"),
    if (.Platform$OS.type == "windows" | useWINE)
      c("modelSaveLog('", logFile, "')\n", "modelSaveLog('",
        logFileTxt, "')\n"), file = script, sep = "", append = FALSE)
  if (!debug)
    cat("modelQuit('y')\n", file = script, append = TRUE)
  sims.files <- paste("CODAchain", 1:n.chains, ".txt", sep = "")
  for (i in seq_len(n.chains))
    cat("OpenBUGS did not run correctly.\n", file = sims.files[i], append = FALSE)
}
