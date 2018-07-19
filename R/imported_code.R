
findUnixBinary <- function(x) {
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


bugs.inits <- function(inits, n.chains, digits, inits.files = paste("inits", 1:n.chains, ".txt", sep = "")) {
  if (!is.null(inits)) {
    for (i in 1:n.chains) {
      if (is.function(inits))
        write.datafile(lapply(inits(), formatC, digits = digits, format = "E"), inits.files[i])
       else
        write.datafile(lapply(inits[[i]], formatC, digits = digits, format = "E"), inits.files[i])
    }
  }
  return(inits.files)
}


write.datafile <- function(datalist, towhere, fill = TRUE) {
  if (!is.list(datalist) || is.data.frame(datalist))
    stop("First argument to write.datafile must be a list.")
  cat(formatdata(datalist), file = towhere, fill = fill)
}


formatdata <- function(datalist) {
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



bugs.script <- function(parameters.to.save, n.chains, n.iter, n.burnin, n.thin,
                        model.file, debug = FALSE, is.inits, bin, DIC = FALSE,
                        useWINE = .Platform$OS.type != "windows", newWINE = TRUE,
                        WINEPATH = NULL, bugs.seed = NULL, summary.only = FALSE,
                        save.history = TRUE, bugs.data.file, bugs.inits.files,
                        over.relax = FALSE) {
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
  initlist <- paste("inits (", 1:n.chains, ", '", inits, "')\n", sep = "")
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
  sims.files <- paste("coda", 1:n.chains, ".txt", sep = "")
  for (i in 1:n.chains) cat("WinBUGS did not run correctly.\n",
                            file = sims.files[i], append = FALSE)
}


bugs.update.settings <- function(n.burnin, bugs.directory) {
    .fileCopy <- file.copy
    .regexpr <- function(...) regexpr(..., useBytes = TRUE)
    .sub <- function(...) sub(..., useBytes = TRUE)
    .writeBin <- if (getRversion() >= "2.10")
      function(...) writeBin(..., useBytes = TRUE)
    else writeBin
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
  win.dr <- substr(windir, 1, 2)
  ind <- pmatch(toupper(win.dr), DriveTable$drive)
  native.dr <- DriveTable$path[ind]
  sub(win.dr, native.dr, windir)
}


winedriveRTr <- function(unixpath, DriveTable = winedriveMap()) {
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


fround <- function(x, digits) format(round(x, digits), nsmall = digits)
