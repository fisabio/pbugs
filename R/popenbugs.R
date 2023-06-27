
popenbugs <- function(data, inits, parameters.to.save, model.file, n.chains = 3,
                     n.iter = 2000, n.burnin = floor(n.iter / 2), n.sims = 1000,
                     n.thin = max(1, floor(n.chains * (n.iter - n.burnin) / n.sims)),
                     saveExec = FALSE, restart = FALSE, OpenBUGS.pgm = "default",
                     bin = (n.iter - n.burnin) / n.thin,
                     debug = FALSE, DIC = TRUE, digits = 5, codaPkg = FALSE,
                     cluster = cluster, pbugs.directory,
                     working.directory = NULL, clearWD = FALSE,
                     useWINE = FALSE, WINE = "default",
                     newWINE = TRUE, WINEPATH = "default", bugs.seed = NULL,
                     save.history = FALSE, over.relax = FALSE, summary.only = FALSE,
                     cluster_export = cluster_export) {

  if (OpenBUGS.pgm == "default") {
    if (.Platform$OS.type == "unix") {
      if (useWINE) {
        OpenBUGS.pgm <- unique(
          c(
            list.files(
              path       = "~/.wine/drive_c/Program Files (x86)",
              pattern    = "OpenBUGS\\.exe$",
              recursive  = TRUE,
              full.names = TRUE
            ),

            list.files(
              path       = "~/.wine/drive_c/Program Files",
              pattern    = "OpenBUGS\\.exe$",
              recursive  = TRUE,
              full.names = TRUE
            )
          )
        )
      } else {
        OpenBUGS.pgm <- "/usr/local/bin/OpenBUGSCli"
      }
    } else {
      windows_path <- list.dirs("C:/", recursive = FALSE)
      OpenBUGS.pgm <- unique(
        c(
          list.files(
            path       = "c:/Program Files",
            pattern    = "OpenBUGS\\.exe$",
            recursive  = TRUE,
            full.names = TRUE
          ),
          list.files(
            path       = "c:/Program Files (x86)",
            pattern    = "OpenBUGS\\.exe$",
            recursive  = TRUE,
            full.names = TRUE
            )
        )
      )
    }
  }


  if (!file.exists(OpenBUGS.pgm)) stop("Cannot find the OpenBUGS program")

  if (isTRUE(useWINE)) {
    if (WINE == "default") {
      WINE <- findUnixBinary("wine")
    }
    if (WINEPATH == "default") {
      WINEPATH <- findUnixBinary("winepath")
    }
    Sys.setenv(WINEDEBUG = "err-ole,fixme-all")
  }

  if (!dir.exists(pbugs.directory)) {
    isok <- dir.create(pbugs.directory, recursive = TRUE, mode = "777")
    if (!isok) {
      stop(paste("Cannot create directory:", pbugs.directory, "\n"))
    }
  }

  inTempDir <- FALSE
  if (!is.null(working.directory)) {
    working.directory <- path.expand(working.directory)
  } else {
    working.directory <- tempdir()
    if (.Platform$OS.type == "unix") {
      working.directory <- gsub("//", "/", working.directory)
      Sys.chmod(working.directory, mode = "777")
      on.exit(Sys.chmod(working.directory, mode = "777"), add = TRUE)
    }
    inTempDir <- TRUE
  }
  savedWD <- getwd()
  setwd(working.directory)
  on.exit(setwd(savedWD), add = TRUE)

  if (summary.only) {
    summary.only <- FALSE
    warning("Option summary.only = TRUE is not supported by pbugs.",
            "\nsummary.only has been coerced to FALSE\n")
  }

  arch  <- list.files(dirname(dirname(OpenBUGS.pgm)), recursive = TRUE, full.names = TRUE)
  arch  <- arch[grep("openbugs", arch, ignore.case = TRUE)]
  arch2 <- list.files(dirname(dirname(OpenBUGS.pgm)), recursive = TRUE)

  for (i in seq_len(n.chains)) {
    pbugs.path  <- file.path(pbugs.directory, paste0("OpenBUGS-", i))
    if (!any(file.exists(file.path(pbugs.path, arch2)))) {
      invisible(
        suppressWarnings(
          sapply(file.path(pbugs.path, unique(dirname(arch2))), dir.create, recursive = TRUE)
        )
      )
      isok <- file.copy(arch, file.path(pbugs.path, arch2))
      if (!all(isok)) {
        stop("Cannot create WinBUGS copies")
      }
    }
  }

  if (saveExec) {
    warning("Option 'saveExec' is not available in pbugs.",
            "'saveExec' has been coerced to FALSE", call. = FALSE)
    saveExec <- FALSE
  }
  if (restart) {
    warning("Option 'restart' is not available in pbugs.",
            "'restart' has been coerced to FALSE", call. = FALSE)
    restart <- FALSE
  }

  if (useWINE && (substr(OpenBUGS.pgm, 2, 2) == ":"))
    OpenBUGS.pgm <- win2native(OpenBUGS.pgm, newWINE = newWINE, WINEPATH = WINEPATH)

  #
  #####################
  if (.Platform$OS.type != "windows" & !useWINE) {
    if (debug) stop("The debug option is only available through Wine in unix")
    if (save.history) stop("History plots ('save.history') are not available in unix")
  }
  if (!is.null(bugs.seed) && !bugs.seed %in% 1:14) stop("OpenBUGS seed must be integer in 1:14")
  if (!is.function(model.file) && length(grep("\\.bug", tolower(model.file))))
    stop("model.file must be renamed with .txt rather than .bug")
  if (is.null(working.directory) && (saveExec || restart))
    stop("The working directory must be specified when saveExec or restart is TRUE")

  .fileCopy <- file.copy

  if (!missing(inits) && !is.function(inits) && !is.null(inits) && (length(inits) != n.chains))
    stop("Number of initialized chains (length(inits)) != n.chains")
  if (is.function(model.file)) {
    temp <- tempfile("model")
    temp <- if (.Platform$OS.type != "windows") {
      paste(temp, "txt", sep = ".")
    } else {
      gsub("\\.tmp$", ".txt", temp)
    }
    R2WinBUGS::write.model(model.file, con = temp, digits = digits)
    model.file <- gsub("\\\\", "/", temp)
  } else {
    if (!file.exists(model.file))
      stop(paste(model.file, "does not exist."))
    if (file.info(model.file)$isdir)
      stop(paste(model.file, "is a directory, but a file is required."))
  }


  #####################
  # Pbugs-specific code
  if (inTempDir && basename(model.file) == model.file)
    try(.fileCopy(file.path(savedWD, model.file), model.file, overwrite = TRUE))
  #
  #####################


  if (!(length(data) == 1 && is.vector(data) && is.character(data) && (regexpr("\\.txt$", data) > 0))) {
    bugs.data.file <- R2WinBUGS::bugs.data(data, dir = getwd(), digits)
  } else {
    if (inTempDir && all(basename(data) == data))
      try(file.copy(file.path(savedWD, data), data, overwrite = TRUE))
    if (!file.exists(data)) stop("File", data, "does not exist.")
    bugs.data.file <- data
  }





  #####################
  # Pbugs-specific code
  try(dir.create(file.path(working.directory, "Pbugs-working"), showWarnings = FALSE))


  cluster_tmp <- min(n.chains, max(2, parallel::detectCores() - 1))
  if (is.null(cluster)) {
    cluster <- cluster_tmp
  } else if (cluster > n.chains) {
    warning(
      paste0("Parameter `cluster` > n.chains! Automatic cluster configuration",
             " (based on n.chains and available cores)")
    )
    cluster <- cluster_tmp
  }



  if (is.null(bugs.seed) && n.chains > 14) {
    seed <- sample.int(n = 14, size = n.chains)
  } else {
    RNGkind("L'Ecuyer-CMRG")
    set.seed(bugs.seed)
    seed <- sample.int(n = 14, size = n.chains)
  }


  if (is.character(inits)) {
    #####################
    # Pbugs-specific code
    if (inTempDir && all(basename(inits) == inits))
      try(.fileCopy(file.path(savedWD, inits), inits, overwrite = TRUE))
    #
    #####################

    if (!all(file.exists(inits))) stop("One or more inits files are missing")
    if (length(inits) != n.chains) stop("Need one inits file for each chain")
    bugs.inits.files <- inits
  } else {
    if (!is.function(inits) && !is.null(inits) && (length(inits) != n.chains))
      stop("Number of initialized chains (length(inits)) != n.chains")
    bugs.inits.files <- bugs.inits(
      inits          = inits,
      n.chains       = n.chains,
      digits         =  digits,
      cluster        = cluster,
      bugs.seed      = bugs.seed,
      cluster_export = cluster_export
    )
  }

  if (DIC) parameters.to.save <- c(parameters.to.save, "deviance")
  if (!length(grep("\\.txt$", tolower(model.file)))) {
    new.model.file <- paste0(basename(model.file), ".txt")
    if (!is.null(working.directory))
      new.model.file <- file.path(working.directory, new.model.file)

    #####################
    # Pbugs-specific code
    .fileCopy(model.file, new.model.file, overwrite = TRUE)
    #
    #####################

    on.exit(try(file.remove(new.model.file)), add = TRUE)
  } else {
    new.model.file <- model.file
  }
  if (useWINE) {
    new.model.file <- gsub("//", "/", new.model.file)
  }

  for (i in seq_len(n.chains)) {
    working.aux <- file.path(working.directory, "Pbugs-working", paste0("ch", i))
    model.file.bug <- file.path(working.aux, gsub("\\.txt", ".bug", basename(new.model.file)))
    if (!restart) {
      try(dir.create(working.aux, showWarnings = F))
      try(.fileCopy(basename(model.file), file.path(working.aux, basename(model.file)), overwrite = TRUE))
      try(.fileCopy("data.txt", file.path(working.aux, "data.txt"), overwrite = TRUE))
      try(.fileCopy(paste0("inits", i, ".txt"), file.path(working.aux, "inits1.txt"), overwrite = TRUE))
    } else if (!file.exists(model.file.bug)) {
      stop("The .bug restart file was not found in the working directory")
    }
    setwd(working.aux)
    openbugs.script(
      parameters.to.save = parameters.to.save,
      n.chains           = 1,
      n.iter             = n.iter,
      n.burnin           = n.burnin,
      n.thin             = n.thin,
      saveExec           = saveExec,
      restart            = restart,
      model.file.bug     = model.file.bug,
      model.file         = new.model.file,
      debug              = debug,
      is.inits           = !is.null(inits),
      DIC                = DIC,
      useWINE            = useWINE,
      newWINE            = newWINE,
      WINEPATH           = WINEPATH,
      bugs.seed          = seed[i],
      summary.only       = summary.only,
      save.history       = save.history,
      bugs.data.file     = bugs.data.file,
      bugs.inits.files   = bugs.inits.files[1],
      over.relax         = over.relax
    )
    setwd(working.directory)
  }

  try(popenbugs.run(
    debug           = debug,
    n.burnin        = n.burnin,
    OpenBUGS.pgm    = OpenBUGS.pgm,
    pbugs.directory = pbugs.directory,
    cluster         = cluster,
    n.chains        = n.chains,
    WINE            = WINE,
    useWINE         = useWINE,
    newWINE         = newWINE,
    WINEPATH        = WINEPATH
  ))

  error.msg <- "OpenBUGS did not run correctly."
  error.ch <- which(
    sapply(
      file.path(getwd(), paste0("CODAchain", seq_len(n.chains), ".txt")),
      readLines,
      n = 1
    ) == error.msg
  )
  for (i in seq_along(error.ch)) {
    warning("Chain ", as.numeric(error.ch[i]), " did not run correctly.",
            "\n Look at the log file and try again with 'debug=TRUE' to\n",
            " figure out what went wrong within Bugs.", call. = FALSE)
  }
  if (length(error.ch) > 0) {
    warning("Chains without errors: ", n.chains - length(error.ch), " of ", n.chains, call. = FALSE)
    real.chains <- seq_len(n.chains)[-error.ch]
  } else {
    real.chains <- seq_len(n.chains)
  }

  if (codaPkg) return(file.path(getwd(), paste0("CODAchain", real.chains, ".txt")))


  #####################
  # Pbugs-specific code
  sims <- c(
    bugs.sims(
      parameters.to.save = parameters.to.save,
      n.chains           = n.chains,
      n.iter             = n.iter,
      n.burnin           = n.burnin,
      n.thin             = n.thin,
      error.ch           = error.ch,
      program            = "openbugs"
    ),
    model.file = model.file,
    program    = "openbugs"
  )

  if (DIC) {
    LOG <- vector(mode = "list", length = n.chains)
    pD  <- rep(NA, n.chains)
    DIC <- rep(NA, n.chains)
    for (i in seq_len(n.chains)) {
      LOG[[i]] <- openbugs.log(
        file.path(working.directory, "Pbugs-working", paste0("ch", real.chains[i]), "log.txt")
      )$DIC
    }
    if (any(is.na(LOG))) {
      deviance <- sims$sims.array[, , dim(sims$sims.array)[3], drop = FALSE]
      dimnames(deviance) <- NULL
      dim(deviance) <- dim(deviance)[1:2]
      pD  <- numeric(n.chains)
      DIC <- numeric(n.chains)
      for (i in seq_len(n.chains)) {
        pD[i]  <- stats::var(deviance[, i]) / 2
        DIC[i] <- mean(deviance[, i]) + pD[i]
      }
      sims$DICbyR <- TRUE
    } else {
      for (i in seq_len(n.chains)) {
        pD[i]  <- LOG[[i]][nrow(LOG[[i]]), 4]
        DIC[i] <- LOG[[i]][nrow(LOG[[i]]), 3]
      }
      sims$DICbyR <- FALSE
    }
    sims$isDIC <- TRUE
    sims$pD    <- mean(pD)
    sims$DIC   <- mean(DIC)
  }

  if (clearWD) {
    unlink(
      c(bugs.data.file, "CODAindex.txt", bugs.inits.files, model.file,
        paste0("CODAchain", seq_len(n.chains), ".txt"), "Pbugs-working"),
      recursive = TRUE
    )
  }
  #####################

  class(sims) <- c("pbugs", "bugs")
  if (!is.null(bugs.seed))
    sims$seed  <- bugs.seed
  sims$n.cores <- cluster

  return(sims)
}











popenbugs.run <- function(n.burnin, OpenBUGS.pgm, pbugs.directory, debug = FALSE,
                          cluster, n.chains, WINE = NULL, WINEPATH = NULL,
                          useWINE = FALSE, newWINE = TRUE) {

  .fileCopy <- file.copy

  bugsCall <- vector(length = n.chains)
  for (i in seq_len(n.chains)) {
    if (.Platform$OS.type == "windows" || useWINE) {
      pbugs.path  <- file.path(pbugs.directory, paste0("OpenBUGS-", i))
      pbugs.path  <- list.files(
        path = pbugs.path,
        recursive   = TRUE,
        pattern     = "openbugs\\.exe$",
        ignore.case = TRUE,
        full.names  = TRUE
      )
      bugsCall[i] <- paste0(
        "\"",
        pbugs.path,
        "\" /PAR \"",
        native2win(
          file.path(getwd(), "Pbugs-working", paste0("ch", i), "script.txt"),
          useWINE = useWINE,
          newWINE = newWINE,
          WINEPATH = WINEPATH
        ),
        "\" /"
      )
      if (!debug)
        bugsCall[i] <- paste0(bugsCall[i], "HEADLESS")
      if (useWINE)
        bugsCall[i] <- paste(WINE, bugsCall[i])
    } else {
      pbugs.path  <- OpenBUGS.pgm
      bugsCall[i] <- paste(
        OpenBUGS.pgm,
        "<",
        file.path(getwd(), "Pbugs-working", paste0("ch", i), "script.txt"),
        ">",
        file.path(getwd(), "Pbugs-working", paste0("ch", i), "log.txt")
      )
    }
  }

  cl <- parallel::makeCluster(cluster, type = "PSOCK", setup_strategy = "sequential")
  on.exit(parallel::stopCluster(cl), add = TRUE)

  if ((.Platform$OS.type == "windows" || useWINE) && debug) {
    temp <- parallel::clusterApply(cl, bugsCall, system, invisible = FALSE)
  } else temp <- parallel::clusterApply(cl, bugsCall, system)

  coda_index <- file.path(getwd(), "Pbugs-working", paste0("ch", seq_len(n.chains)), "CODAindex.txt")
  .fileCopy(coda_index[file.exists(coda_index)][1], "CODAindex.txt", overwrite = TRUE)
  for (i in seq_len(n.chains)) {
    .fileCopy(
      file.path(getwd(), "Pbugs-working", paste0("ch", i), "CODAchain1.txt"),
      paste0("CODAchain", i, ".txt"),
      overwrite = TRUE
    )
  }

  if (any(unlist(temp) == -1))
    stop("Error in popenbugs.run().")
  #
  #####################
}
