
pwinbugs <- function(data, inits, parameters.to.save, model.file, n.chains = 3,
                     n.iter = 2000, n.burnin = floor(n.iter / 2),
                     n.thin = max(1, floor(n.chains * (n.iter - n.burnin) / n.sims)),
                     n.sims = 1000, bin = (n.iter - n.burnin) / n.thin,
                     debug = FALSE, DIC = TRUE, digits = 5, codaPkg = FALSE,
                     bugs.directory = "default",
                     cluster = cluster, pbugs.directory = pbugs.directory,
                     working.directory = NULL, clearWD = FALSE,
                     useWINE = .Platform$OS.type != "windows", WINE = "/usr/bin/wine",
                     newWINE = TRUE, WINEPATH = "/usr/bin/winepath", bugs.seed = NULL,
                     summary.only = FALSE, save.history = !summary.only,
                     over.relax = FALSE) {

  #####################
  # Pbugs-specific code
  if (summary.only) {
    summary.only <- FALSE
    warning("Option summary.only = TRUE is not supported by pbugs.",
            "\nsummary.only has been coerced to FALSE\n")
  }
  if (bugs.directory == "default") {
    bugs.directory <- ifelse(
      .Platform$OS.type == "unix",
      path.expand("~/.wine/drive_c/Program Files/WinBUGS14"),
      "C:/Program Files/WinBUGS14"
    )
  }
  .fileCopy <- file.copy

  # Creates, and copies WinBUGS copies to, pbugs.directory
  if (!dir.exists(pbugs.directory)) {
    isok <- dir.create(pbugs.directory, recursive = TRUE, mode = "0777")
    if (!isok) {
      stop(paste("Cannot create directory:", pbugs.directory, "\n"))
    }
    .fileCopy(
      file.path(bugs.directory,  "System", "Rsrc", "Registry.odc"),
      file.path(pbugs.directory, "Registry_Rsave.odc")
    )
  } else {
    if (!file.exists(file.path(pbugs.directory, "Registry_Rsave.odc"))) {
      .fileCopy(
        file.path(bugs.directory,  "System", "Rsrc", "Registry.odc"),
        file.path(pbugs.directory, "Registry_Rsave.odc")
      )
    }
  }
  for (i in seq_len(n.chains)) {
    pbugs_path  <- file.path(pbugs.directory, paste0(basename(bugs.directory), "-", i))
    bugs_exists <- file.exists(pbugs_path)
    if (!bugs_exists) {
      isok1 <- .fileCopy(bugs.directory, pbugs.directory, recursive = TRUE)
      isok2 <- file.rename(
        file.path(pbugs.directory, basename(bugs.directory)),
        pbugs_path
      )
      if (!isok1 || !isok2) {
        stop("Cannot create WinBUGS copies")
      }
    }
    .fileCopy(
      file.path(pbugs.directory, "Registry_Rsave.odc"),
      file.path(pbugs_path, "System", "Rsrc", "Registry.odc"),
      overwrite = TRUE
    )

    met_pbugs <- file.path(pbugs_path, "Updater", "Rsrc", "Methods.odc")
    met_bugs  <- file.path(bugs.directory, "Updater", "Rsrc", "Methods.odc")

    if (file.info(met_pbugs)[["size"]] != file.info(met_bugs)[["size"]]) {
      .fileCopy(met_bugs, met_pbugs, overwrite = TRUE)
    }
  }
  #####################

  inTempDir <- FALSE
  if (!is.null(working.directory)) {
    working.directory <- path.expand(working.directory)
    savedWD <- getwd()
    setwd(working.directory)
    on.exit(setwd(savedWD))
  } else {
    working.directory <- tempdir()
    if (useWINE) {
      working.directory <- gsub("//", "/", working.directory)
      Sys.chmod(working.directory, mode = "770")
      on.exit(Sys.chmod(working.directory, mode = "700"), add = TRUE)
    }
    savedWD <- getwd()
    setwd(working.directory)
    on.exit(setwd(savedWD), add = TRUE)
    inTempDir <- TRUE
  }
  if (missing(bugs.directory) && !is.null(bugs.dir <- getOption("R2WinBUGS.bugs.directory"))) {
    bugs.directory <- bugs.dir
  }

  if (!missing(inits) && !is.function(inits) && !is.null(inits) && (length(inits) != n.chains))
    stop("Number of initialized chains (length(inits)) != n.chains")
  if (useWINE) {
    Sys.setenv(WINEDEBUG = "err-ole,fixme-all")
    if (is.null(WINE)) {
      WINE <- findUnixBinary(x = "wine")
    }
    if (is.null(WINEPATH)) {
      WINEPATH <- findUnixBinary(x = "winepath")
    }
  }
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

    #####################
    # Pbugs-specific code
    if (inTempDir && all(basename(data) == data))
      try(.fileCopy(file.path(savedWD, data), data, overwrite = TRUE))
    #
    #####################

    if (!file.exists(data)) stop("File", data, "does not exist.")
    bugs.data.file <- data
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
    bugs.inits.files <- bugs.inits(inits, n.chains, digits)
  }
  if (DIC) parameters.to.save <- c(parameters.to.save, "deviance")
  if (!length(grep("\\.txt$", tolower(model.file)))) {
    new.model.file <- paste0(basename(model.file), ".txt")
    if (!is.null(working.directory)) new.model.file <- file.path(working.directory, new.model.file)

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

  #####################
  # Pbugs-specific code
  try(dir.create(file.path(working.directory, "Pbugs-working"), showWarnings = F))

  if (is.null(cluster) || cluster > n.chains) {
    cluster <- min(n.chains, max(2, parallel::detectCores() - 1))
  }
  cl <- parallel::makeCluster(cluster, type = "PSOCK")
  RNGkind("L'Ecuyer-CMRG")
  if (!is.null(bugs.seed)) {
    parallel::clusterSetRNGStream(cl, bugs.seed)
  }
  seed <- parallel::parSapply(cl, rep(1, n.chains), FUN = function(x) sample.int(n = 1e+6, size = 1))
  parallel::stopCluster(cl)

  for (i in seq_len(n.chains)) {
    working.aux <- file.path(working.directory, "Pbugs-working", paste0("ch", i))
    try(dir.create(working.aux, showWarnings = F))
    try(.fileCopy(basename(model.file), file.path(working.aux, basename(model.file)), overwrite = TRUE))
    try(.fileCopy("data.txt", file.path(working.aux, "data.txt"), overwrite = TRUE))
    try(.fileCopy(paste0("inits", i, ".txt"), file.path(working.aux, "inits1.txt"), overwrite = TRUE))
    setwd(working.aux)
    winbugs.script(
      parameters.to.save = parameters.to.save,
      n.chains           = 1,
      n.iter             = n.iter,
      n.burnin           = n.burnin,
      n.thin             = n.thin,
      model.file         = new.model.file,
      debug              = debug,
      is.inits           = !is.null(inits),
      bin                = bin,
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

  try(pwinbugs.run(
    n.burnin        = n.burnin,
    bugs.directory  = bugs.directory,
    cluster         = cluster,
    pbugs.directory = pbugs.directory,
    n.chains        = n.chains,
    WINE            = WINE,
    useWINE         = useWINE,
    newWINE         = newWINE,
    WINEPATH        = WINEPATH
  ))
  error_msg <- "WinBUGS did not run correctly."
  error_ch <- which(
    sapply(
      file.path(getwd(), paste0("coda", seq_len(n.chains), ".txt")),
      readLines,
      n = 1
    ) == error_msg
  )
  for (i in seq_along(error_ch)) {
    warning("Chain ", as.numeric(error_ch[i]), " did not run correctly.",
            "\n Look at the log file and try again with 'debug=TRUE' to\n",
            " figure out what went wrong within Bugs.", call. = FALSE)
  }
  if (length(error_ch) > 0) {
    warning("Chains without errors: ", n.chains - length(error_ch), " of ", n.chains, call. = FALSE)
    real_chains <- seq_len(n.chains)[-error_ch]
  } else {
    real_chains <- seq_len(n.chains)
  }

  #
  #####################

  if (codaPkg) return(file.path(getwd(), paste0("coda", real_chains, ".txt")))

  #####################
  # Pbugs-specific code
  sims <- c(
    bugs.sims(
      parameters.to.save = parameters.to.save,
      n.chains           = n.chains,
      n.iter             = n.iter,
      n.burnin           = n.burnin,
      n.thin             = n.thin,
      error_ch           = error_ch
    ),
    model.file = model.file,
    program    = "winbugs"
  )
  n.chains    <- n.chains - length(error_ch)

  if (DIC) {
    LOG <- list(length = n.chains)
    pD  <- rep(NA, n.chains)
    DIC <- rep(NA, n.chains)
    for (i in seq_len(n.chains)) {
      LOG[[i]] <- bugs.log(
        file.path(working.directory, "Pbugs-working", paste0("ch", real_chains[i]), "log.txt")
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
        pD[i]  <- LOG[[i]][nrow(LOG[[i]]), 3]
        DIC[i] <- LOG[[i]][nrow(LOG[[i]]), 4]
      }
      sims$DICbyR <- FALSE
    }
    sims$isDIC <- TRUE
    sims$pD    <- mean(pD)
    sims$DIC   <- mean(DIC)
  }

  if (clearWD) {
    file.remove(
      c(bugs.data.file, "log.odc", "log.txt", "codaIndex.txt", bugs.inits.files,
        "script.txt", paste0("coda", c(error_ch, real_chains), ".txt"), "Pbugs")
    )
  }
  #####################

  class(sims) <- c("pbugs", "bugs")
  if (!is.null(bugs.seed))
    sims$seed  <- bugs.seed
  sims$n_cores <- cluster

  return(sims)
}



pwinbugs.run <- function(n.burnin, bugs.directory, cluster, pbugs.directory,
                         n.chains, useWINE = .Platform$OS.type != "windows",
                         WINE = NULL, newWINE = TRUE, WINEPATH = NULL) {

  if (useWINE && (substr(bugs.directory, 2, 2) == ":"))
    bugs.directory <- win2native(bugs.directory, newWINE = newWINE, WINEPATH = WINEPATH)

  #####################
  # Pbugs-specific code
  for (i in seq_len(n.chains)) {
    try(
      bugs.update.settings(
        n.burnin       = n.burnin,
        bugs.directory = file.path(pbugs.directory, paste0(basename(bugs.directory), "-", i))
      )
    )
  }
  #####################

  .fileCopy <- file.copy

  #####################
  # Pbugs-specific code
  on.exit(
    .fileCopy(
      file.path(pbugs.directory, "Registry_Rsave.odc"),
      file.path(pbugs.directory, paste0(basename(bugs.directory), "-", seq_len(n.chains)),
                "System", "Rsrc", "Registry.odc"),
      overwrite = TRUE
    ), add = TRUE
  )

  bugsCall <- vector(length = n.chains)
  for (i in seq_len(n.chains)) {
    dos.location <- file.path(
      pbugs.directory,
      paste0(basename(bugs.directory), "-", i),
      grep("^Win[[:alnum:]]*[.]exe$", list.files(bugs.directory), value = TRUE)[1]
    )
    if (!file.exists(dos.location))
      stop(paste("WinBUGS executable does not exist in", paste0(basename(bugs.directory), "-", i)))
    bugsCall[i] <- paste0(
      "\"",
      dos.location,
      "\" /par \"",
      native2win(
        file.path(getwd(), "Pbugs-working", paste0("ch", i), "script.txt"),
        useWINE = useWINE, newWINE = newWINE, WINEPATH = WINEPATH
      ),
      "\""
    )
    if (useWINE)
      bugsCall[i] <- paste(WINE, bugsCall[i])
  }

  cl <- parallel::makeCluster(cluster, type = "PSOCK")
  on.exit(parallel::stopCluster(cl), add = TRUE)

  temp <- parallel::clusterApply(cl, bugsCall, system)
  .fileCopy(file.path(getwd(), "Pbugs-working", "ch1", "codaIndex.txt"), "codaIndex.txt", overwrite = TRUE)
  for (i in seq_len(n.chains)) {
    .fileCopy(
      file.path(getwd(), "Pbugs-working", paste0("ch", i), "coda1.txt"),
      paste0("coda", i, ".txt"),
      overwrite = TRUE
    )
  }
  if (any(unlist(temp) == -1))
    stop("Error in pwinbugs.run().\nCheck that WinBUGS is in the specified directory.")
  #
  #####################
}
