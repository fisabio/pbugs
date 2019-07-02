
pwinbugs <- function(data, inits, parameters.to.save, model.file, n.chains = 3,
                     n.iter = 2000, n.burnin = floor(n.iter/2),
                     n.thin = max(1, floor(n.chains * (n.iter - n.burnin)/n.sims)),
                     n.sims = 1000, bin = (n.iter - n.burnin)/n.thin, debug = FALSE,
                     DIC = TRUE, digits = 5, codaPkg = FALSE, bugs.directory, cluster = NULL,
                     pbugs.directory, working.directory = NULL, clearWD = FALSE,
                     useWINE = (.Platform$OS.type != "windows"), WINE = "/usr/bin/wine",
                     newWINE = TRUE, WINEPATH = "/usr/bin/winepath", bugs.seed = NULL,
                     summary.only = FALSE, save.history = !summary.only, over.relax = FALSE,
                     slice) {

  if (summary.only) {
    summary.only <- FALSE
    warning("Option summary.only = TRUE is not supported by pbugs.",
            "\nsummary.only has been coerced to FALSE\n")
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

  if (.Platform$OS.type == "unix")
    Sys.setenv(WINEDEBUG = "err-ole,fixme-all")

  .fileCopy   <- file.copy
  wb_registry <- system.file("WinBUGS_files/Registry.odc", package = "pbugs")

  path_to_method <- if (slice) {
    system.file("WinBUGS_files/Methods_slice.odc", package = "pbugs")
  } else {
    system.file("WinBUGS_files/Methods.odc", package = "pbugs")
  }

  # Creates, and copies WinBUGS copies to, pbugs.directory
  if (!dir.exists(pbugs.directory)) {
    isok <- dir.create(pbugs.directory, recursive = TRUE, mode = "777")
    if (!isok) {
      stop(paste("Cannot create directory:", pbugs.directory, "\n"))
    }
  }

  for (i in seq_len(n.chains)) {
    pbugs.path  <- file.path(pbugs.directory, paste0(basename(bugs.directory), "-", i))
    bugs.exists <- file.exists(pbugs.path)
    if (!bugs.exists) {
      isok1 <- .fileCopy(bugs.directory, pbugs.directory, recursive = TRUE, overwrite = TRUE)
      isok2 <- file.rename(
        file.path(pbugs.directory, basename(bugs.directory)),
        pbugs.path
      )
      if (!isok1 || !isok2) {
        stop("Cannot create WinBUGS copies")
      }
    }
    .fileCopy(
      wb_registry,
      file.path(pbugs.path, "System", "Rsrc", "Registry.odc"),
      overwrite = TRUE
    )
    .fileCopy(
      path_to_method,
      file.path(pbugs.path, "Updater", "Rsrc", "Methods.odc"),
      overwrite = TRUE
    )
  }
  on.exit(
    .fileCopy(
      system.file("WinBUGS_files/Registry.odc", package = "pbugs"),
      file.path(pbugs.directory, paste0(basename(bugs.directory), "-", seq_len(n.chains)),
                "System", "Rsrc", "Registry.odc"),
      overwrite = TRUE
    ), add = TRUE
  )

  #####################

  if (missing(bugs.directory) && !is.null(bugs.dir <- getOption("R2WinBUGS.bugs.directory"))) {
    bugs.directory <- bugs.dir
  }

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
    bugs.data.file <- R2WinBUGS::bugs.data(data, dir = getwd(), digits = digits)
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
    bugs.inits.files <- bugs.inits(inits, n.chains, digits = digits)
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

  try(pwinbugs.run(
    n.burnin           = n.burnin,
    bugs.directory     = bugs.directory,
    cluster            = cluster,
    pbugs.directory    = pbugs.directory,
    n.chains           = n.chains,
    WINE               = WINE,
    useWINE            = useWINE,
    newWINE            = newWINE,
    WINEPATH           = WINEPATH,
    working.directory  = working.directory,
    new.model.file     = new.model.file,
    parameters.to.save = parameters.to.save,
    n.iter             = n.iter,
    n.thin             = n.thin,
    debug              = debug,
    inits              = inits,
    bin                = bin,
    DIC                = DIC,
    summary.only       = FALSE,
    save.history       = save.history,
    bugs.data.file     = bugs.data.file,
    bugs.inits.files   = bugs.inits.files,
    over.relax         = over.relax,
    bugs.seed          = bugs.seed
  ))

  error.msg <- "WinBUGS did not run correctly."
  error.ch <- which(
    sapply(
      file.path(getwd(), paste0("coda", seq_len(n.chains), ".txt")),
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

  #
  #####################

  if (codaPkg) return(file.path(getwd(), paste0("coda", real.chains, ".txt")))

  #####################
  # Pbugs-specific code
  sims <- c(
    bugs.sims(
      parameters.to.save = parameters.to.save,
      n.chains           = n.chains,
      n.iter             = n.iter,
      n.burnin           = n.burnin,
      n.thin             = n.thin,
      error.ch           = error.ch
    ),
    model.file = model.file,
    program    = "winbugs"
  )
  n.chains    <- n.chains - length(error.ch)

  if (DIC) {
    LOG <- list(length = n.chains)
    pD  <- rep(NA, n.chains)
    DIC <- rep(NA, n.chains)
    for (i in seq_len(n.chains)) {
      LOG[[i]] <- bugs.log(
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
        "script.txt", paste0("coda", c(error.ch, real.chains), ".txt"), "Pbugs")
    )
  }
  #####################

  class(sims) <- c("pbugs", "bugs")
  if (!is.null(bugs.seed))
    sims$seed  <- bugs.seed
  sims$n.cores <- cluster

  return(sims)
}


pwinbugs.run <- function(n.burnin, bugs.directory, cluster, pbugs.directory, n.chains,
                         WINE, useWINE, newWINE, WINEPATH, working.directory,
                         new.model.file, parameters.to.save, n.iter, n.thin, debug,
                         inits, bin, DIC, summary.only, save.history, bugs.data.file,
                         bugs.inits.files, over.relax, bugs.seed) {


  .fileCopy <- file.copy
  cl <- parallel::makeCluster(cluster, type = "PSOCK")
  RNGkind("L'Ecuyer-CMRG")
  on.exit(parallel::stopCluster(cl), add = TRUE)
  if (!is.null(bugs.seed)) {
    parallel::clusterSetRNGStream(cl, bugs.seed)
  }
  seed <- parallel::parSapply(cl, rep(1, n.chains), FUN = function(x) sample.int(n = 1e+6, size = 1))

  for (i in seq_len(n.chains)) {
    working.aux <- file.path(working.directory, "Pbugs-working", paste0("ch", i))
    try(dir.create(working.aux, showWarnings = F))
    try(.fileCopy(basename(new.model.file), file.path(working.aux, basename(new.model.file)), overwrite = TRUE))
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

  temp       <- parallel::clusterApply(cl, bugsCall, system)
  coda_index <- file.path(getwd(), "Pbugs-working", paste0("ch", seq_len(n.chains)), "codaIndex.txt")
  .fileCopy(coda_index[file.exists(coda_index)][1], "codaIndex.txt", overwrite = TRUE)
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
