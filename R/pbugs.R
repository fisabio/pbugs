
pbugs <- function(program = c("winbugs", "openbugs"), cluster = NULL,
                  pbugs.directory = "default", ...) {

  stopifnot(is.character(pbugs.directory))
  if (!is.null(cluster)) {
    stopifnot(is.numeric(cluster) && length(cluster) == 1)
  }
  program <- tolower(program)
  program <- match.arg(program)
  if (pbugs.directory == "default") {
    pbugs.directory <- ifelse(
      .Platform$OS.type == "unix",
      path.expand("~/.wine/drive_c/.pbugs"),
      "c:/.pbugs"
    )
  }
  i_time <- Sys.time()
  if (program == "winbugs") {
    bugs_obj <- pwinbugs(program = program, cluster = cluster, pbugs.directory = pbugs.directory, ...)
  } else {
    bugs_obj <- popenbugs(program = program, cluster = cluster, pbugs.directory = pbugs.directory, ...)
  }
  f_time    <- Sys.time()
  exec_time <- f_time - i_time
  attributes(bugs_obj)$exec_time <- paste(
    "Execution time:", round(as.numeric(exec_time), 2), attr(exec_time, "units")
  )
  class(bugs_obj) <- c(class(bugs_obj), "pbugs")

  return(bugs_obj)
}
