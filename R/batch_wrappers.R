#' Wrapper to qsub command
#'
#' @param files Script files to be submitted.
#' @param ... Other options to be passed to `qsub`.
#'
#' @export
#'
qsub <- function(files, ...) {
  for (i in 1:length(files)){
    system(paste("qsub ", ..., " ", files[i], sep = ""))
  }
}

#' Wrapper to qstat command
#'
#' @param ... Other options to be passed to `qstat`.
#'
#' @export
#'
qstat <- function(...) {
  system(paste("qstat -u ", Sys.info()[["user"]], " ",sep = "", ...))
}


#' Wrapper to sbatch command
#'
#' @param files Script files to be submitted.
#' @param ... Other options to be passed to `sbatch`.
#'
#' @export
#'
sbatch <- function(files, ...) {
  for (i in 1:length(files)){
    system(paste("sbatch ", ..., " ", files[i], sep = ""))
  }
}

#' Wrapper to queue command
#'
#' @param ... Other options to be passed to `squeue`.
#'
#' @export
#'
squeue <- function(...) {
  system(paste("squeue -u ", Sys.info()[["user"]], " ",sep = "", ...))
}

