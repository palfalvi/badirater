#' Wrapper to qsub command
#'
#' @param files Script files to be submitted.
#' @param ... Other options to be passed to `qsub`.
#'
#' @return
#' @export
#'
#' @examples
qsub <- function(files, ...) {
  for (i in 1:length(files)){
    system(paste("qsub ", ..., " ", files[i], sep = ""))
  }
}

#' Wrapper to qstat command
#'
#' @param ... Other options to be passed to `qstat`.
#'
#' @return
#' @export
#'
#' @examples
qstat <- function(...) {
  system(paste("qstat -u ", Sys.info()[["user"]], " ",sep = "", ...))
}


#' Wrapper to sbatch command
#'
#' @param files Script files to be submitted.
#' @param ... Other options to be passed to `sbatch`.
#'
#' @return
#' @export
#'
#' @examples
sbatch <- function(files, ...) {
  for (i in 1:length(files)){
    system(paste("sbatch ", ..., " ", files[i], sep = ""))
  }
}

#' Wrapper to queue command
#'
#' @param ... Other options to be passed to `squeue`.
#'
#' @return
#' @export
#'
#' @examples
squeue <- function(...) {
  system(paste("squeue -u ", Sys.info()[["user"]], " ",sep = "", ...))
}

