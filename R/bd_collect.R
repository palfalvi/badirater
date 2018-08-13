#' Collect results files from BadiRate runs.
#'
#' @param setup_table Setup table created when `prepare_badirate_scripts()` was invited.
#' @param out_dir Output directory to store result files.
#'
#' @return
#' @export
#'
#' @examples
bd_collect <- function(setup_table, out_dir = "./results"){

  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

  cat("Collecting results. This may take a while...\n")

  for (i in 1:nrow(setup_table)) {
    pb <- txtProgressBar(style = 3)
    cat("\nModel:", setup_table[i,]$model, "\n")
    for (j in 1:setup_table[i,]$replicates) {
      in_length = 30 + setup_table[1,]$tree_size
      system(paste0("awk 'FNR==23,FNR==", 22 + setup_table[1,]$tree_size, " {print FILENAME, $0}' ", setup_table[i,]$path, "/*", setup_table[i,]$model, "`printf '%02d' ", j, "`*.bd > ", out_dir, "/", setup_table[i,]$model, "`printf '%02d' ", j, "`.branch_code.txt"))
      system(paste0("awk 'FNR==", in_length, " {print FILENAME, $0}' ", setup_table[i,]$path, "/*", setup_table[i,]$model, "`printf '%02d' ", j, "`.bd > ", out_dir, "/", setup_table[i,]$model, "`printf '%02d' ", j, "`.likelihood.txt"))
      system(paste0("awk 'FNR==", in_length + 2, ",FNR==", in_length + 1 + setup_table[i,]$parameters/2," {print FILENAME, $0}' ", setup_table[i,]$path, "/*", setup_table[i,]$model, "`printf '%02d' ", j, "`.bd > ", out_dir, "/", setup_table[i,]$model, "`printf '%02d' ", j, "`.parameters.txt"))
      system(paste0("awk 'FNR==",in_length + 10 + setup_table[i,]$parameters/2, ",FNR==", in_length + 9 + setup_table[i,]$parameters/2 + setup_table[1,]$tree_size," {print FILENAME, $0}' ", setup_table[i,]$path, "/*", setup_table[i,]$model, "`printf '%02d' ", j, "`.bd > ", out_dir, "/", setup_table[i,]$model, "`printf '%02d' ", j, "`.gains.txt"))
      setTxtProgressBar(pb, j/setup_table[i,]$replicates)
    }
    close(pb)
  }

  cat("Done.")
}


