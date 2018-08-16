#' Splitting and filtering orthogroups to individual files
#'
#' @param og_file Output from orthofinder (Orthogroups.GeneCount.csv) or similarly formatted count table.
#' @param max_count Integer. Maximum number of genes per family/orthogroup per species.
#' @param og_path Path to create the split files into.
#'
#' @return Null. Writes files to given path
#'
#' @import dplyr
#' @importFrom readr read_tsv write_tsv
#' @importFrom utils setTxtProgressBar txtProgressBar
#'
#' @export
#'
split_orthogroups <- function(og_file, max_count = 40, og_path = "./badirate_orthogroups") {


  ortho <- readr::read_tsv(og_file)

  if ("Total" %in% names(ortho)) {
    ortho <- ortho %>%
    dplyr::select(-Total)
  }

  cat(paste("Found ", nrow(ortho), " orthogroups.", "\n", "Filtering...\n", sep = ""))

  filtered <- ortho %>%
    dplyr::filter_at(vars(-1), all_vars(. <= max_count))

  cat(paste("Found ", nrow(filtered), " orthogroups after filtering. (", 100*round(nrow(filtered)/nrow(ortho),4) ,"%)", "\n", "Writing files...\n", sep = ""))

  dir.create(og_path)

  pb <- txtProgressBar(style = 3)
  for (i in 1:nrow(filtered)) {
    readr::write_tsv(x = filtered[i,], path = paste(og_path, "/", filtered[i,1], ".tsv", sep = ""))
    setTxtProgressBar(pb, i/nrow(filtered))
  }
  close(pb)
  cat("Done.")

}
