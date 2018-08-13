# Prepare OGs and run scripts for BadiRate

#' Splitting and filtering orthogroups to individual files
#'
#' @param og_file Output from orthofinder (Orthogroups.GeneCount.csv) or similarly formatted count table.
#' @param max_count
#' @param og_path
#'
#' @return Null. Writes files to given path
#' @export
#'
#' @examples
split_orthogroups <- function(og_file, max_count = 40, og_path = "./badirate_orthogroups") {


  ortho <- readr::read_tsv(og_file) %>%
    dplyr::select(-Total)

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


#' Create BadiRate setup table and corresponding scripts for PBS job arrays
#'
#' @param og_path Path to single-file orthogroup folder, created with `split_orthogroups()`
#' @param tree Path to ultrametric species tree file. Leaf names have to match to orthogroup header names.
#' @param branch_models Vector of named branch models. Name should be a 2-letter abbreviation of choice (except gr and fr, which are specific). Value should be either "GR", "FR" or a specific branch model in the syntax of badirate (e.g. "13->14:13->15_18->21"). *CAUTION: More complex models can increase running time drastically.*
#' @param rate_model Rate model. Deafult is GD.
#' @param out_dir Output directory for the future runs.
#' @param script_dir Directory which the script files should be saved.
#' @param replicates How many replications per model. Default is 2. *CAUTION: Big number of replicates can drastically extend runtime*
#' @param ancestral Logical. See BadiRate -anc option.
#' @param outlier Logical. See BadiRate -outlier option.
#' @param seed Initial seed value to set.
#' @param start_value 0 or 1. 0 will start parameter search from Maximum Parsimony, 1 will choose a random number.
#' @param pbs_q Queue of PBS. Default is smps.
#' @param badirate_path Path to BadiRate.pl executabe if not in $PATH.
#' @param create_scripts  Logical. Create pbs job scripts if TRUE, which is the default.
#'
#' @return Set up table of experimetns. Also writes out script files.
#' @export
#'
#' @examples
prepare_badirate <- function(og_path,
                                     tree,
                                     branch_models = c("gr" = "GR", "fr" = "FR", "sp" = "15->16"),
                                     rate_model = "GD",
                                     out_dir = "./raw_outputs",
                                     script_dir = "./scripts",
                                     replicates = 2,
                                     ancestral = TRUE,
                                     outlier = TRUE,
                                     seed = 20180808,
                                     start_value = 1,
                                     pbs_q = "smps",
                                     badirate_path = "",
                                     create_scripts = TRUE) {

  if (create_scripts) {
      if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
      if (!dir.exists(script_dir)) dir.create(script_dir, recursive = TRUE)

      cat("Creating ", length(branch_models), " different scripts with ", replicates, " replications inside.", sep = "")

      for(i in 1:length(branch_models)) {
        readr::write_file(
          paste("#!/bin/bash\n#PBS -q ", pbs_q ,"\n#PBS -V\n#PBS -J 1-", replicates, "\n\n",
              "### BadiRate script filec for ", rate_model, "-", names(branch_models[i]), "-ML\n\n",
              "cd ", og_path, "\n\n",
              "mkdir ", out_dir, "/", names(branch_models[i]), "\n\n",
              "for file in ./*\ndo\n\n",
              "\tperl ", badirate_path, "BadiRate.pl",
              " -seed ", seed,
              " -start_val ", start_value,
              " -rmodel ", rate_model,
              " -bmodel ", branch_models[[i]],
              " -treefile ", tree,
              " -sizefile ./${file}",
              if_else(ancestral, " -anc", ""),
              if_else(outlier, " -outlier", ""),
              " -out ", out_dir, "/", names(branch_models[i]), "/${file}.gr`printf '%02d' ${PBS_ARRAY_INDEX}`.bd", "\n\n",
              "done",sep = ""),
          paste(script_dir, "/badi_", names(branch_models[i]), "_script.pbs", sep = "")
        )
      }
      cat("\nDone.")
  }
  cat("\nPreparing data table.")

  tree_branches = readr::read_file(tree) %>% stringr::str_count(":")

  dplyr::tibble(model = names(branch_models),
         setup = branch_models) %>%
    dplyr::mutate(path = paste0(out_dir, "/", model),
                  tree_size = tree_branches,
                  replicates = replicates,
                  parameters = recode(setup,
                             "GR" = 1*stringr::str_length(rate_model),
                             "FR" = as.double(tree_branches)*stringr::str_length(rate_model),
                              .default = (stringr::str_count(setup, "_")+2)*stringr::str_length(rate_model))) %>%
    return()

}


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

