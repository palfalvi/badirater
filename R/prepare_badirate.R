#' Create BadiRate setup table and corresponding scripts for PBS job arrays
#'
#' @param og_path Path to single-file orthogroup folder, created with `split_orthogroups()`
#' @param tree Path to ultrametric species tree file. Leaf names have to match to orthogroup header names.
#' @param branch_models Vector of named branch models. Name should be a 2-letter abbreviation of choice (except gr and fr, which are specific). Value should be either "GR", "FR" or a specific branch model in the syntax of badirate (e.g. "13->14:13->15_18->21"). *CAUTION: More complex models can increase running time drastically.*
#' @param rate_model Rate model. Deafult is GD.
#' @param estimation Estimation method used for BadiRate. Default is ML.
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
#'
#' @import dplyr
#' @importFrom stringr str_sub str_length str_count
#' @importFrom readr write_file read_file
#' @importFrom utils setTxtProgressBar txtProgressBar
#'
#' @export
#'
prepare_badirate <- function(og_path,
                             tree,
                             branch_models = c("gr" = "GR", "fr" = "FR", "sp" = "15->16"),
                             rate_model = "GD",
                             estimation = "ML",
                             out_dir = "./raw_outputs",
                             script_dir = "./scripts",
                             replicates = 2,
                             ancestral = TRUE,
                             outlier = TRUE,
                             seed = 20180808,
                             start_value = 1,
                             pbs_q = "smps",
                             badirate_path = "",
                             create_scripts = "pbs") {

  if (create_scripts == "pbs") {
    if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
    if (!dir.exists(script_dir)) dir.create(script_dir, recursive = TRUE)

    if(stringr::str_sub(og_path, 1,1) != "/") {
      og_pwd <- paste0(getwd(), "/")
    } else {
      og_pwd <- ""
    }

    if(stringr::str_sub(tree, 1,1) != "/") {
      tree_pwd <- paste0(getwd(), "/")
    } else {
      tree_pwd <- ""
    }


    cat("Creating ", length(branch_models), " different scripts with ", replicates, " replications inside.", sep = "")

    for(i in 1:length(branch_models)) {
      ## PBS specific output
      readr::write_file(
        paste("#!/bin/bash\n#PBS -q ", pbs_q ,"\n#PBS -V\n#PBS -J 1-", replicates, "\n\n",
              "### BadiRate script file for ", rate_model, "-", names(branch_models[i]), "-", estimation,"\n\n",
              "cd ", pg_pwd, og_path, "\n\n",
              "mkdir ", out_dir, "/", names(branch_models[i]), "\n\n",
              "for file in ./*\ndo\n\n",
              "\tperl ", badirate_path, "BadiRate.pl",
              " -seed ", seed,
              " -start_val ", start_value,
              " -rmodel ", rate_model,
              " -bmodel ", branch_models[[i]],
              " -ep ", estimation,
              " -treefile ", tree_pwd, tree,
              " -sizefile ./${file}",
              if_else(ancestral, " -anc", ""),
              if_else(outlier, " -outlier", ""),
              " -out ", out_dir, "/", names(branch_models[i]), "/${file}.gr`printf '%02d' ${PBS_ARRAY_INDEX}`.bd", "\n\n",
              "done",sep = ""),
        paste(script_dir, "/badi_", names(branch_models[i]), "_script.pbs", sep = "")
      )
    }
    cat("\nDone.")
  } else if (create_scripts == "slurm") {
    ## SLURM specific output
    if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
    if (!dir.exists(script_dir)) dir.create(script_dir, recursive = TRUE)

    cat("Creating ", length(branch_models), " different scripts with ", replicates, " replications inside.", sep = "")

    for(i in 1:length(branch_models)) {
      readr::write_file(
        paste("#!/bin/bash\n#SBATCH --export=all\n#SBATCH --array=1-", replicates, "\n\n",
              "### BadiRate script file for ", rate_model, "-", names(branch_models[i]), "-", estimation,"\n\n",
              "cd ", og_path, "\n\n",
              "mkdir ", out_dir, "/", names(branch_models[i]), "\n\n",
              "for file in ./*\ndo\n\n",
              "\tperl ", badirate_path, "BadiRate.pl",
              " -seed ", seed,
              " -start_val ", start_value,
              " -rmodel ", rate_model,
              " -bmodel ", branch_models[[i]],
              " -ep ", estimation,
              " -treefile ", tree,
              " -sizefile ./${file}",
              if_else(ancestral, " -anc", ""),
              if_else(outlier, " -outlier", ""),
              " -out ", out_dir, "/", names(branch_models[i]), "/${file}.gr`printf '%02d' ${SLURM_ARRAY_TASK_ID}`.bd", "\n\n",
              "done",sep = ""),
        paste(script_dir, "/badi_", names(branch_models[i]), "_script.slurm", sep = "")
      )
    }
    cat("\nDone.")
  } else if (create_scripts == "none") {
    cat("No scripts will be created\n")
  } else {
    return("Inapropriate `create script` argument. Should be one of 'pbs', 'slurm' or 'none'")
  }
  cat("\nPreparing data table.\n")

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
