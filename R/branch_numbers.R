#' Give branch numbering for BadiRate
#'
#' @param tree Ultrametric species tree.
#' @param og Orthogroup count file (Orthogroups.GeneCount.csv).
#' @param badirate_path Path to BadiRate.pl executable.
#' @param plot Logical. If TRUE, plots a tree with corresponding branch numbers. If FALSE, gives the tree in text.
#' @param tree_file Where to save the tree, which contains the branch ids.
#'
#' @return Either a plot or text of a labeled phylogenetic tree.
#'
#' @importFrom treeio read.newick
#' @importFrom readr write_file
#' @importFrom datasets trees
#' @export
#'
#' @examples
bd_branch_numbers <- function(tree, og = "./Orthogroups.GeneCount.csv", badirate_path = "", plot = FALSE, tree_file = "./branch_ids.tree") {

  tree_id <- system(command = paste0("perl ", badirate_path, "BadiRate.pl -print_ids -treefile ", tree, " -sizefile ", og), wait = TRUE, inter = TRUE)[2]
  readr::write_file(x = tree_id, path = tree_file)

  if (plot){
    tree_id <- treeio::read.newick(tree_file)
    return(plot(tree_id, show.node.label = TRUE, show.tip.label = TRUE))
  } else {
    return(tree_id)
  }
}
