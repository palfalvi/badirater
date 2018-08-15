#' Decoding branch numbers to species compositions
#'
#' @param tree_id Ultrametric species tree with branch ids, generated with `bd_branch_numbers`
#'
#' @return A data_frame with 2 columns.
#'
#' @import dplyr
#' @importFrom stringr str_split
#' @importFrom ape subtrees
#' @importFrom purrr map_chr
#' @importFrom stringr str_remove_all
#' @importFrom treeio read.newick
#' @export
#'
#' @examples
bd_decode_tree_ids <- function(tree_id){
  tree <- treeio::read.newick(tree_id)

  dplyr::bind_rows(
    tree$tip.label %>%
      stringr::str_split("_", simplify = TRUE) %>%
      dplyr::as_tibble() %>%
      dplyr::rename(tips = V1, node = V2),

    dplyr::tibble(trees=ape::subtrees(tree) ) %>%
      dplyr::mutate(node = trees %>% purrr::map_chr(function(x){x %>% .$node.label %>% as.numeric() %>% max() %>% as.character()}),
             tips = trees %>% purrr::map_chr(function(x){x %>% .$tip.label %>% stringr::str_remove_all("[_0-9]") %>% paste0(collapse = "&")})) %>%
      dplyr::select(tips, node)
  ) %>%
    dplyr::mutate(node = as.numeric(node)) %>%
    dplyr::arrange(node)

}
