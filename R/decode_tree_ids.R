bd_decode_tree_ids(tree_id){
  tree <- treeio::read.newick(tree_id)

  dplyr::bind_rows(
    tree$tip.label %>%
      stringr::str_split("_", simplify = TRUE) %>%
      dplyr::as_tibble() %>%
      dplyr::rename(tips = V1, node = V2),

    dplyr::tibble(trees=ape::subtrees(tree) ) %>%
      dplyr::mutate(node = trees %>% map_chr(function(x){x %>% .$node.label %>% as.numeric() %>% max() %>% as.character()}),
             tips = trees %>% map_chr(function(x){x %>% .$tip.label %>% str_remove_all("[_0-9]") %>% paste0(collapse = "&")})) %>%
      dplyr::select(tips, node)
  ) %>%
    dplyr::mutate(node = as.numeric(node)) %>%
    dplyr::arrange(node)

}
