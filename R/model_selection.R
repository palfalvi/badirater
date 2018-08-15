#' Model selection from likelihood test and wAIC measurment
#'
#' @param setup_table Setup table created when `prepare_badirate_scripts()` was invited.
#' @param results_dir Directory where results was saved with `bd_collect()`.
#'
#' @return A tibble with one selected model per orthogroup, including calculated values, like AIC, wAIC, wAIC ratio.
#'
#' @import dplyr
#' @importFrom tidyr unnest
#' @importFrom stringr str_sub
#' @importFrom purrr map
#' @importFrom readr read_delim
#' @export
#'
#' @examples
bd_model_select <- function(setup_table, results_dir){

dplyr::tibble(file_path = list.files(path = results_dir, pattern =  ".likelihood.txt")) %>%
    dplyr::mutate(model = file_path %>% stringr::str_sub(0,2),
         replicates = file_path %>% stringr::str_sub(3,4) %>% as.numeric()) %>%
    dplyr::left_join(setup_table %>% select(-replicates), by = "model") %>%
    dplyr::mutate(data = file_path %>% purrr::map(function(x){paste(results_dir, "/", x, sep = "") %>%
        readr::read_delim(col_names=c("filename", "nann", "likelihood"), delim = " ", col_types = cols(col_character(), col_character(), col_double()))})) %>%
    tidyr::unnest() %>%
    dplyr::mutate(og = filename %>% stringr::str_sub(-21, -13)) %>%
    dplyr::select(og, model, parameters, replicates, likelihood) %>%
    dplyr::mutate(aic = 2*(parameters - likelihood)) %>%
    dplyr::group_by(og, model) %>%
    dplyr::slice(which.min(aic)) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(og) %>%
    dplyr::mutate(min_aic = min(aic),
           waic_num = exp((min_aic - aic)/2),
           waic = waic_num / sum(waic_num),
           best_waic = max(waic),
           second_waic = sort(waic)[-2],
           waic_ratio = best_waic / second_waic) %>%
    dplyr::slice(which.max(waic)) %>%
    dplyr::select(og, model, replicates, aic, waic, waic_ratio) %>%
    dplyr::mutate(signif = waic_ratio > 2.7) %>%
    return()

}
