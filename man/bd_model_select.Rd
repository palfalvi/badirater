% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/model_selection.R
\name{bd_model_select}
\alias{bd_model_select}
\title{Model selection from likelihood test and wAIC measurment}
\usage{
bd_model_select(setup_table, results_dir)
}
\arguments{
\item{setup_table}{Setup table created when `prepare_badirate_scripts()` was invited.}

\item{results_dir}{Directory where results was saved with `bd_collect()`.}
}
\value{
A tibble with one selected model per orthogroup, including calculated values, like AIC, wAIC, wAIC ratio.
}
\description{
Model selection from likelihood test and wAIC measurment
}
