#' Title
#'
#' @param pr_data a long data.frame containing you plate reader data
#' @param blank_well the well coordinates of a media blank
#' @param neg_well the well coordinates of a non-fluorescent control
#' @param OD_name the column name for the optical density data
#' @param flu_names the column names for the fluorscence data
#'
#' @return
#' @export
#'
#' @examples
prNorm <- function(pr_data, blank_well="A1", neg_well="A2", OD_name="OD", flu_names=c("GFP")) {
  library(dplyr)

  od_norm_pr_data <- odNorm(pr_data, blank_well, OD_name)

  flu_norm_pr_data <- od_norm_pr_data
  for (flu in flu_names) {
    flu_norm_pr_data <- fluNorm(flu_norm_pr_data, neg_well, blank_well, flu)
  }

  return(flu_norm_pr_data)
}
