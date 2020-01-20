#' Title
#'
#' @param pr_data a long data.frame containing you plate reader data
#' @param blank_well the well coordinates of a media blank
#' @param neg_well the well coordinates of a non-fluorescent control
#' @param OD_name the column name for the optical density data
#' @param flu_names the column names for the fluorscence data
#' @param to_MEFL a Boolean to determine whether to attempt to convert OD and GFP reading to calibrated units
#' @param GFP_gain if to_MEFL=T, the gain value at which GFP was recorded
#' @param lid_type if to_MEFL=T, the sealing mechanism used on the microtitre-plate. See the conversion_factors_csv for available options
#' @param conversion_factors_csv if to_MEFL=T, path of the csv file containing conversion factors from plate reader calibration
#'
#' @return a data.frame with columns for raw plate reader data, normalised data and, if to_MEFL=T, calibrated OD and GFP data
#' @export
#'
#' @examples
prNorm <- function(pr_data, blank_well="A1", neg_well="A2", OD_name="OD", flu_names=c("GFP"), to_MEFL=F, GFP_gain=0,
                   lid_type="nolid", conversion_factors_csv=NA) {
  library(dplyr)

  od_norm_pr_data <- odNorm(pr_data, blank_well, OD_name)

  flu_norm_pr_data <- od_norm_pr_data
  for (flu in flu_names) {
    flu_norm_pr_data <- fluNorm(flu_norm_pr_data, neg_well, blank_well, flu)
  }

  out_data <- flu_norm_pr_data

  if(to_MEFL){
    out_data <- toMEF(out_data, lid_type, GFP_gain, OD_name, conversion_factors_csv)
  }

  return(out_data)
}
