#' Title
#'
#' @param pr_data a long data.frame containing you plate reader data
#' @param blank_well the well coordinates of a media blank
#' @param OD_name the column name for the optical density data
#'
#' @return an updated data.frame with an additional column "normalised_OD"
#'
#' @examples
odNorm <- function(pr_data, blank_well, OD_name) {
  pr_data$normalised_OD <- pr_data[,OD_name]

  # remove background OD signal by negating blank well at each timepoint
  pr_data <- pr_data %>%
    dplyr::group_by(time) %>%
    dplyr::mutate(normalised_OD = normalised_OD - normalised_OD[well==blank_well])

  return(as.data.frame(pr_data))
}
