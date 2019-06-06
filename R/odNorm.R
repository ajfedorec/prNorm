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

  normalised_OD <- c()

  for (timepoint in unique(pr_data$time)) {
    timepoint_normalised_OD <- (pr_data %>%
                                  filter(time == timepoint))[[OD_name]] -
      (pr_data %>%
         filter(time == timepoint) %>%
         filter(well == blank_well))[[OD_name]]

    normalised_OD <- c(normalised_OD, timepoint_normalised_OD)
  }
  pr_data$normalised_OD <- normalised_OD

  return(pr_data)
}