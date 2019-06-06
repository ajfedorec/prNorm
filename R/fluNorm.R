#' Title
#'
#' @param pr_data a long data.frame containing you plate reader data with OD normalised
#' @param neg_well the well coordinates of a non-fluorescent control
#' @param flu_name the column name of the fluorescence chanel to normalise
#'
#' @return
#'
#' @examples
fluNorm <- function(pr_data, neg_well, flu_name) {
  # make a fluorescence per cell column
  pr_data$V1 <- pr_data[[flu_name]] / pr_data$normalised_OD

  # normalise fluorescence per cell against negative control
  normalised_per_cell_gfp <- c()
  for (timepoint in unique(pr_data$time)) {
    timepoint_normalised_per_cell_gfp <- (pr_data %>%
                                            filter(time == timepoint))$V1 -
      (pr_data %>%
         filter(time == timepoint) %>%
         filter(well == neg_well))$V1

    normalised_per_cell_gfp <- c(normalised_per_cell_gfp, timepoint_normalised_per_cell_gfp)
  }
  pr_data$normalised_per_cell_gfp <- normalised_per_cell_gfp

  # remove fluorescence per cell column
  pr_data$V1 <- NULL

  # rename normalised fluorescence per cell column
  names(pr_data)[ncol(pr_data)] <- paste("norm_per_cell_", flu_name, sep = "")

  return(pr_data)
}