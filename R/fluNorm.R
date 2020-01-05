#' Title
#'
#' @param pr_data a long data.frame containing you plate reader data with OD normalised
#' @param neg_well the well coordinates of a non-fluorescent control
#' @param blank_well the well coordinates of a media blank
#' @param flu_name the column name of the fluorescence chanel to normalise
#'
#' @return
#'
#' @examples
fluNorm <- function(pr_data, neg_well, blank_well, flu_name) {
  first_norm_flu <- c()

  for (timepoint in unique(pr_data$time)) {
    timepoint_normalised_flu <- (pr_data %>%
                                   dplyr::filter(time == timepoint))[[flu_name]] -
      (pr_data %>%
         dplyr::filter(time == timepoint) %>%
         dplyr::filter(well == blank_well))[[flu_name]]

    first_norm_flu <- c(first_norm_flu, timepoint_normalised_flu)
  }
  pr_data[,flu_name] <- first_norm_flu


  # make a normalise fluorescence column
  pr_data$V1 <- pr_data[,flu_name]

  negative_data <- pr_data %>% dplyr::filter(well == neg_well)

  # fit polynomial model to normalised OD vs fluorescence of negative control
  # model <- lm(negative_data[,flu_name] ~ poly(negative_data$normalised_OD, 4), raw=TRUE)
  model <- stats::lm(negative_data[,flu_name] ~ negative_data$normalised_OD+I(negative_data$normalised_OD^2)+I(negative_data$normalised_OD^3))

  # normalise fluorescence against model
  pr_data$V1 <- pr_data$V1 - (stats::coef(model)[1] +
                                stats::coef(model)[2] * pr_data$normalised_OD +
                                stats::coef(model)[3] * (pr_data$normalised_OD)^2 +
                                stats::coef(model)[4] * (pr_data$normalised_OD)^3)

  # rename normalised fluorescence per cell column
  names(pr_data)[ncol(pr_data)] <- paste("normalised_", flu_name, sep = "")

  return(pr_data)
}