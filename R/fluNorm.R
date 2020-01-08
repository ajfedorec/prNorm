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
  pr_data$V1 <- pr_data[,flu_name]

  # remove background fluorescence by negating blank well at each timepoint
  pr_data <- pr_data %>%
    dplyr::group_by(time) %>%
    dplyr::mutate(V1 = V1 - V1[well==blank_well])

  pr_data <- as.data.frame(pr_data)

  #
  negative_data <- pr_data %>% dplyr::filter(well == neg_well)

  # fit polynomial model to normalised OD vs fluorescence of negative control
  model <- stats::lm(V1 ~ poly(normalised_OD, 8), data = negative_data)

  # normalise fluorescence by negating predicted autofluorescence at the given OD
  pr_data$V1 <- pr_data$V1 - stats::predict(model, pr_data)

  # plot polynomials for good measure
  ggplot2::ggplot()+
    ggplot2::geom_line(ggplot2::aes(x=negative_data$normalised_OD,
                                    y=stats::predict(model, negative_data)))+
    ggplot2::geom_point(ggplot2::aes(x=negative_data$normalised_OD, y=negative_data$V1))+
    ggplot2::scale_x_continuous("normalised_OD")+
    ggplot2::scale_y_continuous(flu_name)+
    ggplot2::theme_bw()
  ggplot2::ggsave(paste("plot-norm-curve-", flu_name, ".pdf", sep=""))

  # rename normalised fluorescence per cell column
  names(pr_data)[ncol(pr_data)] <- paste("normalised_", flu_name, sep = "")

  return(pr_data)
}
