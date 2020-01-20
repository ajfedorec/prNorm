#' Get Conversion Factors
#' Make sure the ggplot2 library is installed.
#'
#' @param GFP_gain if to_MEFL=T, the gain value at which GFP was recorded
#' @param OD_name the column name for the optical density data
#' @param lid if to_MEFL=T, the sealing mechanism used on the microtitre-plate. See the conversion_factors_csv for available options
#' @param conversion_factors_csv if to_MEFL=T, path of the csv file containing conversion factors from plate reader calibration
#'
#'
#' @return

toMEF <- function(pr_data, lid, GFP_gain, OD_name, conversion_factors_csv) {
  conversion_factors <- utils::read.csv(conversion_factors_csv)

  ### Get conversion factor for OD
  OD_cf <- unlist(conversion_factors %>%
    dplyr::filter(measure==OD_name) %>%
    dplyr::filter(lid_type==lid) %>%
    dplyr::select(slope))

  ### Get conversion factor for GFP
  GFP_cfs <- conversion_factors %>%
    dplyr::filter(Calibrant=="fluorescein") %>%
    dplyr::filter(lid_type==lid)

  GFP_cfs$measure <- as.numeric(gsub('Gain ','',GFP_cfs$measure))

  if(length(which(GFP_cfs$measure == GFP_gain))>0){
    GFP_cf <- GFP_cfs[which(GFP_cfs$measure == GFP_gain),]$slope
  } else {
    model <- stats::lm(log10(slope) ~ poly(measure, 2), data = GFP_cfs)

    GFP_cf <- 10^stats::predict(model, data.frame(measure=GFP_gain))

    ggplot2::ggplot()+
      ggplot2::geom_line(ggplot2::aes(x=GFP_cfs$measure,
                                      y=10^stats::predict(model, GFP_cfs)))+
      ggplot2::geom_point(ggplot2::aes(x=GFP_cfs$measure, y=GFP_cfs$slope))+
      ggplot2::geom_vline(xintercept = GFP_gain, linetype=2)+
      ggplot2::geom_hline(yintercept = 10^stats::predict(model, data.frame(measure=GFP_gain)), linetype=2)+
      ggplot2::geom_point(ggplot2::aes(x=GFP_gain, y=10^stats::predict(model, data.frame(measure=GFP_gain))),
                          colour="red", shape=1, size=2)+
      ggplot2::scale_x_continuous("Gain")+
      ggplot2::scale_y_continuous("Conversion factor (MEFL/a.u.)", trans = "log10")+
      ggplot2::theme_bw()

    # ggplot2::ggsave('plot-GFP-conversion-factor.pdf')
  }

  pr_data$calibrated_OD <- pr_data$normalised_OD / OD_cf
  pr_data$calibrated_GFP <- pr_data$normalised_GFP / GFP_cf

  return(pr_data)

}
