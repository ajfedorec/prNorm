#' Generate Conversion Factors
#' Make sure the dplyr and ggplot2 libraries are installed.
#'
#' @param calibration_dir path of the directory in which your calibration data is held
#' @param date input date as YYYYMMDD on which calibration was carried out. n.b. this should also be the name of the folder holding the csv files
#'
#' @return saves a csv data frame with the conversion factors and two png plots of the conversion factors (for Absorbance and fluorescence)


generate_cfs<-function(calibration_dir, date){

  `%>%` <- magrittr::`%>%` #loading %>% operator from magrittr package

  csv_files <- c("film.csv", "nofilm.csv")
  plate_layout <- read.csv(paste(calibration_dir, "calibration_plate_layout.csv", sep = "/"))

  #### Parse data ####
  all_values <- c()
  for (csv_file in csv_files) {
    csv_data <- read.table(paste(calibration_dir, date, csv_file, sep="/"),
                           sep = ",", blank.lines.skip = T, header = F, stringsAsFactors = F) #read the csv file

    start_time_idx <- which(csv_data[,1] == "Start Time") #get start and end time ids
    end_idx <- which(csv_data[,1] == "End Time")
    names_idx <- which(csv_data[,1] == 'Name')
    names_idx <- names_idx[2:length(names_idx)] #remove the first start time entry which just details plate type
    end_of_file <- F

    lid_type <- gsub(pattern = ".csv", replacement = "", x = csv_file)
    for (i in 1:length(start_time_idx)) {
      block_name <- csv_data[names_idx[i], 2] # record name of what is being measured

      block_start <- start_time_idx[i]+4 # find start and end of measurement block
      block_end_idx <- end_idx[i]-3

      new_block <- csv_data[(block_start):(block_end_idx),1:2] # grab and name the data
      names(new_block)[1] <- "well"
      names(new_block)[2] <- "value"

      joined_block <- dplyr::full_join(plate_layout, new_block) #join to plate layout csv, add measurement category
      joined_block$measure <- block_name
      joined_block$lid_type <- lid_type

      all_values <- rbind(all_values, joined_block) #add to all data
    }
  }

  ### remove saturating values and calculate mean of 4 replicates ###
  all_values$value <- as.numeric(all_values$value)
  summ_values <- all_values %>%
    dplyr::filter(!is.na(value)) %>%
    dplyr::group_by(measure, concentration, Calibrant, lid_type) %>%
    dplyr::summarise(mean_value = mean(value))

  ### remove unnecessary observations ###
  summ_values <- summ_values %>%
    dplyr::filter(((Calibrant=="microspheres") & ((measure == "Abs600") | (measure == "Abs700"))) |
                    ((Calibrant=="fluorescein") & ((measure != "Abs600") & (measure != "Abs700"))))

  ## normalise data
  norm_values <- summ_values %>%
    dplyr::group_by(measure, Calibrant, lid_type) %>%
    dplyr::mutate(normalised_value = mean_value - mean_value[concentration == 0]) %>%
    dplyr::filter(((Calibrant == "fluorescein") & (concentration != 0)) |             #exclude fluorescein concentration of 0 to avoid dividing by 0
                    ((Calibrant == "microspheres") & (concentration > 8.139678e+06))) #only take the top 8 data points to avoid low concentration variability

  # MEFL_per_uM <- 6.02E+13 #MEFL/uM fluorescein (value from Jacob Beal's iGEM conversion excel spreadsheet)

  ## fit linear models to the data (grouped by lid_type, measure and Calibrant)
  fit_values <- norm_values %>%
    dplyr::group_by(lid_type, measure, Calibrant) %>%
    dplyr::do(broom::tidy(lm(normalised_value~concentration, data = .))) %>% # fit linear model (lm) and extract coefficients (broom::tidy)
    dplyr::select(1:5) %>%                                                   # only keep useful columns
    tidyr::spread(term, estimate) %>%                                        # spread the data so we have a column for intercept coeffs and one for slope coeffs
    dplyr::rename("intercept"=`(Intercept)`, "slope"=concentration)

  ### plot the mean normalized values ###
  Abs_plot <-
    ggplot2::ggplot(data=norm_values %>% dplyr::filter(Calibrant == "microspheres"),
                    ggplot2::aes(x=concentration, y=normalised_value)) +
    ggplot2::geom_point() +
    ggplot2::geom_smooth(method='lm', formula=y~x) + #fit a line
    # ggplot2::geom_abline(data = fit_values %>% dplyr::filter(Calibrant == "microspheres"),
    #                      ggplot2::aes(slope = slope, intercept = intercept), colour="red") +
    ggplot2::scale_y_continuous('Normalised Absorbance', trans="log10")+
    ggplot2::scale_x_continuous('Number of Microspheres', trans="log10")+
    ggplot2::facet_grid(measure ~ lid_type) +
    ggplot2::theme_bw(base_size = 12)

  Fluo_plot <-
    ggplot2::ggplot(data=norm_values %>% dplyr::filter(Calibrant == "fluorescein"),
                    ggplot2::aes(x=concentration, y=normalised_value)) +
    ggplot2::geom_point() +
    ggplot2::geom_smooth(method='lm', formula=y~x) +
    ggplot2::scale_y_continuous('Normalized Fluorescence (a.u.)', trans="log10")+
    ggplot2::scale_x_continuous('Fluorescein Concentration (uM)', trans="log10")+
    # ggplot2::geom_abline(data = fit_values %>% dplyr::filter(Calibrant == "fluorescein"),
    #                      ggplot2::aes(slope = slope, intercept = intercept), colour="red") +
    ggplot2::facet_grid(measure ~ lid_type) +
    ggplot2::theme_bw(base_size = 12)

  #save conversion factors to a csv
  write.csv(fit_values, paste(calibration_dir, date, 'cfs_generated.csv', sep='/'), row.names = FALSE)

  #save plots
  ggplot2::ggsave(paste(calibration_dir, date, 'Absorbance_conversion_factors.pdf', sep='/'), plot=Abs_plot)
  ggplot2::ggsave(paste(calibration_dir, date, 'Fluorescence_conversion_factors.pdf', sep='/'), plot=Fluo_plot, width =12, height = 25, units='cm')

  return()
}
