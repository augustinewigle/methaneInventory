#' Prepare raw data for analysis
#' @param raw_data data.frame containing the raw data (for example, use the dat object included in this package)
#' @param component_name name of the column with component identifiers
#' @param facility_name name of the column with facility identifiers
#' @param stratum_name name of the column with stratum names
#' @param day_name name of the column identifying the days
#' @param nh_name name of the column with stratum sample sizes
#' @param Nh_name name of the column with stratum population sizes
#' @param Y_name name of the column with emission rate measurements, in kg/h
#' @param u_name name of the column with wind speed measurements, in m/s
#' @param h_name name of the column with height measurements, in m
#' @param num_wells_name name of the column with the number of wells at the facility
#' @param add_day_pop Logical; default is FALSE, and no new columns are added
#' @param day_pop_value The number of days to use as the stage II population size when adding a column. Default is 365
#' @return dataframe compatible with calc_inventory; adds column num_passes, and optionally, additional column Di if add_day_pop = TRUE
#' @export
prep_data <- function(raw_data,
                      component_name,
                      facility_name,
                      stratum_name,
                      day_name,
                      nh_name,
                      Nh_name,
                      Y_name, u_name, h_name,
                      num_wells_name,
                      add_day_pop = F,
                      day_pop_value = 365) {

  new_data <- raw_data %>% transmute(component_id = !!as.symbol(component_name),
                                     facility_id = !!as.symbol(facility_name),
                                     stratum = !!as.symbol(stratum_name),
                                     day = !!as.symbol(day_name),
                                     nh = !!as.symbol(nh_name),
                                     Nh = !!as.symbol(Nh_name),
                                     Y = !!as.symbol(Y_name),
                                     u = !!as.symbol(u_name),
                                     h = !!as.symbol(h_name),
                                     num_wells = !!as.symbol(num_wells_name)) %>%
    group_by(day,component_id) %>% mutate(num_passes = n())

  if(add_day_pop) {

    new_data$Di <- day_pop_value

  }

  return(new_data)

}
