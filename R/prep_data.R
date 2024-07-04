#' name data columns the right way
#' @param raw_data data.frame containing the data
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
#' @return dataframe with properly named columns and the additional column num_passes
#' @export
prep_data <- function(raw_data,
                      component_name,
                      facility_name,
                      stratum_name,
                      day_name,
                      nh_name,
                      Nh_name,
                      Y_name, u_name, h_name,
                      num_wells_name) {

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

  # if(!is_na(day_pop_name)) {
  #
  #   new_data <- new_data %>% mutate(Di = !!as.symbol(day_pop_name))
  #
  # }

  return(new_data)

}
