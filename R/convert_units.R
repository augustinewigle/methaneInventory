#' Perform conversions to kg/h for common units
#' @param emission_rates vector of emission rates
#' @param units a string describing the units of the emission rate
#' @returns A vector of the emission rates converted to kg/h as required for calc_inventory
convert_units <- function(emission_rates, units) {

  dictionary <- rbind(c("m3/d", 0.028265),
                      c("m^3/d", 0.028265),# In thesis/methane inventory post proposal/m3/day to kg/h
                      c("kt/y", 1/0.00876),
                      c("kg/h", 1))


  return(emission_rates*dictionary[which(dictionary[,1] == units),2])


}
