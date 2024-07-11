#' Perform conversions to kg/h for common units
#' @param emission_rates vector of emission rates
#' @param units a string describing the units of the emission rate
#' @returns A vector of the emission rates converted to kg/h as required for calc_inventory
#' @export
convert_units <- function(emission_rates, units) {

  dictionary <- data.frame(unit = c("m3/d", "m^3/d", "kt/y", "kg/h"),
                           multiplier = c(0.028265, 0.028265, 1/0.00876, 1))



  if(!(units %in% dictionary[,1])) {

    stop("Emission rate units are invalid. Possible units are m^3/d, kt/y, or kg/h.")

  }

  if(units == "kg/h") {

    message("Y_units are kg/h, no conversion done.")

  } else {

    message(paste0("Y_units are ", units, ", converting to kg/h" ))

  }


  return(emission_rates*dictionary[which(dictionary[,1] == units),2])


}
