#' Take a calc_inventory object and produce CIs for the pop total, and desired individual strata, and variance breakdowns
#' @param result the result of calc_inventory
#' @param strata logical; T if CIs for strata are desired
#' @param pop logical; T if CIs for population are desired
#' @param strata_names vector of string names of strata to produce CIs for; required if strata = TRUE. Can set to "ALL" if CIs for all strata are desired
#' @param conf_level confidence level
#' @param var_decomp Logical; should each variance source be reported separately? Default is TRUE.
#' @importFrom tidyr pivot_longer
#' @importFrom  stats qnorm
#' @export
get_inventory_info <- function(result,
                               strata = TRUE,
                               pop = TRUE,
                               strata_names = "ALL",
                               var_decomp = TRUE,
                               conf_level=0.95) {

  pop_summary <- data.frame(stratum = "Population", result$pop_summary)
  stratum_summary <- result$stratum_summary
  alpha <- (1-conf_level)/2

  if(strata & strata_names[1] != "ALL") {

    stratum_summary <- stratum_summary %>% subset(stratum %in% strata_names)

  }

  made_summary <- rbind(pop_summary, stratum_summary)

  if(!pop) {

    made_summary <- subset(made_summary, stratum != "Population")

  }

  if(!strata) {

    made_summary <- subset(made_summary, stratum == "Population")

  }

  if(nrow(made_summary) == 0) {

    stop("One or both of `strata` or `pop` must be TRUE.")

  }

  pop_info <- pivot_longer(made_summary, cols = ends_with("var_kty"),
                           names_pattern = "^(.*?)(?=var_kty)",
                           names_to = "vartype",
                           values_to = "var")
  if(var_decomp == FALSE) {

    ret <- pop_info %>%
      subset(vartype == "tot") %>% mutate(lower = point_est_kty - qnorm(1-alpha)*sqrt(var),
                                          upper = point_est_kty + qnorm(1-alpha)*sqrt(var),
                                          length = upper-lower)

  } else {

    ret <- pop_info %>%
      mutate(lower = point_est_kty - qnorm(1-alpha)*sqrt(var),
             upper = point_est_kty + qnorm(1-alpha)*sqrt(var),
             length = upper-lower) %>%
      group_by(stratum) %>% mutate(var_perc = var/var[vartype == "tot"]*100) %>% ungroup()

  }



  return(ret)

}
