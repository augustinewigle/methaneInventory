#' Take a calc_inventory object and produce CIs for the pop total, and desired individual strata, and variance breakdowns
#' @param result the result of calc_inventory
#' @param strata logical; T if CIs for strata are desired
#' @param strata_names vector of string names of strata to produce CIs for; required if strata = T. Can set to "ALL" if CIs for all strata are desired
#' @param conf_level confidence level
#' @importFrom tidyr pivot_longer
#' @importFrom  stats qnorm
#' @export
get_inventory_info <- function(result,
                               strata = T,
                               strata_names = "ALL",
                               conf_level=0.95) {

  pop_summary <- data.frame(stratum = "Population", result$pop_summary)
  stratum_summary <- result$stratum_summary
  alpha <- (1-conf_level)/2

  if(strata) {

    pop_summary <- rbind(pop_summary, stratum_summary)

    if(strata_names[1] != "ALL") {

      pop_summary <- pop_summary %>% subset(stratum %in% c("Population", strata_names))

    }



  }

  pop_info <- pivot_longer(pop_summary, cols = ends_with("var_kty"),
                           names_pattern = "^(.*?)(?=var_kty)",
                           names_to = "vartype",
                           values_to = "var") %>%
    mutate(lower = point_est_kty - qnorm(1-alpha)*sqrt(var),
           upper = point_est_kty + qnorm(1-alpha)*sqrt(var),
           length = upper-lower) %>%
    group_by(stratum) %>% mutate(var_perc = var/var[vartype == "tot"]*100) %>% ungroup()


  return(pop_info)

}
