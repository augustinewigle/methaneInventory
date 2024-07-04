#' Calculate one inventory
#' @param data dataframe which has been pre-processed so it has phi, r calculated
#' @param est_type the estimate type
#' @import dtplyr
do_one_inventory <- function(data,
                 est_type) {

  # calculate stratum total and variance ests

  if(est_type == "ipw") {

    # calculate daily component estimators
    component_day <- lazy_dt(data) %>% group_by(component_id, day) %>%
      summarise(comp_day_mean = sum(Ytrue*detected/pod)/unique(num_passes),
                comp_day_var = sum(detected*(1-pod)/pod^2*Ytrue^2)/unique(num_passes)^2,
                stratum = unique(stratum),
                facility_id = unique(facility_id),
                num_wells = unique(num_wells),
                day = unique(day),
                phi = unique(phi),
                nh = unique(nh),
                Nh = unique(Nh),
                r = unique(r),
                num_days = unique(num_days),
                Di = unique(Di)) %>%
      mutate(stage3_var = comp_day_var) %>%
      as_tibble()

    comp_mean_func <- component_ht_mean
    comp_var_func <- component_ht_var

    }

  if(est_type == "hajek") {

    # calculate daily component estimators
    component_day <- lazy_dt(data) %>% group_by(component_id, day) %>%
      summarise(comp_day_mean = sum(Ytrue*detected/pod)/sum(detected/pod),
                comp_day_var = ht_variance_est(p = pod,
                                            Ytrue=(Ytrue-sum(Ytrue*detected/pod)/sum(detected/pod))*detected,
                                            phi=unique(phi),
                                            n_pass = unique(num_passes)), # edited this function so it prevents negative variances
                stratum = unique(stratum),
                facility_id = unique(facility_id),
                num_wells = unique(num_wells),
                day = unique(day),
                phi = unique(phi),
                nh = unique(nh),
                Nh = unique(Nh),
                r = unique(r),
                num_days = unique(num_days),
                Di = unique(Di)) %>% filter(r > 0) %>%
      mutate(stage3_var = comp_day_var/phi/phi) %>%
      as_tibble() # no var NAs at this point

    comp_mean_func <- component_hajek_mean
    comp_var_func <- component_hajek_var



  }

  # Fix wells - divide emissions among number of wells at site
  component_day_wells <- subset(component_day, stratum == "Wells") %>% mutate(comp_day_mean = comp_day_mean/num_wells,
                                                                              comp_day_var = comp_day_var/num_wells^2) %>%
    uncount(num_wells, .remove = F, .id = "new") %>% ungroup() %>%
    mutate(component_id = paste0(component_id,"_", new)) %>% select(-new)

  component_day_all <- bind_rows(subset(component_day, stratum != "Wells"), component_day_wells) %>%
    group_by(component_id) # %>% mutate(Di = !!as.symbol(day_pop_name))

  component_summary <- lazy_dt(component_day_all) %>% group_by(component_id) %>%
    summarise(comp_mean = unique(comp_mean_func(mi = unique(num_days), Yid = comp_day_mean, phi = phi)),
              vi_temp = unique(comp_var_func(Di = unique(Di), mi = unique(num_days),
                                             Yid = comp_day_mean, Vid = comp_day_var,
                                             phi = phi)),
              VIII_contrib = sum(stage3_var/num_days^2),# sum(stage3_var*Di^2/num_days^2),
              num_days = unique(num_days),
              any_r = sum(r) > 0,
              stratum = unique(stratum),
              nh = unique(nh),
              Nh = unique(Nh),
              facility_id = unique(facility_id),
              component_id = unique(component_id)) %>%
    # have to impute the variance for the ones which have only one day
    group_by(stratum) %>%
    mutate(vi_stratum_pooled = mean(vi_temp[which(num_days > 1)])) %>% # got rid of any_r > 0
    group_by(component_id) %>%
    mutate(vi = if_else(num_days > 1,
                        vi_temp,
                        vi_stratum_pooled)) %>% as_tibble()


  # stratum totals and variances (contribution from each stratum)
  stratum_totals <- component_summary %>% group_by(stratum) %>%
    summarise(point_est_kty = sum(unique(Nh/nh)*comp_mean)*0.00876,
              designvar_kty = 0.00876^2*stratum_variance_func(nh = unique(nh),
                                                           Nh = unique(Nh),
                                                           facility_ids = facility_id,
                                                           psu_means = comp_mean,
                                                           psu_vars = vi),
              stage3var_kty = 0.00876^2*sum(Nh/nh*VIII_contrib),
              stage2var_kty = max(0.00876^2*sum(Nh^2/nh^2*vi) - stage3var_kty, 0),# max(0.00876^2*sum(Nh^2/nh^2*vi) - stage3var_kty, 0),
              stage1var_kty = max(designvar_kty - stage2var_kty - stage3var_kty, 0), # might need max here
              designvar_kty = max(designvar_kty, stage1var_kty + stage2var_kty+stage3var_kty),
              totvar_kty = designvar_kty,
              measurementvar_kty = 0)

  return(stratum_totals)

}
