stratum_variance_func <- function(nh, Nh, facility_ids, psu_means, psu_vars, ssu_only = F) {

  if(ssu_only) {

    return(sum(Nh^2/nh^2*psu_vars))

  } else {

    cov_mat <- calc_strat_matrix(facility_ids = facility_ids,
                                 psu_means = psu_means,
                                 nh = nh,
                                 Nh = Nh,
                                 dimension = length(facility_ids))

    return(sum(cov_mat) + sum(Nh/nh*psu_vars))

  }

}
