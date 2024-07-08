#' Runs MC simulation to account for measurement bias and uncertainty
#' runs a monte carlo simulation to incorporate measurement bias and uncertainty into the stratum and pop estimates
#' @param data_frame a data_frame that contains all information on the relevant dataset
#' @param est_type string indicating which estimator should be used to aggregate pass data, either "ipw" or "hajek"
#' @param with_me logical indicating if measurement error should be incorporated or not
#' @param n_sim the number of times we want to simulate y values. Must be specified if with_me = T
#' @param truncate_pod value at which to truncate probabilities of detection for small probabilities. Default is 0.02
#' @param bias_corr constant to multiply all Y values by to correct for bias. Only used if with_me = F. Set to 1 if no bias correction is necessary.
#' @param consider_stageII Logical; should stage II uncertainty be considered? If TRUE, a simple random sample with population size derived from column Di in data_frame is used. Otherwise, ignored.
#' @return A list:
#'    stratum_totals - point estimates of stratum totals, in kt/y
#'    pop_total - point estimate of population total in kt/y
#'    stratum_var a data.frame with columns var_me (variance due to measurement error), var_de (variance due to the design), and var_total (total variance, var_me + var_de)
#'    pop_var same as stratum_var but for the population total
#' @import dtplyr
#' @import dplyr
#' @importFrom tidyr uncount
#' @export
calc_inventory <- function(data_frame,
                  est_type,
                  with_me = TRUE,
                  n_sim = NA,
                  truncate_pod = 0.02,
                  bias_corr = 0.918,
                  consider_stageII = TRUE) {

  if(with_me & is.na(n_sim)) {

    stop("`n_sim` must be specified if `with_me` = TRUE")

  }

  data_frame <- group_by(data_frame, component_id) %>% mutate(num_days = length(unique(day)))

  if(!consider_stageII) {

    data_frame <- mutate(data_frame, Di=num_days)


  } else {

    if(is.null(data_frame$Di)) {

      stop("data_frame must contain the column Di if consider_stageII = TRUE. Use prep_data with argument add_day_pop = TRUE and try again.")

    }

  }

  if(!(with_me)) {


    data <- mutate(data_frame, detected = Y > 0) %>%
      mutate(Ytrue = Y*bias_corr,
             pod = pod_func(Ytrue = Ytrue, u = u, h = h, truncate = truncate_pod)) %>%
      group_by(component_id, day) %>% mutate(phi = estimate_phi(num_passes,
                                                                p = pod,
                                                                truncate = 0),
                                             r = sum(detected)>0)
    # code to calculate things without measurement error
    stratum_totals <- do_one_inventory(data = data, est_type = est_type)

    pop_tots <- apply(stratum_totals[,-1], 2, sum)

    names(pop_tots) <- names(stratum_totals[,-1])

    return(list(stratum_summary = stratum_totals,
                pop_summary = data.frame(t(pop_tots))))



  } else {


    summaries <- run_mc_me(data_frame = data_frame,
                           est_type = est_type,
                           n_sim = n_sim,
                           truncate_pod = truncate_pod)


    return(summaries)

  }


}
