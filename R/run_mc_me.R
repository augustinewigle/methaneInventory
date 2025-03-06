#' Run Monte Carlo Simulation
#' @param data_frame a data_frame that contains all information on the relevant dataset
#' @param est_type string indicating which estimator should be used to aggregate pass data, either "ipw" or "hajek"
#' @param n_sim the number of times we want to simulate y values. Must be specified if with_me = T
#' @param truncate_pod value at which to truncate probabilities of detection for small probabilities. Default is 0.02
#' @param stage2_design Number of days in the population at stage 2. 365 for a year, or set to 0 to ignore variance contributino from stage 2.
#' @param progress should progress be printed?
#' @importFrom stats var
#' @export
run_mc_me <- function(data_frame,
                      # use_true_pod,
                      # true_pod_vec = NULL,
                      est_type,
                      n_sim,
                      truncate_pod = 0.02,
                      stage2_design, progress = T) {

  # Set up things to save data
  strata_total_ests <- as.data.frame(matrix(nrow = n_sim, ncol = length(unique(data_frame$stratum))))
  strata_total_var_ests <- strata_total_ests
  strata_total_stage1var_ests <- strata_total_ests
  strata_total_stage2var_ests <- strata_total_ests
  strata_total_stage3var_ests <- strata_total_ests

  inc <- max(1, floor(n_sim*0.2))

  # Draw all the new y's that you are going to need ahead of time

  yvec <- rep(data_frame$Y, n_sim)

  new_y <- matrix(draw_y(yvec), byrow = F, ncol = n_sim)

  data_frame$Ymean <- apply(new_y, 1, mean)

  # data_frame$pod <- true_pod_vec # if use_true_pod = F, pod will get overwritten at each mc iteration

  for(b in 1:n_sim) {

    data_frame$Ytrue <- new_y[,b]

    # if(use_true_pod) {
    #
    #   new_ydata <- mutate(data_frame, detected = Y > 0) %>%
    #    group_by(component_id, day) %>%
    #     mutate(r = sum(detected)>0,
    #            phi = -1000)
    #
    #   if(est_type == "hajek") {
    #
    #     new_ydata <- new_ydata %>% mutate(phi = estimate_phi(num_passes,
    #                                                          p = pod,
    #                                                          truncate = 0))
    #
    #   }


    # } else {

      new_ydata <- mutate(data_frame, detected = Y > 0) %>%
        mutate(pod = pod_func(Ytrue = Ymean, u = u, h = h, truncate = truncate_pod)) %>%
        group_by(component_id, day) %>%
        mutate(r = sum(detected)>0,
               phi = -1000)

      if(est_type == "hajek") {

        new_ydata <- new_ydata %>% mutate(phi = estimate_phi(num_passes,
                                                             p = pod,
                                                             truncate = 0))

    }



    stratum_totals <- do_one_inventory(data = new_ydata,
                                       est_type = est_type)

    # save things - overall total estimate and variance, stratum total estimates and variances
    strata_total_ests[b,] <- stratum_totals$point_est_kty
    strata_total_var_ests[b,] <- stratum_totals$designvar_kty
    strata_total_stage1var_ests[b,] <- stratum_totals$stage1var_kty
    strata_total_stage2var_ests[b,] <- stratum_totals$stage2var_kty
    strata_total_stage3var_ests[b,] <- stratum_totals$stage3var_kty

    if(b%%inc == 0 & progress) {

      print(paste0(b/n_sim*100, "% complete"))

    }
  }

  if(progress) {print("Post-processing...")}

  names(strata_total_ests) <- names(strata_total_var_ests) <- names(strata_total_stage1var_ests) <- names(strata_total_stage2var_ests) <- stratum_totals$stratum

  # debugging

  # print(paste0("number of negative stage1var ests = ", sum(strata_total_stage1var_ests < 0)))
  # print(paste0("number of negative stage2var ests = ", sum(strata_total_stage2var_ests < 0)))
  # print(paste0("number of negative stage3var ests = ", sum(strata_total_stage3var_ests < 0)))
  # print(strata_total_stage1var_ests)
  # print(strata_total_stage2var_ests)

  # Calculate total est and var for each iteration
  pop_total_ests <- apply(strata_total_ests, 1, sum)
  pop_total_var_ests <- apply(strata_total_var_ests, 1, sum)
  pop_total_stage1var_ests <- apply(strata_total_stage1var_ests, 1, sum)
  pop_total_stage2var_ests <- apply(strata_total_stage2var_ests, 1, sum)
  pop_total_stage3var_ests <- apply(strata_total_stage3var_ests, 1, sum)

  # calculate point estimates and the variance - var(vector of total estimates) + mean(vector of variance of total estimates)

  stratum_summary <- data.frame(stratum = names(strata_total_ests),
                                point_est_kty = apply(strata_total_ests, 2, mean),
                                measurementvar_kty = apply(strata_total_ests, 2, var),
                                designvar_kty = apply(strata_total_var_ests, 2, mean),
                                stage1var_kty = apply(strata_total_stage1var_ests, 2, mean),
                                stage2var_kty = apply(strata_total_stage2var_ests, 2, mean),
                                stage3var_kty = apply(strata_total_stage3var_ests, 2, mean))
  pop_summary <- data.frame(point_est_kty = mean(pop_total_ests),
                            measurementvar_kty = var(pop_total_ests),
                            designvar_kty = mean(pop_total_var_ests),
                            stage1var_kty = mean(pop_total_stage1var_ests),
                            stage2var_kty = mean(pop_total_stage2var_ests),
                            stage3var_kty = mean(pop_total_stage3var_ests))

  stratum_summary$totvar_kty <- stratum_summary$measurementvar_kty + stratum_summary$designvar_kty
  pop_summary$totvar_kty <- pop_summary$measurementvar_kty + pop_summary$designvar_kty

  if(progress) { print("Complete!") }

  return(list(stratum_summary = stratum_summary,
              pop_summary = pop_summary,
              convergence_checking = strata_total_var_ests))

}
