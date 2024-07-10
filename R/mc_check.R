#' Make plots to assess the number of MC iterations
#' @param inventory an inventory obtained by running calc_inventory which had measurement error
#' @param ... additional arguments passed to par
#' @importFrom stringr str_trunc
#' @importFrom graphics par
#' @export
mc_check <- function(inventory, ...) {

  if(is.null(inventory$convergence_checking)) {

    stop("No Monte Carlo used in this inventory. No need to check convergence.")

  }

  inc <- ceiling(nrow(inventory$convergence_checking)/1000)

  # set up data.frame
  calc <- seq(2, nrow(inventory$convergence_checking), by = inc)
  mc_df <- matrix(nrow = length(calc), ncol = ncol(inventory$convergence_checking)+1)
  for(i in 1:length(calc)) {

    mc_df[i,1] <- calc[i]

    mc_df[i,2:ncol(mc_df)] <- apply(inventory$convergence_checking[1:calc[i],], 2, mean)

  }

  # plots
  nstrata <- ncol(inventory$convergence_checking)
  par(mfrow = c(4, ceiling(nstrata/4)), mar = c(4, 6, 2, 1), ...)

  for(strata in 1:ncol(inventory$convergence_checking)) {

    p <- plot(mc_df[,1], mc_df[,strata+1], type = "l", col = "navyblue",
              main = str_trunc(names(inventory$convergence_checking)[strata],
                              width = 15),
              xlab = "MC iteration",
              ylab = expression(tilde(V)^{3~stage}))
    invisible(p)

  }


}
