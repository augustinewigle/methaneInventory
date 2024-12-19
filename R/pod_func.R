#' Calculate the probability of detection using the function in Conrad et al 2023
#' @param Ytrue true value of emission rate in kg/h
#' @param h height in m
#' @param u mind speed in m/s
#' @param truncate truncate probabilities less than this value
#' @export
pod_func <- function(Ytrue, h, u, truncate = 0.01) {

  ps <- exp(-1*((0.224*Ytrue^(1.07)/((h/1000)^(2.44)*(u+2.14)^(1.69)))^(-2.53)))
  # ps <- exp(-1*((0.244*Ytrue^(1.07)/((h/1000)^(2.44)*(u+2.14)^(1.69)))^(-2.53)))

  ps[which(ps < truncate)] <- truncate

  return(ps)

}

