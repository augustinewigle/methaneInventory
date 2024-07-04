#' Impute probabilites and estimate phi
#' @param p vector of known probabilities
#' @param num_passes total number of passes
#' @param truncate if estimated phi is less than truncate it is set to truncate
#' @return the estimate of phi
#' @export
estimate_phi <- function(p, num_passes, truncate = 0) {

  imputed_p <- ifelse(!is.na(p), mean(p), NA)

  phi <- 1-(1-imputed_p)^(num_passes-length(p))*prod(1-p)

  return(ifelse(phi < truncate, truncate, phi))

}
