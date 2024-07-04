#' Calculate HT style variance estimator
#' @param p vector of pass-level probabilities
#' @param phi estimate or true value of phi
#' @param Ytrue pass-level response values
#' @param n_pass the total number of passes
#' @export
ht_variance_est <- function(p, Ytrue, phi, n_pass) {

  mat <- calc_component_matrix(p = p, Ytrue = Ytrue, phi = phi, n_pass = n_pass)

  # for(k in 1:n_pass) {
  #
  #   for(l in 1:n_pass) {
  #
  #     if(k == l) {
  #
  #       mat[k,l] <- (phi/p[k])^2*(1-p[k]/phi)*Ytrue[k]^2
  #
  #     } else {
  #
  #       mat[k,l] <- phi*(1-1/phi)*Ytrue[k]*Ytrue[l]*phi/(p[k]*p[l])
  #
  #     }
  #
  #   }
  #
  # }

  v <- max(0, sum(mat)/n_pass^2)

  return(v)

}
