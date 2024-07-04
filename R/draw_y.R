#' simulate a true y value given a msmt
#' @param Ymeas a vector of measured Y values
#' @return A vector of Y values drawn from the distribution. Y values that were equal to zero stay as zero, negative values return NaN.
#' @importFrom flexsurv rllogis
draw_y <- function(Ymeas) {

  true <- flexsurv::rllogis(length(Ymeas),
                    scale = Ymeas*0.891*0.918, # scale corresponds to alpha
                    shape = 3.82) # shape corresponds to beta

  true[which(Ymeas == 0)] <- 0

  return(true)

}
