
#'
component_ht_var <- function(Di, mi, Yid, Vid, phi) {

  temp <- phi
  ((Di-mi)/(mi*mi - mi)*sum(Yid*Yid) +
     (mi-Di)/(mi*mi*mi - mi*mi)*(sum(Yid))^2 +
     1/mi*sum(Vid))/Di

}

component_ht_mean <- function(mi, Yid, phi) {

  temp <- phi
  sum(Yid)/mi

}

component_hajek_var <- function(Di, mi, Yid, Vid, phi) {

  (1/(mi*mi - mi)*sum((Di-1-phi*mi+phi)*Yid*Yid/phi/phi) +
     (mi-Di)/(mi*mi*mi-mi*mi)*(sum(Yid/phi))^2 +
     1/mi*sum(Vid/phi))/Di

}

component_hajek_mean <- function(mi, Yid, phi) {

 sum(Yid/phi)/mi

}

