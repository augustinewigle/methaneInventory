#' Pass-level Anonymous Aerial Survey Data from British Columbia, 2021
#'
#' A subset of the data used to create the methane inventory for the province of British Columbia, Canada in 2021 in Wigle and Beliveau.
#' The dataset combines published data from Johnson et al 2023 and confidential data from BC OGRIS and MERC.
#' For access to the full dataset, contact the MERC via Peter Kos \email{Peter.Kos@@gov.bc.ca}.
#'
#' \describe{
#'   \item{anonSiteID}{Anonymous site ID}
#'   \item{anonSourceID}{Anonymous unique identifier for component}
#'   \item{daysSinceInitial}{Number of days since the initial survey of that component. A value of zero indicates the initial survey.}
#'   \item{altitude_m}{Altitude of the plane in m at the time of the measurement}
#'   \item{windSpeed_ms}{Wind speed in m/s at the time of the measurement}
#'   \item{detected}{Logical - was methane detected or not?}
#'   \item{emissionRate_kgh}{Bridger's measured emission rate in kg/h. Note that when detected is TRUE and emissionRate_kgh is 0, this indicates a technical problem where methane was detected but could not be quantified. These are treated as undetected in the analysis.}
#'   \item{facility_id}{Anonymous unique identifier for the facility}
#'   \item{my_stratum}{Name of the stratum the component belongs to in the analysis in the article.}
#'   \item{num_wells}{The number of wells associated with the site in June 2021}
#'   \item{my_pop_excl, my_pop_incl}{The number of facilities in the population, excluding and including shut-in, respectively. See Appendix in the article for details.}
#'   \item{my_samp_excl, my_samp_incl}{The number of facilities in the sample, excluding and including shut-in, respectively. See Appendix in the article for details.}
#' }
#' @usage dat
"dat"
