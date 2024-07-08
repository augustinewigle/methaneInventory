#' Pass-level Anonymous Aerial SUrvey Data from BC, 2021
#'
#' A subset of data from the anonymized dataset provided to the authors by BC oGRIS and MERC.
#' The dataset combines published data from Johnson et al 2023 with data provided by BC OGRIS and MERC.
#' For access to the full dataset, contact MERC via Peter Kos \email{Peter.Kos@@gov.bc.ca}.
#'
#' A data frame with 603 rows and 19 columns:
#' \describe{
#'   \item{anonSiteID}{Anonymized site ID}
#'   \item{anonSourceID}{Unique identifier for component}
#'   \item{daysSinceInitial}{Number of days since the initial survey of that component. A value of zero indicates the initial survey.}
#'   \item{altitude_m}{Altitude of the plane in m at the time of the measurement}
#'   \item{windSpeed_ms}{Wind speed in m/s at the time of the measurement}
#'   \item{detected}{Logical - was methane detected or not?}
#'   \item{emissionRate_kgh}{Bridger's measured emission rate in kg/h. Note that when detected is TRUE and emissionRate_kgh is 0, this indicates a technical problem where methane was detected but could not be quantified.}
#'   \item{facility_id}{Anonymous identified for the facility}
#'   \item{my_stratum}{Name of the stratum the component belongs to in the analysis in the article.}
#'   \item{num_wells}{The number of wells associated with the site in June 2021}
#'   \item{my_pop_excl, my_pop_incl}{The number of facilities in the population, excluding and including shut-in respectively}
#'   \item{my_samp_excl, my_samp_incl}{The number of facilities in the sample, excluding and including shut-in, respectively}
#' }
#' @usage dat
"dat"
