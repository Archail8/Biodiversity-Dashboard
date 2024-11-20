#' Global Biodiversity Information Facility species occurrence data
#'
#' A subset of data from the Global Biodiversity Information Facility limited
#' to what, when, where (in Poland only) and in which count was observed.
#'
#' @name occurrence_pl
#' @docType data
#' @format The \code{data.frame} with 48,461 rows and 6 columns:
#' \describe{
#'   \item{scientificName}{Scientific name of the specie}
#'   \item{vernacularName}{Common name of the specie}
#'   \item{individualCount}{Number of individuals observed}
#'   \item{longitudeDecimal}{Longitude of observation location}
#'   \item{latitudeDecimal}{Latitute of observation location}
#'   \item{eventDate}{Date on which individials were observed}
#' }
#' @source <https://www.gbif.org/occurrence/search?dataset_key=8a863029-f435-446a-821e-275f4f641165>