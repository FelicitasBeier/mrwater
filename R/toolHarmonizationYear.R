#' @title       toolHarmonizationYear
#' @description Returns the end of the harmonization period. Until this year,
#'              all water scenarios follow one common path (ssp2, EFP off),
#'              committed irrigated area is fully reserved and
#'              missing water is attributed to non-renewable groundwater.
#'              Afterwards, scenarios diverge.
#'              Taken from the end of the LPJmL climate harmonization
#'              (refYearGcm in mrlandcore::toolLPJmLDefault), so that it is set in one place.
#'              Must agree with sm_fix_SSP2 in MAgPIE (checked in MAgPIE).
#'
#' @return       numeric year
#' @author       Felicitas Beier
#'
#' @examples
#' toolHarmonizationYear()
#'
#' @export

toolHarmonizationYear <- function() {
  return(as.integer(sub("y", "", mrlandcore::toolLPJmLDefault(suppressNote = TRUE)$refYearGcm)))
}
