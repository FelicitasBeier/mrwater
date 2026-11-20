#' @title       toolHarmonizationYear
#' @description Returns the end of the harmonization period. Until this year,
#'              all water scenarios follow one common path (ssp2, EFP off),
#'              committed irrigated area is fully reserved and
#'              missing water is attributed to non-renewable groundwater.
#'              Afterwards, scenarios diverge.
#'              Must agree with sm_fix_SSP2 in MAgPIE.
#'
#' @return       numeric year
#' @author       Felicitas Beier
#'
#' @examples
#' toolHarmonizationYear()
#'
#' @export

toolHarmonizationYear <- function() {
  return(2025)
}
