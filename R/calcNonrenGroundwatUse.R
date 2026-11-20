#' @title       calcNonrenGroundwatUse
#' @description Calculates current non-renewable (fossil) groundwater use
#'              as negative difference between
#'              water availability from renewable sources and
#'              human water use (consumption and withdrawal).
#'              Until the end of the harmonization period (toolHarmonizationYear),
#'              the missing water of the respective year is non-renewable groundwater.
#'              Afterwards, it is fixed to the missing water of the harmonization year.
#'
#' @param output        "total": total groundwater use (non-agricultural and agricultural),
#'                      "nonAg": groundwater use in non-agricultural sector (industry, domestic, electricity),
#'                      "comAg": groundwater use in agricultural sector (currently irrigated area)
#' @param lpjml         LPJmL version used
#' @param climatetype   Switch between different climate scenarios or
#'                      historical baseline "GSWP3-W5E5:historical"
#' @param selectyears   Years to be returned
#' @param iniyear       Initialization year
#' @param transDistGW   Water transport distance allowed to fulfill locally
#'                      unfulfilled water demand by surrounding cell water availability
#'                      to determine missing water that is tagged as non-renewable groundwater
#' @param multicropping Multicropping activated (TRUE) or not (FALSE) and
#'                      Multiple Cropping Suitability mask selected
#'                      (mask can be:
#'                      "none": no mask applied (only for development purposes)
#'                      "actual:total": currently multicropped areas calculated from total harvested areas
#'                                      and total physical areas per cell from LandInG
#'                      "actual:crop" (crop-specific), "actual:irrigation" (irrigation-specific),
#'                      "actual:irrig_crop" (crop- and irrigation-specific) "total"
#'                      "potential:endogenous": potentially multicropped areas given
#'                                              temperature and productivity limits
#'                      "potential:exogenous": potentially multicropped areas given
#'                                             GAEZ suitability classification)
#'                      (e.g. TRUE:actual:total; TRUE:none; FALSE)
#'
#' @return cellular magpie object
#' @author Felicitas Beier
#'
#' @examples
#' \dontrun{
#' calcOutput("NonrenGroundwatUse", aggregate = FALSE)
#' }
#'
#' @importFrom magclass collapseNames dimSums
#' @importFrom stringr str_split
#'
#' @export

calcNonrenGroundwatUse <- function(output, lpjml, climatetype,
                                   transDistGW = 100, multicropping = "TRUE:actual:irrig_crop",
                                   selectyears, iniyear) {

  # Non-renewable groundwater use is determined based on unfulfilled water demands of the past
  missingWat <- calcOutput("MissingWater", output = output,
                           lpjml = lpjml, climatetype = climatetype,
                           transDistGW = transDistGW, multicropping = multicropping,
                           selectyears = selectyears, iniyear = iniyear,
                           aggregate = FALSE)

  # Groundwater is calculated based on full water use of the past given all available water (EFP off).
  # Non-agricultural scenarios are identical in the past (ISIMIP history) and follow the common
  # SSP2 path until the end of the harmonization period. SSP2 is therefore used as representative case.
  scenNonAg  <- "ssp2"
  scenEFP    <- "off"
  missingWat <- collapseNames(missingWat[, , paste(scenEFP, scenNonAg, sep = ".")])

  # Missing water until the end of the harmonization period is attributed to non-renewable groundwater
  # (committed area is fully irrigated during the harmonization period).
  # Afterwards, it is fixed to the missing water of the harmonization year.
  harmYr <- toolHarmonizationYear()

  if (is.character(selectyears)) {
    selectyears <- as.numeric(gsub("y", "", selectyears))
  }
  if (is.character(iniyear)) {
    iniyear <- as.numeric(gsub("y", "", iniyear))
  }
  if (iniyear > selectyears[1]) {
    stop(paste0("Please align the initialization year ('iniyear') ",
                "with the selected years ('selectyears'). ",
                "The 'iniyear' has to be the first year of 'selectyears' ",
                "or lie in the past of it."))
  }
  if (iniyear > harmYr) {
    stop(paste0("Please align the initialization year ('iniyear') ",
                "with the end of the harmonization period (see toolHarmonizationYear()). ",
                "The 'iniyear' has to be smaller or equal to this year."))
  }
  # include iniyear in selectyears
  if (length(intersect(iniyear, selectyears)) < 1) {
    cmbYrs <- c(iniyear, selectyears)
  } else {
    cmbYrs <- selectyears
  }
  # selectyears of the harmonization period (including iniyear)
  pstYrs <- cmbYrs[cmbYrs <= harmYr]
  # selectyears of the future
  ftrYrs <- cmbYrs[cmbYrs > harmYr]
  if (length(ftrYrs) > 0 && !(harmYr %in% cmbYrs)) {
    stop(paste0("calcNonrenGroundwatUse: 'selectyears' must contain the end of the ",
                "harmonization period (", harmYr, ") to fix the future groundwater pool."))
  }

  ######################
  ### Prepare output ###
  ######################
  out <- new.magpie(fill = NA,
                    cells_and_regions = getItems(missingWat, dim = 1),
                    years = selectyears,
                    names = c("withdrawal", "consumption"))
  getSets(out) <- c("x", "y", "iso", "year", "wtype")

  # In the harmonization period, missing water is accounted as fossil groundwater
  out[, pstYrs, ] <- missingWat[, pstYrs, ]
  # In future time steps, fossil groundwater is held constant
  out[, ftrYrs, ] <- missingWat[, harmYr, ]

  ##############
  ### Checks ###
  ##############
  if (any(round(out, digits = 4) < 0, na.rm = TRUE)) {
    stop("calcNonrenGroundwatUse produced negative values")
  }
  # correct negatives below rounding imprecision
  out[out < 0] <- 0
  if (any(is.na(out))) {
    stop("calcNonrenGroundwatUse produced NAs")
  }

  return(list(x            = out,
              weight       = NULL,
              unit         = "mio. m^3",
              description  = paste0("usable non-renewable groundwater volume ",
                                    "for human consumption and withdrawal"),
              isocountries = FALSE))
}
