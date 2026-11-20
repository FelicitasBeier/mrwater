#' @title       calcIrrigAreaActuallyCommitted
#' @description calculates area reserved for irrigation based on area irrigated
#'              in initialization and available water resources
#'
#' @param lpjml         LPJmL version used
#' @param climatetype   Switch between different climate scenarios
#'                      or historical baseline "GSWP3-W5E5:historical"
#' @param selectyears   Years to be returned
#'                      (Note: does not affect years of harmonization or smoothing)
#' @param iniyear       Initialization year of irrigation system
#' @param efrMethod     EFR method used including selected strictness of EFRs
#'                      (Smakhtin:good, VMF:fair)
#' @param multicropping Multicropping activated (TRUE) or not (FALSE) and
#'                      Multiple Cropping Suitability mask selected
#'                      (mask can be:
#'                      "none": no mask applied (only for development purposes)
#'                      "actual:total": currently multicropped areas calculated from total harvested areas
#'                                      and total physical areas per cell from readLandInG
#'                      "actual:crop" (crop-specific), "actual:irrigation" (irrigation-specific),
#'                      "actual:irrig_crop" (crop- and irrigation-specific) "total"
#'                      "potential:endogenous": potentially multicropped areas given
#'                                              temperature and productivity limits
#'                      "potential:exogenous": potentially multicropped areas given
#'                                             GAEZ suitability classification)
#'                      (e.g. TRUE:actual:total; TRUE:none; FALSE)
#' @param transDist      Water transport distance allowed to fulfill locally
#'                       unfulfilled water demand by surrounding cell water availability
#' @param fossilGW       If TRUE: non-renewable groundwater can be used.
#'                       If FALSE: non-renewable groundwater cannot be used.
#'
#' @return magpie object in cellular resolution
#' @author Felicitas Beier
#'
#' @examples
#' \dontrun{
#' calcOutput("IrrigAreaActuallyCommitted", aggregate = FALSE)
#' }
#'
#' @importFrom madrat calcOutput
#' @importFrom magclass collapseNames collapseDim new.magpie getCells getNames

calcIrrigAreaActuallyCommitted <- function(lpjml, climatetype,
                                           selectyears, iniyear,
                                           efrMethod, fossilGW,
                                           multicropping, transDist) {

  ## Current Uses
  if (as.logical(stringr::str_split(multicropping, ":")[[1]][1])) {
    m <- "TRUE:actual:irrig_crop"
  } else {
    m <- FALSE
  }

  ######################
  ### Read in Inputs ###
  ######################
  # Read in physical cropland area (by crop) from crop area initialization (in Mha)
  comArea <- calcOutput("IrrigAreaCommitted",
                        selectyears = selectyears, iniyear = iniyear,
                        aggregate = FALSE)

  # Irrigation water requirements per cell per crop
  # given irrigation system (in m^3 per hectare per year)
  cropIrrigReq <- calcOutput("ActualIrrigWatRequirements",
                             irrigationsystem = "initialization",
                             selectyears = selectyears, iniyear = iniyear,
                             lpjml = lpjml, climatetype = climatetype,
                             multicropping = m, aggregate = FALSE)

  # Water already committed to irrigation (in mio. m^3)
  comWater <- calcOutput("RiverHumanUseAccounting",
                         iteration = "committed_agriculture",
                         lpjml = lpjml, climatetype = climatetype,
                         transDist = transDist, comAg = NULL,
                         efrMethod = efrMethod, multicropping = m,
                         selectyears = selectyears, iniyear = iniyear,
                         accessibilityrule = NULL,
                         rankmethod = NULL, gainthreshold = NULL,
                         cropmix = NULL, yieldcalib = NULL,
                         irrigationsystem = NULL, landScen = NULL,
                         aggregate = FALSE)
  comWatWWact <- collapseNames(comWater[, , "currHumanWWtotal"])
  comWatWCact <- collapseNames(comWater[, , "currHumanWCtotal"])

  ####################
  ### Calculations ###
  ####################
  # Total irrigation requirements per cell (in mio. m^3)
  totalIrrigReqWW <- collapseNames(dimSums(cropIrrigReq[, , "withdrawal"] * comArea,
                                           dim = "crop"))
  totalIrrigReqWC <- collapseNames(dimSums(cropIrrigReq[, , "consumption"] * comArea,
                                           dim = "crop"))

  # Share of Area that is irrigated given limited renewable water availability
  # Note: Areas that are reported to be irrigated
  wwShr <- ifelse(totalIrrigReqWW > 0,
                  comWatWWact / totalIrrigReqWW,
                  1)
  wcShr <- ifelse(totalIrrigReqWC > 0,
                  comWatWCact / totalIrrigReqWC,
                  1)

  ### Checks ###
  if (any(round(wwShr - wcShr, digits = 4) != 0)) {
    stop("There seems to be a mismatch in consumption and withdrawal
         in calcIrrigAreaActuallyCommitted.
         Please make sure that the fulfilled ratio is the same
         for consumption and withdrawal")
  }
  if (any(round(wcShr, digits = 4) > 1)) {
    stop("In calcAreaActuallyCommitted: Water requirements are over-fulfilled. ",
         "This should not be the case in the initialization year. Please double-check! ",
         "A wild guess: it might be related to the fossil groundwater calculation.")
  }

  # Area Actually Committed for Irrigation given available renewable water resource (in Mha)
  out <- comArea * collapseNames(wcShr)

  # Fossil groundwater use to fulfill committed agricultural water use
  if (fossilGW) {

    # Missing water: demand of committed area that cannot be fulfilled by renewable water (in mio. m^3)
    # (same routing and EFR setting as the renewable share above)
    missWW <- totalIrrigReqWW - comWatWWact
    missWC <- totalIrrigReqWC - comWatWCact
    # correct negatives below rounding imprecision
    missWW[missWW < 0] <- 0
    missWC[missWC < 0] <- 0

    # Fossil groundwater pool available for irrigation
    # (missing water of last year with data, EFP off and fixed transport distance of 100 km;
    # one pool for all scenarios)
    gw0 <- calcOutput("NonrenGroundwatUse",
                      output = "comAg", multicropping = m, transDistGW = 100,
                      lpjml = lpjml, climatetype = climatetype,
                      selectyears = selectyears, iniyear = iniyear,
                      aggregate = FALSE)

    # Only up to pool of fossil groundwater can be used to serve missing water.
    # Where less water required, only necessary amount is served.
    # The pool limits withdrawal and consumption jointly (same share of missing water for both),
    # because the consumption-withdrawal ratio of the pool (fixed year) differs from the current demand.
    shrGW <- pmin(ifelse(missWW > 0, collapseNames(gw0[, , "withdrawal"]) / missWW, 1),
                  ifelse(missWC > 0, collapseNames(gw0[, , "consumption"]) / missWC, 1))
    shrGW[shrGW > 1] <- 1
    rm(gw0)

    # Share of Area that is irrigated given limited water availability
    # under consideration of fossil GW
    wwShr <- collapseNames(ifelse(totalIrrigReqWW > 0,
                                  (comWatWWact + shrGW * missWW) / totalIrrigReqWW,
                                  1))
    wcShr <- collapseNames(ifelse(totalIrrigReqWC > 0,
                                  (comWatWCact + shrGW * missWC) / totalIrrigReqWC,
                                  1))

    # Checks
    if (any(round(wwShr - wcShr, digits = 4) != 0)) {
      stop("Fulfilled ratios of consumption and withdrawal differ in calcIrrigAreaActuallyCommitted ",
           "(largest difference: ", signif(max(abs(wwShr - wcShr)), 2), ")")
    }
    if (any(round(max(wcShr), digits = 6) > 1)) {
      stop("The actually irrigated area is over-fulfilled. This should not be the case.
           It may be related to the fossilGW argument. Please check mrwater::calcIrrigAreaActuallyCommitted.")
    }
    # Correct rounding imprecision
    wcShr[wcShr > 1] <- 1

    # Area Actually Committed for Irrigation given available water (in Mha)
    out <- comArea * pmin(wcShr, wwShr)
  }

  # Check for NAs and negative values
  if (any(is.na(out))) {
    stop("calcIrrigAreaActuallyCommitted produced NA irrigated areas")
  }
  if (any(round(out, digits = 6) < 0)) {
    stop("calcIrrigAreaActuallyCommitted produced negative irrigated areas")
  }
  # correct negatives below rounding imprecision
  out[out < 0] <- 0

  return(list(x            = out,
              weight       = NULL,
              unit         = "Mha",
              description  = "Cropland area reserved for irrigation per crop",
              isocountries = FALSE))
}
