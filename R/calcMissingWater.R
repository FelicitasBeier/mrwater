#' @title       calcMissingWater
#' @description Calculates difference between requested water
#'              and water that can be fulfilled in algorithm
#'              given available surface water resources
#'              for all scenarios.
#'              This information is used to derive
#'              non-renewable groundwater use.
#'
#' @param output        sector to be reported:
#'                      non-agricultural sector water use ("nonAg"),
#'                      (committed) agricultural water use ("comAg"),
#'                      or both ("total")
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
#' calcOutput("MissingWater", aggregate = FALSE)
#' }
#'
#' @importFrom magclass collapseNames getItems<-
#' @importFrom magpiesets findset
#' @importFrom stringr str_split
#'
#' @export

calcMissingWater <- function(output, lpjml, climatetype,
                             transDistGW = 100, multicropping = "TRUE:actual:irrig_crop",
                             selectyears, iniyear) {

  # Multiple cropping as of current multiple cropping pattern
  if (as.logical(str_split(multicropping, ":")[[1]][1])) {
    m <- "TRUE:actual:irrig_crop"
  } else {
    m <- as.logical(str_split(multicropping, ":")[[1]][1])
  }

  # Only currently reported water use that cannot be fulfilled
  # even under a chosen transport distance is counted as non-renewable groundwater
  # Note: the transDistGW argument is separate from the transDist argument that can be
  #       used to vary surface water transport distance
  transDist <- transDistGW

  # Helper function: select withdrawal and consumption items of the routing output
  .select <- function(x, items) {
    x <- collapseNames(x[, , items])
    getItems(x, dim = 3.3) <- c("withdrawal", "consumption")
    return(x)
  }

  ##############################
  ### Results from Algorithm ###
  ##############################
  # Water demand as requested in the river routing (from reported current water uses)
  # and the part that can be fulfilled with renewable water sources (in mio. m^3)
  # Non-agricultural uses
  routingNAU <- calcOutput("RiverHumanUseAccounting",
                           iteration = "non_agriculture",
                           lpjml = lpjml, climatetype = climatetype,
                           efrMethod = "VMF:fair", transDist = transDist,
                           selectyears = selectyears, iniyear = iniyear,
                           multicropping = m,
                           accessibilityrule = NULL,
                           rankmethod = NULL, gainthreshold = NULL,
                           cropmix = NULL, yieldcalib = NULL,
                           irrigationsystem = NULL, landScen = NULL,
                           # comAg: must be set, no effect here (only used in "potential_irrigation")
                           comAg = FALSE,
                           aggregate = FALSE)
  actNAU       <- .select(routingNAU, c("demandWW", "demandWC"))
  fulfilledNAU <- .select(routingNAU, c("currHumanWWtotal", "currHumanWCtotal"))

  # Committed agricultural uses (including depreciation of irrigation equipment)
  routingCAU <- calcOutput("RiverHumanUseAccounting",
                           iteration = "committed_agriculture",
                           lpjml = lpjml, climatetype = climatetype,
                           efrMethod = "VMF:fair", transDist = transDist,
                           selectyears = selectyears, iniyear = iniyear,
                           multicropping = m,
                           accessibilityrule = NULL,
                           rankmethod = NULL, gainthreshold = NULL,
                           cropmix = NULL, yieldcalib = NULL,
                           irrigationsystem = NULL, landScen = NULL,
                           # comAg: must be set, no effect here (only used in "potential_irrigation")
                           comAg = FALSE,
                           aggregate = FALSE)
  actCAU       <- .select(routingCAU, c("demandWW", "demandWC"))
  fulfilledCAU <- .select(routingCAU, c("currHumanWWtotal", "currHumanWCtotal"))

  ################################
  ### Unfulfilled Water Demand ###
  ################################
  missingNAU <- actNAU - fulfilledNAU
  missingCAU <- actCAU - fulfilledCAU
  missingWat <- missingNAU + missingCAU

  ##############
  ### Output ###
  ##############
  if (output == "total") {
    out <- missingWat
  } else if (output == "nonAg") {
    out <- missingNAU
  } else if (output == "comAg") {
    out <- missingCAU
  } else {
    stop("Please select whether total groundwater use or sector-wise groundwater use
         shall be reported by mrwater::calcMissingWater")
  }

  ##############
  ### Checks ###
  ##############
  if (any(round(out, digits = 4) < 0, na.rm = TRUE)) {
    stop("mrwater::calcMissingWater produced negative values")
  }
  # correct negatives below rounding imprecision
  out[out < 0] <- 0

  if (any(is.na(out))) {
    stop("mrwater::calcMissingWater produced NAs")
  }
  if (any(round(missingCAU, digits = 4) < 0, na.rm = TRUE)) {
    stop("There are negatives in mrwater::calcMissingWater for committed agricultural uses")
  }
  if (any(round(missingNAU, digits = 4) < 0, na.rm = TRUE)) {
    stop("There are negatives in mrwater::calcMissingWater for non-agricultural uses")
  }

  return(list(x            = out,
              weight       = NULL,
              unit         = "mio. m^3",
              description  = paste0("missing water to fulfilled currently observed ",
                                    "(or committed) agricultural or non-agricultural ",
                                    "water consumption and withdrawal"),
              isocountries = FALSE))
}
