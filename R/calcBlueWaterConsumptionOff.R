#' @title       calcBlueWaterConsumptionOff
#' @description This function calculates consumptive blue water use for the whole year based on
#'              LPJmL blue water consumption of crops and the difference between rainfed and irrigated
#'              evapotranspiration of grass
#'
#' @param selectyears   Years to be returned
#' @param iniyear       Initialization year for filtering rules of data for regression
#' @param lpjml         LPJmL version required for respective inputs: natveg or crop
#' @param climatetype   Climate model (e.g., "MRI-ESM2-0:ssp370")
#'                      or historical baseline (e.g., "GSWP3-W5E5:historical")
#'
#' @return magpie object in cellular resolution
#' @author Felicitas Beier, Jens Heinke
#'
#' @examples
#' \dontrun{
#' calcOutput("BlueWaterConsumptionOff", aggregate = FALSE)
#' }
#'
#' @importFrom magclass getItems collapseNames
#' @importFrom madrat calcOutput getFromComment
#' @importFrom stats lm coef

calcBlueWaterConsumptionOff <- function(selectyears, iniyear,
                                        lpjml, climatetype) {

  ####################
  ### Read in data ###
  ####################
  # crop blue water consumption in main irrigated growing period (in m^3/ha)
  bwc1st <- calcOutput("BlueWaterConsumptionMain", selectyears = selectyears,
                       lpjml = lpjml, climatetype = climatetype, aggregate = FALSE)
  unit <- getFromComment(bwc1st, "unit")
  # crop evapotranspiration under irrigated conditions in irrigated season
  etCropIRgrper <- calcOutput("Evapotranspiration", runtype = "cft:ir",
                              lpjml = lpjml, climatetype = climatetype,
                              selectyears = selectyears,
                              aggregate = FALSE)
  # crop evapotranspiration without irrigation in irrigated season
  etCropNOIRgrper <- calcOutput("Evapotranspiration", runtype = "cft:noir",
                                lpjml = lpjml, climatetype = climatetype,
                                selectyears = selectyears,
                                aggregate = FALSE)
  # grass evapotranspiration under irrigated conditions in irrigated season
  ### To Do: will be replaced with calcGrassET
  etGrassIRgrper <- calcOutput("Evapotranspiration",
                               lpjml = lpjml, climatetype = climatetype,
                               runtype = "grass:ir",
                               selectyears = selectyears,
                               aggregate = FALSE)
  # grass evapotranspiration without irrigation in irrigated season
  etGrassNOIRgrper <- calcOutput("Evapotranspiration",
                                 lpjml = lpjml, climatetype = climatetype,
                                 runtype = "grass:noir",
                                 selectyears = selectyears,
                                 aggregate = FALSE)
  # yearly grass evapotranspiration (grassland cft grows throughout the whole year)
  etGrassIRyear <- etCropIRgrper[, , "grassland"]
  etGrassNOIRyear <- etCropNOIRgrper[, , "grassland"]

  ###################
  ### Filter data ###
  ###################
  # multiple cropping suitability under irrigated conditions
  mcSuit <- collapseNames(calcOutput("MulticroppingSuitability", selectyears = iniyear,
                                     lpjml = lpjml, climatetype = climatetype,
                                     suitability = "endogenous", sectoral = "lpj",
                                     aggregate = FALSE)[, , "irrigated"])
  # off-season yields
  yldSingle <- setYears(calcOutput("YieldsLPJmL", selectyears = iniyear,
                          lpjml = lpjml, climatetype = climatetype,
                          multicropping = FALSE,
                          aggregate = FALSE)[, , "irrigated"], iniyear)
  yldMultiple <- setYears(calcOutput("YieldsLPJmL", selectyears = iniyear,
                            lpjml = lpjml, climatetype = climatetype,
                            multicropping = "TRUE:potential:endogenous",
                            aggregate = FALSE)[, , "irrigated"], iniyear)
  yldOffSeason <- collapseNames(yldMultiple - yldSingle)[, , getItems(mcSuit, dim = 3)]

  # initialize object for filtering
  naCells <- bwc1st
  naCells[, , ] <- 0
  # where no multicropping suitability under irrigated conditions
  naCells[!mcSuit] <- 1
  # where no yield increase through multiple cropping
  naCells[yldOffSeason <= 0] <- 1

  ####################
  ### Calculations ###
  ####################
  # crop blue water consumption in main growing season
  bconsCrop <- bwc1st
  # grass blue water consumption in main growing season
  bconsGrass <- collapseNames(etGrassIRgrper - etGrassNOIRgrper)

  # crop lists
  missingCrps <- setdiff(getItems(bconsCrop, dim = "crop"), getItems(bconsGrass, dim = "crop"))
  lpj         <- intersect(getItems(bconsCrop, dim = "crop"), getItems(bconsGrass, dim = "crop"))

  # select crops
  naCells    <- naCells[, , lpj]
  bconsCrop  <- bconsCrop[, , lpj]
  bconsGrass <- bconsGrass[, , lpj]

  # Apply filtering
  bconsCrop[naCells] <- NA
  bconsGrass[naCells] <- NA

  # Regression (linear fit between crop blue water consumption
  # by irrigation system and grass evapotranspiration) to derive
  # coefficient used to derive off-season blue water consumption
  # Dependent variable (y): crop blue water consumption for given system in main season
  # Independent variable (x): grass ET in irrigated growing period of respective crop
  a <- b <- new.magpie(cells_and_regions = getItems(bconsGrass, dim = 1),
                       years = getItems(bconsGrass, dim = 2),
                       names = getItems(bconsCrop, dim = 3),
                       fill = NA)
  tmp1 <- getItems(bconsCrop, dim = 3.1)
  tmp2 <- getItems(bconsCrop, dim = 3.2)
  # regression is executed for each year since the relationship can change over time
  for (yr in selectyears) {
    # regression is executed for each crop and each irrigation system separately
    for (i in getItems(bconsCrop, dim = 3)) {
      # i is the combination of crop and irrigation system
      # For grass, select crop only crop
      if (any(grepl("drip", tmp1))) {
        # remove irrigation system dimension (dim 3.1)
        j <- gsub("^[^.]*\\.", "", i)
      } else if (any(grepl("drip", tmp2))) {
        # remove irrigation system dimension (dim 3.2)
        j <- gsub("\\..*$", "", i)
      } else {
        stop("Wrong dimensionality in object used in regression of calcBlueWaterConsumptionOff.")
      }
      # Linear regression
      fit <- stats::lm(y ~ x, na.action = "na.omit",
                data = data.frame(y = as.vector(bconsCrop[, yr, i]),
                                  x = as.vector(bconsGrass[, yr, j])))
      # Extract intercept and slope coefficient
      a[, yr, i] <- stats::coef(fit)[1]
      b[, yr, i] <- stats::coef(fit)[2]
    }
  }

  ### To do: write out coefficients (a, b, r2, rse) for checking
  ### Check with Jan (toolExpect warning/note?)

  # grass blue water consumption in the entire year
  bconsGrassYr <- etGrassIRyear - etGrassNOIRyear

  # Second season blue water consumption of grass ("off season")
  grassBWC2nd <- bconsGrassYr - bconsGrass         ### To Do: check whether dimensions work out.
  # Second season blue water consumption of crop ("off season")
  bwc2nd <- a + b * grassBWC2nd                    ### To Do: check whether dimensions work out.
  # Exclude cells with low yields in second season
  bwc2nd[naCells] <- 0     #### To Do: double-check whether object dimensions are correct

  #### To Do: check whether perennials all got 0 BWC in second season (should be the case via N/A rules above!)

  # add missing crops and assign them 0 (no multiple cropping for these)
  noBWC2nd <- new.magpie(cells_and_regions = getItems(bwc2nd, dim = 1),
                         years = getItems(bwc2nd, dim = 2),
                         names = missingCrps,
                         fill = 0)
  getSets(noBWC2nd) <- getSets(bwc2nd)
  bwc2nd <- mbind(bwc2nd, noBWC2nd)

  ##############
  ### Checks ###
  ##############
  if (any(is.na(bwc2nd))) {
    stop("calcBlueWaterConsumptionOff produced NA values")
  }
  if (any(bwc2nd < 0)) {
    warning("calcBlueWaterConsumptionOff produced negative values")
    # ToDo: Check values and remove warning
  }
  # Correction of negative values
  bwc2nd[bwc2nd < 0] <- 0

  # Check system blue water consumption
  if (any(bwc2nd[, , "sprinkler"] < bwc2nd[, , "surface"])) {
    stop(paste0("Problem in calcBlueWaterConsumptionOff: ",
                "Sprinkler should always have greater blue water consumption than surface."))
  }
  if (any(bwc2nd[, , "surface"] < bwc2nd[, , "drip"])) {
    stop(paste0("Problem in calcBlueWaterConsumptionOff: ",
                "Surface should always have greater blue water consumption than drip"))
  }

  return(list(x = bwc2nd,
              weight = NULL,
              unit = unit,
              description = paste0("blue water consumption ",
                                   "in the off season ",
                                   "for three different irrigation systems"),
              isocountries = FALSE))
}
