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

  ### To Do (discuss with Jens): we can replace calcEvapotranspiration with calcGrassET,
  ### but I still need grass ET for the growing period of the crop and grass ET for the entire year...
  ### What's the correct variable? Is it already written out?
  ### Decide which ones to include/exclude for next runs

  ###################
  ### Filter data ###
  ###################
  # multiple cropping suitability under irrigated conditions
  mcSuit <- collapseNames(calcOutput("MulticroppingSuitability", selectyears = iniyear,
                                     lpjml = lpjml, climatetype = climatetype,
                                     suitability = "endogenous", sectoral = "lpj",
                                     aggregate = FALSE)[, , "irrigated"])
  # off-season yields
  yldSingle <- calcOutput("YieldsLPJmL", selectyears = selectyears,
                          lpjml = lpjml, climatetype = climatetype,
                          multicropping = FALSE,
                          aggregate = FALSE)[, iniyear, "irrigated"]
  yldMultiple <- calcOutput("YieldsLPJmL", selectyears = selectyears,
                            lpjml = lpjml, climatetype = climatetype,
                            multicropping = "TRUE:potential:endogenous",
                            aggregate = FALSE)[, iniyear, "irrigated"]
  yldOffSeason <- collapseNames(yldMultiple - yldSingle)

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
  bconsCrop[naCells] <- NA

  # grass blue water consumption in main growing season
  bconsGrass <- etGrassIRgrper - etGrassNOIRgrper
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
      if (grepl("drip", tmp1)) {
        # remove irrigation system dimension (dim 3.1)
        j <- gsub("^[^.]*\\.", "", i)
      } else if (grepl("drip", tmp2)) {
        # remove irrigation system dimension (dim 3.2)
        j <- gsub("\\..*$", "", i)
      } else {
        stop("Wrong dimensionality in object used in regression of calcBlueWaterConsumptionOff.")
      }
      # Linear regression
      fit <- stats::lm(y ~ x,
                data = data.frame(y = as.vector(bconsCrop[, yr, i]),
                                  x = as.vector(bconsGrass[, yr, j])))
      # Extract intercept and slope coefficient
      a[, yr, i] <- stats::coef(fit)[1]
      b[, yr, i] <- stats::coef(fit)[2]
    }
  }

  # grass blue water consumption in the entire year
  bconsGrassYr <- etGrassIRyear - etGrassNOIRyear

  # First season blue water consumption of crop ("main season")
  bwc1st <- bconsCrop
  # Second season blue water consumption of grass ("off season")
  grassBWC2nd <- bconsGrassYr - bconsGrass
  # Second season blue water consumption of crop ("off season")
  bwc2nd <- a + b * grassBWC2nd
  # Exclude cells with low yields in second season
  bwc2nd[naCells] <- 0     #### To Do: double-check whether object dimensions are correct

  #### To Do: check whether perennials all got 0 BWC in second season (should be the case via N/A rules above!)
  #### To Do: check whether betr and begr are included (and also have 0 BWC in 2nd period)

  ##############
  ### Checks ###
  ##############
  if (any(is.na(bwc2nd))) {
    stop("calcBlueWaterConsumptionOff produced NA values")
  }
  if (any(bwc2nd < 0)) {
    warning("calcBlueWaterConsumptionOff produced negative values")
    # ToDo: Check whether this should be stop (discuss with Jens)
  }
  # Correction of negative values
  bwc2nd[bwc2nd < 0] <- 0

  # Check system blue water consumption
  # To Do (double-check with Jens): sprinkler > surface > drip
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
