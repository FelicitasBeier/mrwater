#' @title       calcIrrigAreaCommitted
#' @description calculates area reserved for irrigation: irrigated area of the
#'              respective year until the last year with data (past_til2020),
#'              area of the last year with data until the end of the
#'              harmonization period (2025), afterwards reduced by
#'              depreciation parameter (set to 0.1) every year
#'
#' @param iniyear        initialization year
#' @param selectyears    select years
#' @param aggregateCrops Logical. If FALSE (default), result is returned per crop.
#'                       If TRUE, values are aggregated over the crop dimension.
#'
#' @return magpie object in cellular resolution
#' @author Felicitas Beier
#'
#' @examples
#' \dontrun{
#' calcOutput("IrrigAreaCommitted", aggregate = FALSE)
#' }
#'
#' @importFrom madrat calcOutput
#' @importFrom magclass collapseNames collapseDim new.magpie getCells getNames dimSums setYears
#' @importFrom magpiesets findset
#' @importFrom utils tail

calcIrrigAreaCommitted <- function(selectyears, iniyear, aggregateCrops = FALSE) {

  # Set depreciation parameter
  depreciation <- 0.1
  # End of harmonization period (sm_fix_SSP2 in MAgPIE):
  # committed area is fully reserved until this year and depreciates afterwards
  harmYr <- 2025
  # Last year with irrigated area data
  lastYr <- as.numeric(gsub("y", "", tail(findset("past_til2020"), n = 1)))

  # Read in data: crop- and water supply type specific physical crop area (in Mha)
  croparea <- collapseNames(calcOutput("Croparea",
                                       physical = TRUE, fallow = FALSE,
                                       sectoral = "kcr", cellular = TRUE,
                                       irrigation = TRUE,
                                       aggregate = FALSE)[, , "irrigated"])


  # Prepare year arguments for further usage
  if (is.character(selectyears)) {
    selectyears <- as.numeric(gsub("y", "", selectyears))
  }
  if (is.character(iniyear)) {
    iniyear     <- as.numeric(gsub("y", "", iniyear))
  }
  if (iniyear > selectyears[1]) {
    stop(paste0("Please align the initialization year ('iniyear') ",
                 "with the selected years ('selectyears'). ",
                 "The 'iniyear' has to be the first year of 'selectyears' ",
                 "or lie in the past of it."))
  }
  if (harmYr < lastYr) {
    stop("The end of the harmonization period must not lie before the last year with data.")
  }
  # selectyears with and without irrigated area data
  pstYrs <- selectyears[selectyears <= lastYr]
  ftrYrs <- selectyears[selectyears > lastYr]

  # Empty object to be filled with area reserved for irrigation
  irrigArea <- new.magpie(cells_and_regions = getCells(croparea),
                          years = selectyears,
                          names = getNames(croparea),
                          sets = c("x.y.iso", "year", "crop"))

  # In years with data, the area irrigated in the respective year is reserved
  if (length(pstYrs) > 0) {
    irrigArea[, pstYrs, ] <- croparea[, pstYrs, ]
  }
  # Afterwards, the area of the last year with data is reserved until the end of the harmonization period.
  # Then, each year certain share (parameter: "depreciation") of irrigated cropland is lost.
  for (y in ftrYrs) {
    irrigArea[, y, ] <- setYears(croparea[, lastYr, ], NULL) * (1 - depreciation)^max(0, y - harmYr)
  }

  # optionally, aggregate crop dimension for total committed irrigated area
  if (aggregateCrops) {
    irrigArea <- dimSums(irrigArea, dim = "crop")
    description <- "Total cropland area reserved for irrigation"
  }
  else {
    description <- "Cropland area reserved for irrigation per crop"
  }

  # check for NAs and negative values
  if (any(is.na(irrigArea))) {
    stop("produced NA irrigation water requirements")
  }
  if (any(irrigArea < 0)) {
    stop("produced negative irrigation water requirements")
  }

  return(list(x            = irrigArea,
              weight       = NULL,
              unit         = "Mha",
              description  = description,
              isocountries = FALSE))
}
