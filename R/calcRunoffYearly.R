#' @title       calcRunoffYearly
#' @description This function calculates yearly runoff from runoff
#'              on land and water provided by LPJmL
#'
#' @param selectyears Years to be returned
#'                    (Note: does not affect years of harmonization or smoothing)
#' @param lpjml       LPJmL version required for respective inputs: natveg or crop
#' @param climatetype Switch between different climate scenarios or
#'                    historical baseline "GSWP3-W5E5:historical"
#'
#' @importFrom madrat calcOutput
#' @importFrom magclass collapseNames
#'
#' @return magpie object in cellular resolution
#' @author Felicitas Beier, Jens Heinke
#'
#' @examples
#' \dontrun{
#' calcOutput("YearlyRunoff", aggregate = FALSE)
#' }
#'
calcRunoffYearly <- function(selectyears, lpjml, climatetype) {

  ### To Do: Confirm with Kristine
  ### How would this be handled with the new structure (calcLPJmLtransform and calcLPJmLharmonize)?
  ### Ideally I would like to be able to avoid to have to make this distinction at all.
  ### Can I just call calcLPJmLharmonize and it handles the distinction? I basically always want
  ### as harmonized as possible and if it's not available (e.g., because it's only historical data)
  ### than I want it smoothed.
  # Read in input data already time-smoothed and for climate scenarios harmonized to the baseline
  if (grepl("historical", climatetype)) {
    # Baseline is only smoothed (not harmonized)
    stage <- "smoothed"
  } else {
    # Climate scenarios are harmonized to baseline
    stage <- "harmonizedHistorical"  #### To Do (Feli): change once calcLPJmLharmonize is ready
  }

  # Yearly runoff (m^3/ha) [smoothed & harmonized]
  x <- calcOutput("LPJmLharmonize", subtype = "runoff", stage = stage,
                  version = lpjml, climatetype = climatetype,
                  aggregate = FALSE)
  #### To Do (Feli, Kristine): handle aggregation to yearly before harmonization (in calcLPJmLtransform)
  x <- dimSums(x, dim = "month")
  # LUH landarea (in Mha)
  landArea <- setYears(collapseNames(dimSums(readSource("LUH2v2", subtype = "states",
                                                        convert = "onlycorrect")[, "y1995", ],
                                             dim = 3)),
                       NULL)
  ### Transformation to flow on land ###
  # Transformation factor: 1 m^3/ha = 1e-6 mio. m^3/ha
  # Transformation factor: 1 Mha    = 1e+6 ha
  runoffLand <- x * landArea

  # Precipitation/runoff on water bodies (in mio. m^3)
  runoffWater <- calcOutput("LakeFlows", subtype = "input_lake",
                            lpjml = lpjml, climatetype = climatetype,
                            aggregate = FALSE)

  ## Calculate Runoff (on land and water)
  out <- runoffLand + runoffWater

  return(list(x            = out,
              weight       = NULL,
              unit         = "mio. m^3",
              description  = "yearly runoff",
              isocountries = FALSE))
}
