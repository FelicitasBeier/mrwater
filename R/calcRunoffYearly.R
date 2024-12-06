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

  # Yearly runoff (m^3/ha) [smoothed & harmonized]
  x <- calcOutput("LPJmLharmonize", subtype = "pnv:runoff",
                  lpjmlversion = lpjml, climatetype = climatetype,
                  years = selectyears,
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
                            years = selectyears,
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
