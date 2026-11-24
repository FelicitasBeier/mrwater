#' @title       calcHarmonizationYearMAgPIE
#' @description returns the end of the harmonization period of the mrwater inputs for MAgPIE
#'              (toolHarmonizationYear): until this year, all water scenarios follow ssp2.
#'              Used for a consistency check in MAgPIE (must agree with sm_fix_SSP2).
#'
#' @return magpie object with one global value
#' @author Felicitas Beier
#'
#' @examples
#' \dontrun{
#' calcOutput("HarmonizationYearMAgPIE", aggregate = FALSE)
#' }
#'
#' @importFrom magclass new.magpie
#'
#' @export

calcHarmonizationYearMAgPIE <- function() {

  out <- new.magpie(cells_and_regions = "GLO", years = NULL,
                    names = "harmonization_year", fill = toolHarmonizationYear())

  # Not aggregatable: one global value
  return(list(x                   = out,
              weight              = NULL,
              unit                = "year",
              description         = paste0("End of the harmonization period of the mrwater inputs ",
                                           "(all scenarios follow ssp2 until then)"),
              aggregationFunction = local(function(...) stop("calcHarmonizationYearMAgPIE: use aggregate = FALSE"),
                                          baseenv()),
              isocountries        = FALSE))
}
