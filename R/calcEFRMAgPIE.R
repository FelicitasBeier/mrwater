#' @title       calcEFRMAgPIE
#' @description returns environmental flow requirements (VMF:fair) per cell for MAgPIE
#'              (as in calcRiverRoutingInputs) for the climate scenarios (clim25):
#'              cc (climate change) and nocc_hist (no climate change after the
#'              harmonization year; LPJmL add-on +nocchist).
#'              EFR = EFR share * natural discharge, i.e. not additive across cells:
#'              cellular output only (aggregate = FALSE).
#'
#' @param lpjml         LPJmL version used
#' @param climatetype   Switch between different climate scenarios or
#'                      historical baseline "GSWP3-W5E5:historical"
#' @param selectyears   Years to be returned
#'
#' @return magpie object in cellular resolution
#' @author Felicitas Beier
#'
#' @examples
#' \dontrun{
#' calcOutput("EFRMAgPIE", aggregate = FALSE)
#' }
#'
#' @importFrom madrat calcOutput
#' @importFrom magclass add_dimension mbind collapseDim getYears
#'
#' @export

calcEFRMAgPIE <- function(lpjml, climatetype, selectyears) {

  # Climate scenarios (clim25): LPJmL version per climate scenario
  lpjmlClim <- c(cc = lpjml, nocc_hist = paste0(lpjml, "+nocchist"))

  # MVP: VMF:fair only
  out <- NULL
  for (clim in names(lpjmlClim)) {
    x   <- calcOutput("EnvmtlFlowRequirements",
                      lpjml = lpjmlClim[[clim]], climatetype = climatetype,
                      selectyears = selectyears, efrMethod = "VMF:fair",
                      aggregate = FALSE)[, , "EFR"]
    out <- mbind(out, add_dimension(x, dim = 3.1, add = "clim25", nm = clim))
  }

  # Check: climate scenarios differ only after the harmonization year
  harmYrs <- getYears(out)[getYears(out, as.integer = TRUE) <= toolHarmonizationYear()]
  if (length(harmYrs) > 0) {
    ccDiff <- collapseDim(out[, harmYrs, list(clim25 = "cc")], dim = "clim25") -
      collapseDim(out[, harmYrs, list(clim25 = "nocc_hist")], dim = "clim25")
    if (max(abs(ccDiff)) > 1e-6) {
      stop("mrwater::calcEFRMAgPIE: cc and nocc_hist differ until the harmonization year ",
           "(max. abs. difference: ", max(abs(ccDiff)), ")")
    }
  }

  # Checks
  if (any(is.na(out))) {
    stop("mrwater::calcEFRMAgPIE produced NA values")
  }
  if (any(round(out, digits = 6) < 0)) {
    stop("mrwater::calcEFRMAgPIE produced negative values")
  }

  # Not aggregatable: discharge-based, i.e. upstream flow is counted again in every downstream cell
  # (neither a sum nor a weighted mean is meaningful)
  return(list(x                   = out,
              weight              = NULL,
              unit                = "mio. m^3",
              description         = "Environmental flow requirements (VMF:fair) per cell",
              aggregationFunction = local(function(...) stop("calcEFRMAgPIE: use aggregate = FALSE"), baseenv()),
              isocountries        = FALSE))
}
