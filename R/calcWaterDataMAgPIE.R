#' @title       calcWaterDataMAgPIE
#' @description returns irrigation and water data required for MAgPIE (module 25)
#'              for different water scenarios (scen25, see toolWaterScenarios25)
#'              and climate scenarios (clim25) given available water and land.
#'              clim25: cc (climate change) and nocc_hist (no climate change after the
#'              harmonization year; LPJmL add-on +nocchist).
#'              Harmonized scenarios follow the ssp2 scenario until the harmonization year
#'              (toolHarmonizationYear); sensitivity scenarios (harmonized = FALSE) do not.
#'
#' @param output        "PotIrrigAreas": potentially irrigated areas (PIA)
#'                                       including areas irrigated with non-renewable groundwater
#'                      "WaterAvlMAgPIE": potential irrigation water withdrawals and consumption
#'                                        from renewable sources (ren_ag)
#'                      "NonAgWater": non-agricultural water demand, demand served from renewable
#'                                    sources and unmet demand (after non-renewable groundwater)
#'                      "NonrenGW": non-renewable groundwater pool (withdrawal) for
#'                                  agriculture (nonren_ag) and non-agricultural uses (nonren_nonAg);
#'                                  scenario-independent (ssp2) and climate-independent
#'                                  (missing water until the harmonization year, constant afterwards)
#' @param lpjml         LPJmL version used
#' @param climatetype   Switch between different climate scenarios or
#'                      historical baseline "GSWP3-W5E5:historical"
#' @param selectyears   Years for which irrigatable area is calculated
#' @param iniyear       Initialization year for initial croparea
#' @param countryAggregation TRUE: cell values are summed to country level,
#'                      FALSE: cellular resolution
#'
#' @return magpie object in cellular resolution
#' @author Felicitas Beier
#'
#' @examples
#' \dontrun{
#' calcOutput("WaterDataMAgPIE", output = "PotIrrigAreas", aggregate = FALSE)
#' }
#'
#' @importFrom madrat calcOutput
#' @importFrom magclass collapseNames collapseDim add_dimension mbind getYears getSets getItems dimSums
#'
#' @export

calcWaterDataMAgPIE <- function(output,
                                lpjml, climatetype,
                                selectyears, iniyear,
                                countryAggregation = FALSE) {

  # Water scenarios (scen25)
  # To Do: implement full set of SSP/SDP scenarios
  #        (scenario names must also be added to scen25 in MAgPIE)
  scenarios <- toolWaterScenarios25()[c("ssp1", "ssp2", "ssp2efp", "ssp2strict", "ssp3")]

  # Climate scenarios (clim25): LPJmL version per climate scenario
  lpjmlClim <- c(cc = lpjml, nocc_hist = paste0(lpjml, "+nocchist"))

  # Arguments common to all calls (in addition to the scenario settings and the LPJmL version)
  commonArgs <- list(climatetype = climatetype, selectyears = selectyears, iniyear = iniyear,
                     cropAggregation = TRUE, countryAggregation = FALSE, usagetype = "withdrawal")

  # Helper function: calcOutput of the selected function with the arguments it accepts
  # Note: calcOutput calls with explicit type are required for the
  #       madrat dependency detection (cache fingerprint)
  .calc <- function(type, args) {
    calcCall <- switch(type,
                       PotIrrigAreas  = function(...) calcOutput("PotIrrigAreas", aggregate = FALSE, ...),
                       WaterAvlMAgPIE = function(...) calcOutput("WaterAvlMAgPIE", aggregate = FALSE, ...),
                       PotWater       = function(...) calcOutput("PotWater", aggregate = FALSE, ...))
    args <- args[names(args) %in% names(formals(get(paste0("calc", type))))]
    return(do.call(calcCall, args))
  }

  if (output == "NonrenGW") {

    # Non-renewable groundwater pool, as used in calcPotWater (taken from the ssp2 scenario)
    s   <- scenarios[["ssp2"]]
    x   <- .calc("PotWater", c(s, commonArgs, lpjml = lpjml))[, , s$efp][, , s$nonAg]
    out <- mbind(add_dimension(collapseNames(x[, , "wat_gw_ww"]), dim = 3.1, add = "source", nm = "nonren_ag"),
                 add_dimension(collapseNames(x[, , "wat_nonag_gw_ww"]), dim = 3.1, add = "source",
                               nm = "nonren_nonAg"))

  } else {

    out <- NULL
    for (clim in names(lpjmlClim)) {
      for (scen in names(scenarios)) {

        s    <- scenarios[[scen]]
        args <- c(s, commonArgs, lpjml = lpjmlClim[[clim]])

        if (output == "PotIrrigAreas") {

          x <- collapseNames(.calc("PotIrrigAreas", args)[, , s$efp][, , s$nonAg])

        } else if (output == "WaterAvlMAgPIE") {

          ww <- .calc("WaterAvlMAgPIE", args)[, , s$efp][, , s$nonAg][, , "ren_ag"]
          wc <- .calc("WaterAvlMAgPIE", utils::modifyList(args, list(usagetype = "consumption")))
          wc <- wc[, , s$efp][, , s$nonAg][, , "ren_ag"]
          x  <- mbind(add_dimension(collapseNames(ww), dim = 3.1, add = "wtype", nm = "withdrawal"),
                      add_dimension(collapseNames(wc), dim = 3.1, add = "wtype", nm = "consumption"))

        } else if (output == "NonAgWater") {

          # Non-agricultural water demand and the parts served from renewable sources and
          # from non-renewable groundwater, as accounted for in calcPotWater (river routing)
          x     <- collapseNames(.calc("PotWater", args)[, , s$efp][, , s$nonAg])
          items <- c(demand = "wat_nonag_dem", served_ren = "wat_nonag_ren", gw = "wat_nonag_gw")
          x     <- x[, , paste0(rep(items, each = 2), c("_ww", "_wc"))]
          getItems(x, dim = 3, raw = TRUE) <- paste(rep(names(items), each = 2),
                                                    c("withdrawal", "consumption"), sep = ".")
          getSets(x, fulldim = FALSE)[3] <- "type.wtype"

          # Unmet demand: neither served from renewable sources nor from non-renewable groundwater
          unmet <- collapseDim(x[, , "demand"], dim = "type") -
            collapseDim(x[, , "served_ren"], dim = "type") - collapseDim(x[, , "gw"], dim = "type")
          unmet[unmet < 0] <- 0
          x <- mbind(x[, , c("demand", "served_ren")],
                     add_dimension(unmet, dim = 3.1, add = "type", nm = "unmet"))

        } else {
          stop("calcWaterDataMAgPIE: unknown output ", output)
        }

        x   <- add_dimension(x, dim = 3.1, add = "clim25", nm = clim)
        out <- mbind(out, add_dimension(x, dim = 3.1, add = "scen25", nm = scen))
      }
    }

    harmYrs <- getYears(out)[getYears(out, as.integer = TRUE) <= toolHarmonizationYear()]
    if (length(harmYrs) > 0) {
      # Check: climate scenarios differ only after the harmonization year
      ccDiff <- collapseDim(out[, harmYrs, list(clim25 = "cc")], dim = "clim25") -
        collapseDim(out[, harmYrs, list(clim25 = "nocc_hist")], dim = "clim25")
      if (max(abs(ccDiff)) > 1e-6) {
        stop("mrwater::calcWaterDataMAgPIE: cc and nocc_hist differ until the harmonization year ",
             "(max. abs. difference: ", max(abs(ccDiff)), ")")
      }
      # Harmonization: harmonized scenarios follow the common ssp2 path (EFP off) until the end of the
      # harmonization period (sm_fix_SSP2 in MAgPIE); scenarios diverge afterwards
      harmScen <- names(scenarios)[vapply(scenarios, function(s) s$harmonized, logical(1))]
      out[, harmYrs, list(scen25 = harmScen)] <- collapseDim(out[, harmYrs, list(scen25 = "ssp2")], dim = "scen25")
    }
  }

  description <- switch(output,
                        PotIrrigAreas  = paste0("Potentially irrigated area (PIA) including non-renewable ",
                                                "groundwater for different scenarios given available water and land"),
                        WaterAvlMAgPIE = paste0("Potential irrigation water withdrawals (PIWW) and consumption ",
                                                "(PIWC) from renewable sources for different scenarios given ",
                                                "available water and land"),
                        NonAgWater     = paste0("Non-agricultural water demand, demand served from renewable ",
                                                "sources and unmet demand (after non-renewable groundwater) ",
                                                "for different scenarios"),
                        NonrenGW       = paste0("Non-renewable groundwater withdrawal pool for agriculture ",
                                                "(nonren_ag) and non-agricultural uses (nonren_nonAg)"))
  unit <- if (output == "PotIrrigAreas") "Mha" else "mio. m^3"

  # Aggregation to country level
  if (countryAggregation) {
    out <- dimSums(out, dim = c("x", "y"))
    description <- paste0(description, " at country level")
  }

  # Checks
  if (any(is.na(out))) {
    stop("mrwater::calcWaterDataMAgPIE produced NA values")
  }
  if (any(round(out, digits = 6) < 0)) {
    stop("mrwater::calcWaterDataMAgPIE produced negative values")
  }

  # absolute values, additive across cells: summed in aggregation (no weight)
  return(list(x            = out,
              weight       = NULL,
              unit         = unit,
              description  = description,
              isocountries = FALSE))
}
