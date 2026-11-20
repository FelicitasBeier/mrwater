#' @title       calcIrrigationDataMAgPIE
#' @description returns irrigation and water data required for MAgPIE
#'              for different scenarios in MAgPIE (scen25)
#'              given available water and land.
#'              All outputs are derived from the same scenario definitions.
#'
#' @param output        "PotIrrigAreas": potentially irrigated areas (PIA)
#'                                       including areas irrigated with non-renewable groundwater
#'                      "WaterAvlMAgPIE": potential irrigation water withdrawals and consumption
#'                                        from renewable sources (ren_ag)
#'                      "NonAgWater": non-agricultural water demand, demand served from renewable
#'                                    sources and unmet demand (after non-renewable groundwater)
#'                      "NonrenGW": non-renewable groundwater pool (withdrawal) for
#'                                  agriculture (nonren_ag) and non-agricultural uses (nonren_nonAg)
#'                                  (scenario-independent)
#'                      "EFR": environmental flow requirements (VMF:fair) per cell
#'                             (discharge-based, not additive across cells)
#'                      "HarmonizationYear": end of the harmonization period of all outputs
#'                                           (toolHarmonizationYear), for a consistency check in MAgPIE
#' @param lpjml         LPJmL version used
#' @param climatetype   Switch between different climate scenarios or
#'                      historical baseline "GSWP3-W5E5:historical"
#' @param selectyears   Years for which irrigatable area is calculated
#' @param iniyear       Initialization year for initial croparea
#' @param countryAggregation TRUE: cell values are summed to country level
#'                      (only for additive outputs; not for "EFR" and "HarmonizationYear"),
#'                      FALSE: cellular resolution
#'
#' @return magpie object in cellular resolution
#' @author Felicitas Beier
#'
#' @examples
#' \dontrun{
#' calcOutput("IrrigationDataMAgPIE", aggregate = FALSE)
#' }
#'
#' @importFrom stringr str_split
#' @importFrom madrat calcOutput
#' @importFrom magclass collapseNames add_dimension add_columns mbind collapseDim getYears getSets new.magpie dimSums
#'
#' @export

calcIrrigationDataMAgPIE <- function(output,
                                     lpjml, climatetype,
                                     selectyears, iniyear,
                                     countryAggregation = FALSE) {

  # Country aggregation is only meaningful for additive outputs
  # (EFR is discharge-based and not additive across cells)
  if (countryAggregation && output %in% c("EFR", "HarmonizationYear")) {
    stop("calcIrrigationDataMAgPIE: countryAggregation is not possible for output ", output)
  }

  # Helper function: keep only those arguments that belong to respective calcFunction
  .filterArgs <- function(args, fun) {
    args[names(args) %in% names(formals(fun))]
  }

  ##############################
  ### Scenario construction: ###
  ##############################
  # Note: fossilGW = TRUE for all scenarios: PIA includes areas irrigated with non-renewable
  #       groundwater, consistent with the non-renewable groundwater pool (NonrenGW) that is
  #       available to agriculture in MAgPIE in all scenarios.
  #       Renewable water (ren_ag) does not depend on fossilGW.
  # To Do: scenario-specific fossilGW (after the harmonization period), see backlog 24
  commonArgs <- list(cropAggregation = TRUE,
                     usagetype = "withdrawal",
                     countryAggregation = FALSE,
                     lpjml = lpjml,
                     climatetype = climatetype,
                     selectyears = selectyears,
                     iniyear = iniyear,
                     comAg = TRUE,
                     fossilGW = TRUE,
                     cropmix = "hist_total", # c("maize", "rapeseed", "puls_pro"), (switch to proxy crops when ready)
                     yieldcalib = "TRUE:TRUE:actual:irrig_crop",
                     multicropping = "TRUE:actual:irrig_crop") # To Do: decide on default argument (or make scenario-dependent)

  ### Question (Jan, Benni): Should the scenarios fade in / start in 2020 or
  ###                        in initialization year. How to do the fade-over?
  ###                        In this function? Fading from ssp2off to the respective scenarios?
  ###                        How to ensure consistency with PIWW?
  ### What about "cropmix"? proxycrops or historical cropmix?
  ### Groundwater fade-out? Allow fossil-GW for 1995-2025 and then fade out after?
  # Idea for calcPotIrrWatMAgPIE: report fossil GW separately and feed to GAMS, so that it can be deactivated / faded out there
  # Which ranking to apply:   USD_ha (USD per hectare) for relative area return, or
  #                           USD_m3 (USD per cubic meter) for relative volumetric return;

  scenarioArgs <- list(
    # Taking the Green Road: sustainability
    ssp1 = list(efrMethod = "VMF:fair", # environment: environmental flow protection following VMF
                landScen = "potCropland:30by30", # environment: no irrigation in certain areas
                accessibilityrule = "CV:2", # technology
                irrigationsystem = "drip", # technology
                transDist = 100, # technology
                allocationrule = "optimization", # economics
                rankmethod = "USD_m3:GLO:TRUE", # economics: global ranking by relative volumetric return and full potential
                gainthreshold = 50 # economics (above 50 USD/ha: irrigation takes place)
    ),
    # Sustainable development: economy-driven innovation
    sdpEI = list(efrMethod = "Smakhtin:natural", # environment: very high EFP
                 landScen = "potCropland:HalfEarth", # environment: no irrigation in certain areas
                 accessibilityrule = "CV:2", # technology
                 irrigationsystem = "drip", # technology: high efficiency
                 transDist = 100, # technology
                 allocationrule = "optimization", # economics
                 rankmethod = "USD_m3:GLO:TRUE", # economics: global ranking by relative volumetric return and full potential
                 gainthreshold = 50 # economics (above 50 USD/ha: irrigation takes place)
    ),
    # Sustainable development: resilient communities
    sdpRC = list(efrMethod = "VMF:fair", # environment: environmental flow protection following VMF
                 landScen = "potCropland:30by30", # environment: no irrigation in certain areas
                 accessibilityrule = "CV:2", # technology
                 irrigationsystem = "sprinkler", # technology: medium efficiency
                 transDist = 100, # technology
                 allocationrule = "optimization", # economics
                 rankmethod = "USD_m3:GLO:TRUE", # economics: global ranking by relative volumetric return and full potential
                 gainthreshold = 50 # economics (above 50 USD/ha: irrigation takes place)
    ),
    # Sustainable development: managing the global commons
    sdpMC = list(efrMethod = "Smakhtin:natural", # environment: very high EFP
                 landScen = "potCropland:HalfEarth", # environment: no irrigation in certain areas
                 accessibilityrule = "CV:2", # technology
                 irrigationsystem = "drip", # technology: high efficiency
                 transDist = 100, # technology
                 allocationrule = "optimization", # economics
                 rankmethod = "USD_m3:GLO:TRUE", # economics: global ranking by relative volumetric return and full potential
                 gainthreshold = 50 # economics (above 50 USD/ha: irrigation takes place)
    ),
    # Middle of the Road
    ssp2 = list(efrMethod = "VMF:fair", # environment: environmental flow protection following VMF
                landScen = "potCropland:WDPA", # environment: no irrigation in certain areas
                accessibilityrule = "CV:2", # technology
                irrigationsystem = "initialization", # technology
                transDist = 100, # technology
                allocationrule = "optimization", # economics
                rankmethod = "USD_m3:GLO:FALSE", # economics: global ranking by relative volumetric return, but with reduced potential (slightly less optimal)
                gainthreshold = 50 # economics (above 50 USD/ha: irrigation takes place)
    ),
    # Rocky Road: Regional rivalry
    ssp3 = list(efrMethod = "VMF:fair", # environment: environmental flow protection following VMF
                landScen = "potCropland:30by30", # environment: no irrigation in certain areas
                accessibilityrule = "CV:2", # technology
                irrigationsystem = "initialization", # technology
                transDist = 100, # technology
                allocationrule = "upstreamfirst", # economics
                rankmethod = "USD_m3:GLO:TRUE", # economics: global ranking by relative volumetric return and full potential
                gainthreshold = 50 # economics (above 50 USD/ha: irrigation takes place)
    ),
    # A Road Divided: Inequality
    ssp4 = list(efrMethod = "VMF:fair", # environment: environmental flow protection following VMF
                landScen = "potCropland:30by30", # environment: no irrigation in certain areas
                accessibilityrule = "CV:2", # technology
                irrigationsystem = "initialization", # technology
                transDist = 100, # technology
                allocationrule = "optimization", # economics
                rankmethod = "USD_m3:ISO:TRUE", # economics: ranking by relative volumetric return and full potential based on country-level prices
                gainthreshold = 50 # economics (above 50 USD/ha: irrigation takes place)
    ),
    # Taking the Highway: Fossil-fuel-driven development
    ssp5 = list(efrMethod = "VMF:fair", # environment: environmental flow protection following VMF
                landScen = "potCropland:30by30", # environment: no irrigation in certain areas
                accessibilityrule = "CV:2", # technology
                irrigationsystem = "drip", # technology
                transDist = 100, # technology
                allocationrule = "optimization", # economics
                rankmethod = "USD_m3:GLO:FALSE", # economics: global ranking by relative volumetric return and reduced potential
                gainthreshold = 50 # economics (above 50 USD/ha: irrigation takes place)
    )
  )

  # scen25 scenarios: scenario arguments, EFP setting and non-agricultural water use scenario
  # To Do: call calc functions only once for scenarios with identical arguments (ssp2, ssp2efp)
  scen25 <- list(ssp1    = c(args = "ssp1", efp = "on",  nonAg = "ssp1"),
                 ssp2    = c(args = "ssp2", efp = "off", nonAg = "ssp2"),
                 ssp2efp = c(args = "ssp2", efp = "on",  nonAg = "ssp2"),
                 ssp3    = c(args = "ssp3", efp = "off", nonAg = "ssp3"))
  # To Do: implement full set of SSP/SDP scenarios
  #        (add entries to scen25; scenario names must also be added to scen25 in MAgPIE)
  # ssp4  = c(args = "ssp4",  efp = "off", nonAg = "ssp3"),  # nolint: commented_code_linter
  #         To do (SSP4): implement EFP protection based on development status
  # ssp5  = c(args = "ssp5",  efp = "on",  nonAg = "ssp5"),  # nolint: commented_code_linter
  # sdpEI = c(args = "sdpEI", efp = "on",  nonAg = "sdpEI"), # nolint: commented_code_linter
  # sdpRC = c(args = "sdpRC", efp = "on",  nonAg = "sdpRC"), # nolint: commented_code_linter
  # sdpMC = c(args = "sdpMC", efp = "on",  nonAg = "sdpMC")  # nolint: commented_code_linter

  # Helper function: select the scen25 scenario from an object with EFP and
  # non-agricultural scenario dimension
  .selectScen <- function(x, nm) {
    s <- scen25[[nm]]
    return(add_dimension(collapseNames(x[, , s[["efp"]]][, , s[["nonAg"]]]),
                         dim = 3.1, add = "scen25", nm = nm))
  }

  # Helper function: name single-item object
  .setItem <- function(x, add, nm) {
    return(add_dimension(collapseNames(x), dim = 3.1, add = add, nm = nm))
  }

  # Helper function: run calc function of a scen25 scenario
  # Note: calcOutput calls with explicit type are required for the
  #       madrat dependency detection (cache fingerprint)
  .runCalc <- function(type, nm, ...) {
    calcCall <- switch(type,
                       PotIrrigAreas  = function(...) calcOutput("PotIrrigAreas", aggregate = FALSE, ...),
                       WaterAvlMAgPIE = function(...) calcOutput("WaterAvlMAgPIE", aggregate = FALSE, ...))
    args <- utils::modifyList(c(commonArgs, scenarioArgs[[scen25[[nm]][["args"]]]]), list(...))
    args <- .filterArgs(args, get(paste0("calc", type)))
    return(do.call(calcCall, args))
  }

  # Helper function: aggregation function for outputs that must not be aggregated
  .noAggregation <- function(...) {
    stop("calcIrrigationDataMAgPIE: output ", output, " cannot be aggregated. Use aggregate = FALSE.")
  }
  # minimal environment: the function is part of the cached result (otherwise all local objects are cached, too)
  environment(.noAggregation) <- list2env(list(output = output), parent = baseenv())
  # default aggregation (madrat: toolAggregate); outputs that must not be aggregated overwrite it
  aggregationFunction <- NULL

  if (output == "PotIrrigAreas") {

    out <- mbind(lapply(names(scen25), function(nm) .selectScen(.runCalc("PotIrrigAreas", nm), nm)))

    description <- paste0("Potentially irrigated area (PIA) including non-renewable groundwater ",
                          "for different scenarios given available water and land")
    unit <- "Mha"
    # absolute values, additive across cells: summed in aggregation (no weight)
    weight <- NULL

  } else if (output == "WaterAvlMAgPIE") {

    .renAg <- function(nm, usagetype) {
      x <- .selectScen(.runCalc("WaterAvlMAgPIE", nm, usagetype = usagetype)[, , "ren_ag"], nm)
      return(add_dimension(x, dim = 3.2, add = "wtype", nm = usagetype))
    }
    out <- mbind(lapply(names(scen25), function(nm) {
      mbind(.renAg(nm, "withdrawal"), .renAg(nm, "consumption"))
    }))

    description <- paste0("Potential irrigation water withdrawals (PIWW) and consumption (PIWC) ",
                          "from renewable sources for different scenarios given available water and land")
    unit <- "mio. m^3"
    # absolute values, additive across cells: summed in aggregation (no weight)
    weight <- NULL

  } else if (output == "NonAgWater") {

    # Non-agricultural water demand as used in the river routing (calcRiverRoutingInputs)
    demand <- calcOutput("WaterUseNonAg",
                         selectyears = selectyears, cells = "lpjcell",
                         datasource = "WATERGAP_ISIMIP", usetype = "total",
                         seasonality = "total", harmonType = "average",
                         lpjml = NULL, climatetype = NULL, aggregate = FALSE)
    # Non-renewable groundwater pool for non-agricultural uses (as in calcPotWater)
    gw <- calcOutput("NonrenGroundwatUse", output = "nonAg",
                     multicropping = commonArgs$multicropping, transDistGW = 100,
                     lpjml = lpjml, climatetype = climatetype,
                     selectyears = selectyears, iniyear = iniyear,
                     aggregate = FALSE)

    .nonAg <- function(nm) {
      s        <- scen25[[nm]]
      scenArgs <- scenarioArgs[[s[["args"]]]]
      # Non-agricultural water use served from renewable sources (call as in calcPotWater)
      served <- calcOutput("RiverHumanUseAccounting",
                           iteration = "non_agriculture",
                           lpjml = lpjml, climatetype = climatetype,
                           transDist = scenArgs$transDist, comAg = NULL,
                           efrMethod = scenArgs$efrMethod, multicropping = commonArgs$multicropping,
                           selectyears = selectyears, iniyear = iniyear,
                           accessibilityrule = NULL,
                           rankmethod = NULL, gainthreshold = NULL,
                           cropmix = NULL, yieldcalib = NULL,
                           irrigationsystem = NULL, landScen = NULL,
                           aggregate = FALSE)
      served <- served[, , s[["efp"]]][, , s[["nonAg"]]]
      served <- mbind(.setItem(served[, , "currHumanWWtotal"], add = "wtype", nm = "withdrawal"),
                      .setItem(served[, , "currHumanWCtotal"], add = "wtype", nm = "consumption"))
      dem    <- mbind(.setItem(demand[, , s[["nonAg"]]][, , "withdrawal"], add = "wtype", nm = "withdrawal"),
                      .setItem(demand[, , s[["nonAg"]]][, , "consumption"], add = "wtype", nm = "consumption"))
      served <- add_dimension(served, dim = 3.1, add = "scen25", nm = nm)
      dem    <- add_dimension(dem, dim = 3.1, add = "scen25", nm = nm)

      if (any(round(served - dem, digits = 6) > 0)) {
        stop("mrwater::calcIrrigationDataMAgPIE: served non-agricultural water use exceeds demand")
      }
      # Unmet demand: neither served from renewable sources nor from non-renewable groundwater
      unmet <- dem - served - gw
      unmet[unmet < 0] <- 0

      return(mbind(add_dimension(dem, dim = 3.2, add = "type", nm = "demand"),
                   add_dimension(served, dim = 3.2, add = "type", nm = "served_ren"),
                   add_dimension(unmet, dim = 3.2, add = "type", nm = "unmet")))
    }
    out <- mbind(lapply(names(scen25), .nonAg))

    description <- paste0("Non-agricultural water demand, demand served from renewable sources ",
                          "and unmet demand (after non-renewable groundwater) for different scenarios")
    unit <- "mio. m^3"
    # absolute values, additive across cells: summed in aggregation (no weight)
    weight <- NULL

  } else if (output == "NonrenGW") {

    # Non-renewable groundwater pool (as in calcPotWater; scenario-independent)
    .gw <- function(gwOutput, nm) {
      gw <- calcOutput("NonrenGroundwatUse", output = gwOutput,
                       multicropping = commonArgs$multicropping, transDistGW = 100,
                       lpjml = lpjml, climatetype = climatetype,
                       selectyears = selectyears, iniyear = iniyear,
                       aggregate = FALSE)
      return(.setItem(gw[, , "withdrawal"], add = "source", nm = nm))
    }
    out <- mbind(.gw("comAg", "nonren_ag"), .gw("nonAg", "nonren_nonAg"))

    description <- paste0("Non-renewable groundwater withdrawal pool for agriculture (nonren_ag) ",
                          "and non-agricultural uses (nonren_nonAg)")
    unit <- "mio. m^3"
    # absolute values, additive across cells: summed in aggregation (no weight)
    weight <- NULL

  } else if (output == "EFR") {

    # Environmental flow requirements (as in calcRiverRoutingInputs; MVP: VMF:fair only)
    # Note: EFR = EFR share * natural discharge, i.e. not additive across cells
    out <- calcOutput("EnvmtlFlowRequirements",
                      lpjml = lpjml, climatetype = climatetype,
                      selectyears = selectyears, efrMethod = "VMF:fair",
                      aggregate = FALSE)[, , "EFR"]

    description <- "Environmental flow requirements (VMF:fair) per cell"
    unit <- "mio. m^3"
    # not aggregatable: discharge-based, i.e. upstream flow is counted again in every downstream cell
    # (neither a sum nor a weighted mean is meaningful); cellular output only
    weight <- NULL
    aggregationFunction <- .noAggregation

  } else if (output == "HarmonizationYear") {

    # Until this year, all scen25 scenarios follow ssp2 (must agree with sm_fix_SSP2 in MAgPIE)
    out <- new.magpie(cells_and_regions = "GLO", years = NULL,
                      names = "harmonization_year", fill = toolHarmonizationYear())

    description <- "End of the harmonization period of the mrwater inputs (all scenarios follow ssp2 until then)"
    unit <- "year"
    # not aggregatable: one global value
    weight <- NULL
    aggregationFunction <- .noAggregation

  } else {
    stop("calcIrrigationDataMAgPIE: unknown output ", output)
  }

  if ("scen25" %in% getSets(out)) {
    # Harmonization: all scenarios follow the common ssp2 path (EFP off) until the end of the
    # harmonization period (sm_fix_SSP2 in MAgPIE); scenarios diverge afterwards
    harmYrs <- getYears(out)[getYears(out, as.integer = TRUE) <= toolHarmonizationYear()]
    if (length(harmYrs) > 0) {
      out[, harmYrs, ] <- collapseDim(out[, harmYrs, list(scen25 = "ssp2")], dim = "scen25")
    }
    # Climate scenario (MVP: climate change only)
    out <- add_dimension(out, dim = 3.2, add = "clim25", nm = "cc")
  } else if (output == "EFR") {
    out <- add_dimension(out, dim = 3.1, add = "clim25", nm = "cc")
  }

  # Aggregation to country level
  if (countryAggregation) {
    out <- dimSums(out, dim = c("x", "y"))
    description <- paste0(description, " at country level")
  }

  # Checks
  if (any(is.na(out))) {
    stop("mrwater::calcIrrigationDataMAgPIE produced NA values")
  }
  if (any(round(out, digits = 6) < 0)) {
    stop("mrwater::calcIrrigationDataMAgPIE produced negative values")
  }

  # Note: weight and aggregation are set per output (weight is only used in aggregation:
  #       NULL = sum for absolute values, otherwise weighted mean, e.g. for shares)
  return(list(x                   = out,
              weight              = weight,
              unit                = unit,
              description         = description,
              aggregationFunction = aggregationFunction,
              isocountries        = FALSE))
}
