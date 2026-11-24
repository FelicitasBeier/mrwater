#' @title       toolWaterScenarios25
#' @description Settings of the water scenarios for MAgPIE (scen25 in module 25):
#'              arguments of the river routing and irrigation potential functions,
#'              environmental flow protection (efp: "on" or "off"),
#'              scenario of non-agricultural water use (nonAg) and
#'              harmonization with ssp2 until the harmonization year (harmonized: TRUE or FALSE).
#'
#' @return named list: one list of settings per scenario
#' @author Felicitas Beier
#'
#' @examples
#' toolWaterScenarios25()[["ssp2"]]
#'
#' @export

toolWaterScenarios25 <- function() {

  # Settings common to all scenarios
  # Note: fossilGW = TRUE for all scenarios: PIA includes areas irrigated with non-renewable
  #       groundwater, consistent with the non-renewable groundwater pool (calcWaterDataMAgPIE, NonrenGW) that is
  #       available to agriculture in MAgPIE in all scenarios.
  #       Renewable water (ren_ag) does not depend on fossilGW.
  # To Do: scenario-specific fossilGW (after the harmonization period), see backlog 24
  common <- list(harmonized = TRUE,
                 comAg = TRUE,
                 fossilGW = TRUE,
                 cropmix = "hist_total", # c("maize", "rapeseed", "puls_pro"), (switch to proxy crops when ready)
                 yieldcalib = "TRUE:TRUE:actual:irrig_crop",
                 # To Do: decide on default argument (or make scenario-dependent)
                 multicropping = "TRUE:actual:irrig_crop")

  ### Question (Jan, Benni): Should the scenarios fade in / start in 2020 or
  ###                        in initialization year. How to do the fade-over?
  ###                        In this function? Fading from ssp2off to the respective scenarios?
  ###                        How to ensure consistency with PIWW?
  ### What about "cropmix"? proxycrops or historical cropmix?
  ### Groundwater fade-out? Allow fossil-GW for 1995-2025 and then fade out after?
  # Idea for calcPotIrrWatMAgPIE: report fossil GW separately and feed to GAMS,
  # so that it can be deactivated / faded out there
  # Which ranking to apply:   USD_ha (USD per hectare) for relative area return, or
  #                           USD_m3 (USD per cubic meter) for relative volumetric return;

  # Taking the Green Road: sustainability
  ssp1 <- list(efrMethod = "VMF:fair", # environment: environmental flow protection following VMF
               landScen = "potCropland:30by30", # environment: no irrigation in certain areas
               accessibilityrule = "CV:2", # technology
               irrigationsystem = "drip", # technology
               transDist = 100, # technology
               allocationrule = "optimization", # economics
               # economics: global ranking by relative volumetric return and full potential
               rankmethod = "USD_m3:GLO:TRUE",
               gainthreshold = 50, # economics (above 50 USD/ha: irrigation takes place)
               efp = "on",
               nonAg = "ssp1")

  # Middle of the Road
  ssp2 <- list(efrMethod = "VMF:fair", # environment: environmental flow protection following VMF
               landScen = "potCropland:WDPA", # environment: no irrigation in certain areas
               accessibilityrule = "CV:2", # technology
               irrigationsystem = "initialization", # technology
               transDist = 100, # technology
               allocationrule = "optimization", # economics
               # economics: global ranking by relative volumetric return,
               # but with reduced potential (slightly less optimal)
               rankmethod = "USD_m3:GLO:FALSE",
               gainthreshold = 50, # economics (above 50 USD/ha: irrigation takes place)
               efp = "off",
               nonAg = "ssp2")

  # Rocky Road: Regional rivalry
  ssp3 <- list(efrMethod = "VMF:fair", # environment: environmental flow protection following VMF
               landScen = "potCropland:30by30", # environment: no irrigation in certain areas
               accessibilityrule = "CV:2", # technology
               irrigationsystem = "initialization", # technology
               transDist = 100, # technology
               allocationrule = "upstreamfirst", # economics
               # economics: global ranking by relative volumetric return and full potential
               rankmethod = "USD_m3:GLO:TRUE",
               gainthreshold = 50, # economics (above 50 USD/ha: irrigation takes place)
               efp = "off",
               nonAg = "ssp3")

  # A Road Divided: Inequality
  # To do (SSP4): implement EFP protection based on development status
  ssp4 <- list(efrMethod = "VMF:fair", # environment: environmental flow protection following VMF
               landScen = "potCropland:30by30", # environment: no irrigation in certain areas
               accessibilityrule = "CV:2", # technology
               irrigationsystem = "initialization", # technology
               transDist = 100, # technology
               allocationrule = "optimization", # economics
               # economics: ranking by relative volumetric return and full potential based on country-level prices
               rankmethod = "USD_m3:ISO:TRUE",
               gainthreshold = 50, # economics (above 50 USD/ha: irrigation takes place)
               efp = "off",
               nonAg = "ssp3")

  # Taking the Highway: Fossil-fuel-driven development
  ssp5 <- list(efrMethod = "VMF:fair", # environment: environmental flow protection following VMF
               landScen = "potCropland:30by30", # environment: no irrigation in certain areas
               accessibilityrule = "CV:2", # technology
               irrigationsystem = "drip", # technology
               transDist = 100, # technology
               allocationrule = "optimization", # economics
               # economics: global ranking by relative volumetric return and reduced potential
               rankmethod = "USD_m3:GLO:FALSE",
               gainthreshold = 50, # economics (above 50 USD/ha: irrigation takes place)
               efp = "on",
               nonAg = "ssp5")

  # Sustainable development: economy-driven innovation
  sdpEI <- list(efrMethod = "Smakhtin:natural", # environment: very high EFP
                landScen = "potCropland:HalfEarth", # environment: no irrigation in certain areas
                accessibilityrule = "CV:2", # technology
                irrigationsystem = "drip", # technology: high efficiency
                transDist = 100, # technology
                allocationrule = "optimization", # economics
                # economics: global ranking by relative volumetric return and full potential
                rankmethod = "USD_m3:GLO:TRUE",
                gainthreshold = 50, # economics (above 50 USD/ha: irrigation takes place)
                efp = "on",
                nonAg = "sdpEI")

  # Sustainable development: resilient communities
  sdpRC <- list(efrMethod = "VMF:fair", # environment: environmental flow protection following VMF
                landScen = "potCropland:30by30", # environment: no irrigation in certain areas
                accessibilityrule = "CV:2", # technology
                irrigationsystem = "sprinkler", # technology: medium efficiency
                transDist = 100, # technology
                allocationrule = "optimization", # economics
                # economics: global ranking by relative volumetric return and full potential
                rankmethod = "USD_m3:GLO:TRUE",
                gainthreshold = 50, # economics (above 50 USD/ha: irrigation takes place)
                efp = "on",
                nonAg = "sdpRC")

  # Sustainable development: managing the global commons
  sdpMC <- list(efrMethod = "Smakhtin:natural", # environment: very high EFP
                landScen = "potCropland:HalfEarth", # environment: no irrigation in certain areas
                accessibilityrule = "CV:2", # technology
                irrigationsystem = "drip", # technology: high efficiency
                transDist = 100, # technology
                allocationrule = "optimization", # economics
                # economics: global ranking by relative volumetric return and full potential
                rankmethod = "USD_m3:GLO:TRUE",
                gainthreshold = 50, # economics (above 50 USD/ha: irrigation takes place)
                efp = "on",
                nonAg = "sdpMC")

  scenarios <- list(ssp1    = ssp1,
                    ssp2    = ssp2,
                    ssp2efp = utils::modifyList(ssp2, list(efp = "on")),
                    # Sensitivity scenario (not harmonized): stricter accessibility of discharge
                    # (only discharge up to the 25 % quantile of monthly flows is accessible;
                    # applies to potential irrigation only, not to committed and non-agricultural uses)
                    ssp2strict = utils::modifyList(ssp2, list(accessibilityrule = "Q:0.25", harmonized = FALSE)),
                    ssp3    = ssp3,
                    ssp4    = ssp4,
                    ssp5    = ssp5,
                    sdpEI   = sdpEI,
                    sdpRC   = sdpRC,
                    sdpMC   = sdpMC)

  return(lapply(scenarios, function(s) utils::modifyList(common, s)))
}
