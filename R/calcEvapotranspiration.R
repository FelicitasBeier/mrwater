#' @title calcEvapotranspiration
#'
#' @description Calculates evapotranspiration (ET) from LPJmL inputs
#'              (transpiration, evaporation, interception)
#'
#' @param selectyears    Years to be returned
#' @param runtype        Select crop and management type for which ET should be calculated
#'                       options: `cft:ir` (crop irrigated in irrigated growing period of respective crop),
#'                                `cft:noir` (crop not irrigated in irrigated growing period of respective crop),
#'                                `grass:ir` (grass irrigated in irrigated growing period of crop),
#'                                `grass:noir` (grass not irrigated in irrigated growing period of crop)
#' @param lpjml          LPJmL version required for respective inputs: natveg or crop
#' @param climatetype    Switch between different climate scenarios or
#'                       historical baseline "GSWP3-W5E5:historical"
#' @return magpie object in cellular resolution
#' @author Felicitas Beier, Jens Heinke
#'
#' @importFrom madrat getFromComment
#'
#' @examples
#' \dontrun{
#' calcOutput("Evapotranspiration", aggregate = FALSE)
#' }

calcEvapotranspiration <- function(selectyears, runtype,
                                   lpjml, climatetype) {

  #########################
  ### Extract arguments ###
  #########################
  if (unlist(strsplit(runtype, split = ":"))[2] == "ir") {
    # To Do: Ensure correct run is selected when we changed from crops vs. cropsIrrigswap to cropsRF vs. cropsIR
    runfolder <- "crops" # in future (To Do!): from cropsIR run (i.e. no more runfolder distinction will be needed)
    mngt      <- "irrigated"
  } else if (unlist(strsplit(runtype, split = ":"))[2] == "noir") {
    runfolder <- "cropsIrrigswap" # in future (To Do!): from cropsIR run (i.e. no more runfolder distinction will be needed)
    mngt      <- "rainfed"
  }
  # subtype function for monthly input data
  if (grepl("cft", unlist(strsplit(runtype, split = ":"))[1])) {
    .subtype <- function(x, runfolder) {
      out <- paste(runfolder,
                   paste0("cft_", x),
                   sep = ":")
      return(out)
    }
  } else if (grepl("grass", unlist(strsplit(runtype, split = ":"))[1])) {
    .subtype <- function(x, runfolder) {
      out <- paste(runfolder,
                   paste0("cft_", x, "_grass"),
                   sep = ":")
      return(out)
    }
  }

  ####################
  ### Read in data ###
  ####################
  # transpiration (in m^3/ha)
  transp <- calcOutput("LPJmLHarmonize", subtype = .subtype(x = "transp", runfolder = runfolder),
                       lpjmlversion = lpjml, climatetype = climatetype,
                       aggregate = FALSE)[, selectyears, mngt]
  # evaporation (in m^3/ha)
  evap <- calcOutput("LPJmLHarmonize", subtype = .subtype(x = "evap", runfolder = runfolder),
                     lpjmlversion = lpjml, climatetype = climatetype,
                     aggregate = FALSE)[, selectyears, mngt]
  # interception (in m^3/ha)
  interc <- calcOutput("LPJmLHarmonize", subtype = .subtype(x = "interc", runfolder = runfolder),
                       lpjmlversion = lpjml, climatetype = climatetype,
                       aggregate = FALSE)[, selectyears, mngt]
  # extract unit
  unit <- getFromComment(transp, "unit")

  ### Correction Start ###
  ### To Do (Feli): Delete once new LPJmL runs are ready! This is only a temporary fix due to a typo in LPJmL
  if (runtype == "grass:ir") {
    transp <- transp * 1e6
  }
  ### Correction End   ###

  ####################
  ### Calculations ###
  ####################
  # calculate evapotranspiration
  et <- transp + evap + interc
  description <- "evapotranspiration"

  ##############
  ### Checks ###
  ##############
  if (any(is.na(et))) {
    stop("mrwater::calcEvapotranspiration produced NA values")
  }
  if (any(et < 0)) {
    stop("mrwater::calcEvapotranspiration produced negative values")
  }

  return(list(x = et,
              weight = NULL,
              unit = unit,
              description = description,
              isocountries = FALSE))
}
