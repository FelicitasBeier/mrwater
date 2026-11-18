#' @title        toolSelectRiverBasin
#' @description  returns coordinates of cells that belong to chosen basin
#'
#' @param basinname river basin name as listed in mapping file RiverBasinMapping.csv
#'
#' @details Basin names in RiverBasinMapping.csv were assigned by overlaying the
#'          mrwater river basins (all cells with the same endcell) with the
#'          GRDC Major River Basins of the World. A name was assigned if the
#'          overlap covers at least 50 % of both the mrwater basin and the GRDC
#'          basin and the mrwater basin has at least 5 cells. Names follow GRDC
#'          (title case); duplicate names carry the receiving sea in brackets.
#'          Some names were set manually (e.g. Chang Jiang (Yangtze), Syr-Darya).
#'          Source: GRDC (2020): GRDC Major River Basins. Global Runoff Data
#'          Centre. 2nd, rev. ed. Koblenz: Federal Institute of Hydrology (BfG);
#'          incorporating data from HydroSHEDS (Lehner 2013, WWF).
#'
#' @return       list of cell names
#' @author       Felicitas Beier
#'
#' @export

toolSelectRiverBasin <- function(basinname) {

  # River structure attributes
  rs     <- readRDS(system.file("extdata/riverstructure_stn_coord.rds", package = "mrwater"))
  rs$iso <- toolGetMappingCoord2Country()$iso

  # Read in basin name - endcell mapping
  basinMap <- read.csv(system.file("extdata/RiverBasinMapping.csv", package = "mrwater"))
  if (!basinname %in% basinMap$Basin.Name) {
    stop("River basin '", basinname, "' is not listed in RiverBasinMapping.csv. ",
         "See inst/extdata/RiverBasinMapping.csv of mrwater for available basin names.")
  }

  # Select cells that belong to selected basin
  c     <- basinMap$Endcell[basinMap$Basin.Name == basinname]
  iso   <- rs$iso[rs$endcell == c]
  coord <- rs$coordinates[rs$endcell == c]

  out <- paste(coord, iso, sep = ".")

  return(out)
}
