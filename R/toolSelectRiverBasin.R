#' @title        toolSelectRiverBasin
#' @description  returns coordinates of cells that belong to chosen basin
#'
#' @param basinname river basin name as listed in mapping file RiverBasinMapping.csv
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
