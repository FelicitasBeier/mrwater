#' @title fullWATER4MAGPIE
#' @description Function that produces the complete cellular water data set
#'              for the MAgPIE model.
#'              Note: This is only needed temporarily until it is included
#'              in the fullCELLULARMAGPIE
#'
#' @param rev data revision which should be used as input (numeric_version).
#' @param ctype aggregation clustering type, which is a combination of a single letter,
#'              indicating the cluster methodology, and a number, indicating the number
#'              of resulting clusters. Available methodologies are
#'              - hierarchical clustering (h),
#'              - normalized k-means clustering (n) and
#'              - combined hierarchical/normalized k-means clustering (c).
#'              In the latter hierarchical clustering is used to determine the cluster
#'              distribution among regions whereas normalized k-means is used for the
#'              clustering within a region.
#' @param dev development suffix to distinguish development versions for the same data revision.
#'            This can be useful to distinguish parallel lines of development.
#' @param climatetype Global Circulation Model to be used
#' @param lpjml Defines LPJmL version for crop/grass and natveg specific inputs
#' @param isimip Defines isimip crop model input which replace maiz, tece, rice_pro and soybean
#' @param emu_id Pasture Soil carbon emulator ID
#' @param clusterweight Should specific regions be resolved with more or less detail?
#'                      Values > 1 mean higher share, < 1 lower share
#'                      e.g. cfg$clusterweight <- c(LAM=2) means that
#'                      a higher level of detail for region LAM if set to NULL
#'                      all weights will be assumed to be 1. Examples:
#'                      c(LAM=1.5,SSA=1.5,OAS=1.5) or c(LAM=2,SSA=2,OAS=2)
#' \code{\link[madrat]{setConfig}} (e.g. for setting the mainfolder if not already set properly).
#'
#' @author Felicitas Beier, Kristine Karstens, Jan Philipp Dietrich
#' @seealso
#' \code{\link[madrat]{readSource}},\code{\link[madrat]{getCalculations}},\code{\link[madrat]{calcOutput}},
#' \code{\link[madrat]{setConfig}}
#' @examples
#' \dontrun{
#' retrieveData("CELLULARMAGPIE", rev = numeric_version("12"),
#'              mainfolder = "pathtowhereallfilesarestored")
#' }
#' @importFrom madrat setConfig getConfig
#' @importFrom magpiesets findset
#' @importFrom digest digest
#' @importFrom luplot plotregionscluster
#' @importFrom ggplot2 ggsave
#' @importFrom withr local_options

fullCELLULARMAGPIE <- function(rev = numeric_version("0.1"), dev = "",
                               ctype = "c200",
                               climatetype = "MRI-ESM2-0:ssp370",
                               lpjml = c(natveg = "LPJmL4_for_MAgPIE_44ac93de",
                                         crop = "ggcmi_phase3_nchecks_9ca735cb",
                                         grass = "lpjml5p2_pasture"),
                               isimip = NULL,
                               clusterweight = NULL,
                               emu_id = NULL) { # nolint

  "!# @pucArguments ctype clusterweight"

  withr::local_options(magclass_sizeLimit = 1e+12)

  ### Version settings ###
  if (rev < numeric_version("4.94")) {
    stop("mrmagpie(>= 1.35.2) does not support revision below 4.94 anymore. ",
         "Please use an older snapshot/version of the library, if you need older revisions.")
  }
  cells       <- "lpjcell"

  message(paste0("Start preprocessing for \n climatescenario: ", climatetype,
                 "\n LPJmL-Versions: ", paste(names(lpjml), lpjml, sep = "->", collapse = ", "),
                 "\n clusterweight: ", paste(names(clusterweight), clusterweight, sep = ":", collapse = ", "),
                 "\n isimip yield subtype: ", paste(names(isimip), isimip, sep = ":", collapse = ", ")))

  # Create version tag (will be returned at the very end of this function)
  versionTag <- paste(ctype,
                      gsub(":", "-", climatetype),
                      paste0("lpjml-", digest::digest(lpjml, algo = getConfig("hash"))),
                      sep = "_")
  versionTag <- ifelse(is.null(isimip), versionTag,
                       paste0(versionTag, "_isimip-",
                              digest::digest(isimip, algo = getConfig("hash"))))
  versionTag <- ifelse(is.null(clusterweight), versionTag,
                       paste0(versionTag, "_clusterweight-",
                              digest::digest(clusterweight, algo = getConfig("hash"))))
  versionTag <- ifelse(is.null(emu_id), versionTag,
                       paste0(versionTag, "_gsoilc-", emu_id))

  iniyear          <- "y1995"
  magYearsPastLong <- c("y1995", "y2000", "y2005", "y2010", "y2015")
  magYears         <- findset("time")
  shortYears       <- findset("t_all")
  lpjYears         <- seq(1995, 2100, by = 5)
  roundArea        <- 5
  stats            <- c("summary", "sum")

  # Clustering based on 67420 cells
  map      <- calcOutput("Cluster", ctype = ctype, weight = clusterweight, lpjml = lpjml,
                         clusterdata = "yield_airrig", aggregate = FALSE)
  weightID <- ifelse(is.null(clusterweight), "", paste0("_", names(clusterweight), clusterweight, collapse = ""))
  clustermapname <- sub("\\.[^.]*$", ".rds",
                        paste0("clustermap_rev", rev, dev, "_", ctype, "_67420",
                               weightID, "_", getConfig("regionmapping")))
  addMapping(clustermapname, map)

  # plot map with regions and clusters
  clustermap <- readRDS(clustermapname) # nolint
  p <- plotregionscluster(clustermap, cells = "lpjcell") # nolint
  suppressWarnings(ggsave(sub(".rds", ".pdf", sub("clustermap", "spamplot", clustermapname)),
                          p, height = 6, width = 10, scale = 1))

  # distinguish between region and superregion if mapping provides this distinction
  mapReg      <- toolGetMapping(getConfig("regionmapping"), type = "regional", where = "mappingfolder")
  superregion <- ifelse("superregion" %in% colnames(mapReg), "superregion", "region")




  #### Additional settings ####
  # until Hackathon branch is merged
  lpjml             <- "lpjml5.9.16-m1"
  climatetype       <- "MRI-ESM2-0:ssp370"

  # water specific settings
  irrigationsystem  <- "initialization"
  efrMethod         <- "VMF:fair"
  multicropping     <- "TRUE:actual:irrig_crop"

  accessibilityrule <- "CV:2"
  rankmethod        <- "USD_m3:GLO:TRUE"
  gainthreshold     <- 10 # to remove unproductive areas form potential irrigation (Note: temporary until we found solution for accounting for regional costs)
  allocationrule    <- "optimization"

  comAg             <- TRUE # potential includes priority for committed agricultural areas
  yieldcalib        <- FALSE
  # Question: should we activate yield-calibration for determination of PIWW?
  #           In MAgPIE, yields are being calibrated, so I guess it would be more consistent
  #           If so: global or country-level calibration (MAgPIE is regional)
  fossilGW  <- TRUE
  transDist <- 100

  cropmix   <- "hist_total" # cropmix as of LandInG
  landScen  <- "potCropland:NULL" # potential cropland and no land protection (for testing)
  # To Do: different area protection scenario settings for different scenarios

  # 14_yields
  # irrigated and rainfed yields with different aggregation weights
  calcOutput("Yields", source = c(lpjml = lpjml, isimip = isimip), climatetype = climatetype,
             round = 2, years = lpjYears,
             aggregate = "cluster",
             outputStatistics = stats, file = paste0("lpj_yields_", ctype, ".mz"),
             weighting = "totalLUspecific") # Note: We use a different weight than the default: "totalCrop"

  # 41 area equipped for irrigation
  # area committed for irrigation according to data in past
  # Note: currently, this is based on LandInG
  calcOutput("IrrigAreaCommitted",
             selectyears = magYearsPastLong, iniyear = iniyear, round = roundArea,
             aggregate = "cluster", file = paste0("area_irrig_", ctype, ".mz"))
  # Question (Benni): This used to be "avl_irrig_...", Should I rename such files, too?

  # 42 water demand
  # irrigation water requirements for chosen irrigation system settings
  # Question: Should we generate different scenarios for different irrigation efficiencies?
  #           (i.e., initialization, all_sprinkler, all_drip)
  #           This would mean an additional set/column in MAgPIE
  calcOutput("ActualIrrigWatRequirements", selectyears = lpjYears, iniyear = iniyear,
             lpjml = lpjml, climatetype = climatetype,
             irrigationsystem = irrigationsystem, multicropping = multicropping,
             aggregate = "cluster", file = paste0("irrig_req_crop", ctype, ".mz"))

  # 43 water availability
  # Potentially irrigated areas with same settings as PIWW for disaggregation
  # Question (Jan, Jens, Benni): Do we need this?
  calcOutput("PotIrrigAreas", cropAggregation = TRUE,
             lpjml = lpjml, climatetype = climatetype,
             selectyears = lpjYears, iniyear = iniyear,
             efrMethod = efrMethod, irrigationsystem = irrigationsystem,
             accessibilityrule = accessibilityrule, rankmethod = rankmethod,
             gainthreshold = gainthreshold, allocationrule = allocationrule,
             yieldcalib = yieldcalib, comAg = comAg,
             fossilGW = fossilGW, transDist = transDist,
             multicropping = multicropping,
             landScen, cropmix = cropmix,
             aggregate = "cluster", file = paste0("pia", ctype, ".mz"))

  # Question (Jan): Can I remove one dimension here in full-function? or do I need a separate function for that?
  calcOutput("PotWater", lpjml = lpjml, climatetype = climatetype,
             selectyears = lpjYears, iniyear = iniyear,
             efrMethod = efrMethod, irrigationsystem = irrigationsystem,
             accessibilityrule = accessibilityrule, rankmethod = rankmethod,
             gainthreshold = gainthreshold, allocationrule = allocationrule,
             yieldcalib = yieldcalib, comAg = comAg,
             fossilGW = fossilGW, transDist = transDist,
             multicropping = multicropping,
             landScen, cropmix = cropmix,
             aggregate = "cluster", file = paste0("piww", ctype, ".mz"))

  # To Do: remove non-renewable GW from this PIWW and report separately

  ### Question: Do we need to report EFRs (e.g., for postprocessing (e.g., PBs or disaggregation?))
  # calcOutput("EFRSmakthin", lpjml = lpjml, years = lpjYears, climatetype = climatetype,
  #            aggregate = "cluster", cells = cells,
  #            round = 6, seasonality = "grper",
  #            outputStatistics = stats, file = paste0("lpj_envflow_grper_", ctype, ".mz"))
  # calcOutput("EFRSmakthin", lpjml = lpjml, years = lpjYears, climatetype = climatetype,
  #            aggregate = "cluster", cells = cells,
  #            round = 6, seasonality = "total",
  #            outputStatistics = stats, file = paste0("lpj_envflow_total_", ctype, ".mz"))
  #
  # if (dev == "EFRtest") {
  #   calcOutput("EnvmtlFlow", lpjml = lpjml, years = lpjYears, climatetype = climatetype,
  #              aggregate = "cluster",
  #              round = 6, seasonality = "grper",
  #              outputStatistics = stats, file = paste0("envflow_grper_", ctype, ".cs3"))
  #   calcOutput("EnvmtlFlow", lpjml = lpjml, years = lpjYears, climatetype = climatetype,
  #              aggregate = "cluster",
  #              round = 6, seasonality = "total",
  #              outputStatistics = stats, file = paste0("envflow_total_", ctype, ".cs3"))
  # }

  ### Question: Do we need to report non-agricultural water use (for postprocessing)
  #### I think yes: for reporting of total water use (e.g., for water consumption PB)
  # To Do: make sure to harmonize with what entered in PIWW
  # calcOutput("WaterUseNonAg", datasource = "WATERGAP_ISIMIP", usetype = "all:all",
  #            selectyears = lpjYears, seasonality = "grper", lpjml = lpjml, climatetype = climatetype,
  #            aggregate = "cluster", cells = cells,
  #            outputStatistics = stats, file = paste0("watdem_nonagr_grper_", ctype, ".mz"))
  #
  # calcOutput("WaterUseNonAg", datasource = "WATERGAP_ISIMIP", usetype = "all:all",
  #            selectyears = lpjYears, seasonality = "total", lpjml = lpjml, climatetype = climatetype,
  #            aggregate = "cluster", cells = cells,
  #            outputStatistics = stats, file = paste0("watdem_nonagr_total_", ctype, ".mz"))

  ##### AGGREGATION ######

  # create info file
  writeInfo <- function(file, lpjmlData, resHigh, resOut, rev, cluster) {
    functioncall <- paste(deparse(sys.call(-3)), collapse = "")

    map <- toolGetMapping(type = "regional", where = "mappingfolder", name = getConfig("regionmapping"))
    regionscode <- regionscode(map)

    info <- c("lpj2magpie settings:",
              paste("* LPJmL data:", lpjmlData),
              paste("* Revision:", rev),
              "", "aggregation settings:",
              paste("* Input resolution:", resHigh),
              paste("* Output resolution:", resOut),
              paste("* Regionscode:", regionscode),
              "* Number of clusters per region:",
              paste(format(names(cluster), width = 5, justify = "right"), collapse = ""),
              paste(format(cluster, width = 5, justify = "right"), collapse = ""),
              paste("* Call:", functioncall))

    base::cat(info, file = file, sep = "\n")
  }
  nrClusterPerRegion <- substr(attributes(p$data)$legend_text, 6,
                               nchar(attributes(p$data)$legend_text) - 1)

  writeInfo(file = "info.txt",
            lpjmlData = climatetype,
            resHigh = "0.5",
            resOut = ctype,
            rev = rev,
            cluster = nrClusterPerRegion)

  mstools::toolWriteMadratLog()

  return(list(tag = versionTag,
              pucTag = sub("^[^_]*_", "", versionTag)))
}
