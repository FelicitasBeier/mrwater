#' @title       downloadJaegermeyr2015
#' @description Downloads irrigation system suitability per crop type
#'              and country-level irrigation system shares from Jaegermeyr (2015)
#'
#' @param subtype Data to be downloaded:
#'                "systemShare": irrigation system share
#'                               as provided in SI of Jaegermeyr et al. (2015),
#'                               based on FAO 2014, ICID 2012 and Rohwer et al. 2007);
#'                "systemSuitability": biophysical and technical irrigation system suitability
#'                                     by crop type (CFT) as provided in Table 2 of Jaegermeyr et al. (2015)
#'                                     based on Sauer et al. (2010) and Fischer et al (2012).
#'
#' @author Felicitas Beier
#'
#' @examples
#' \dontrun{
#' readSource("Jaegerymeyr2015", convert = FALSE)
#' }
#' @importFrom utils download.file person bibentry

downloadJaegermeyr2015 <- function(subtype) {

  # link to data
  zenodolink <- "https://zenodo.org/records/18696907/"

  # different subtypes that can be selected in read function
  if (subtype == "systemShare") {
    fname <- "Jaegermeyr-2015-supplement_shr.csv"
  } else if (subtype == "systemSuitability") {
    fname <- "Jaegermeyr2015_Table2_main.csv"
  }

  # download data
  download.file(paste0(zenodolink, "files/", fname, "?download=1"), destfile = fname, mode = "wb")

  # Compose meta data by adding elements that are the same for all subtypes.
  return(list(url           = zenodolink,
              doi           = "10.5281/zenodo.18670898",
              title         = "Irrigation System Data by Jaegermeyr et al. (2015)",
              description   = paste0("Irrigation system share and irrigation system suitability data ",
                                     "published as part of article by Jaegermeyr et al. (2015)"),
              author        = person("Jonas Jaegermeyr", email = "jonas.jaegermeyr@pik-potsdam.de",
                                     comment = "https://orcid.org/0000-0002-8368-0018"),
              unit          = "1",
              version       = "1.0",
              license       = "CC Attribution 3.0",
              reference     = bibentry("Article",
                                       title = paste("Water savings potentials of irrigation systems: ",
                                                     "global simulation of processes and linkages"),
                                       author = c(person("Jonas Jaegermeyr", email = "jonas.jaegermeyr@pik-potsdam.de",
                                                         comment = "https://orcid.org/0000-0002-8368-0018"),
                                                  person("Dieter", "Gerten"),
                                                  person("Jens", "Heinke"),
                                                  person("Sibyll", "Schaphoff"),
                                                  person("Matti", "Kummu"),
                                                  person("Wolfang", "Lucht")),
                                       year = "2015",
                                       journal = "Hydrol. Earth Syst Sci.",
                                       volume = "19",
                                       pages = "3073-3091",
                                       url = "https://doi.org/10.5194/hess-19-3073-2015",
                                       doi = "10.5194/hess-19-3073-2015")))
}
