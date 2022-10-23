#' Downloads WEO forecast data from the internet
#'
#' @return nothing, downloads data
#' @export
#'
#' @examples download.data()
download.data <- function(){


  weourl <- "https://www.imf.org/external/pubs/ft/weo/data/WEOhistorical.xlsx"

  #mode "wb" (otherwise won't be readable)
  download.file(weourl, "WEOforecasts.xlsx", mode = "wb")

}


#' Title
#'
#' @param sheetname name of sheet
#' @param explicitMissings should missings be explicit, default: TRUE
#' @param filename File name of Excel file
#'
#' @return data.table object
#' @export
#'
#' @importFrom readxl read_xlsx
#' @importFrom tidyr expand
#' @importFrom tidyr nesting
#' @importFrom dplyr right_join
#'
#' @import data.table
#'
#' @examples read.sheet("ngdp_rpch")
#'
read.sheet <- function(sheetname,
                       explicitMissings = TRUE,
                       filename = "WEOforecasts.xlsx"){

  if(!sheetname %in% c("ngdp_rpch", "pcpi_pch", "bca_gdp_bp6")){
    stop("Sheet name must be one of \"ngdp_rpch\", \"pcpi_pch\", \"bca_gdp_bp6\"")
  }

  if(!file.exists(filename)){
    stop("File doesn't exist")
  }

  #this if for piping with data.table
  .d <- `[`

  #read in data from sheet
  #split up by year and country before tidying
  weoSheet <- read_xlsx(path = filename,
                       sheet = sheetname) |>
    data.table::setDT() |>
    split(by = c("country", "year"))


  weoData <- lapply(weoSheet, tidy.data) |>
    rbindlist() |>
    .d(order(country, target_date, horizon), )


  #make explicit missing values
  if(explicitMissings){

    #expand data to include all combinations
    allCombs <- weoData |>
      tidyr::expand(
        tidyr::nesting(country, WEO_Country_Code, ISOAlpha_3Code), #all redundant variables
        target,
        #nesting prevents combinations that don't exist to appear
        #for instance, forecast_date 2022 with target 1999
        tidyr::nesting(forecast_season, horizon, forecast_year, target_date, type))


    weoData <- weoData |>
      merge(
        allCombs,
        by = c("country", "WEO_Country_Code", "ISOAlpha_3Code",
               "target",
               "target_date", "forecast_season", "horizon",
               "forecast_year", "type"),
        all.y = TRUE #ensures NA introduction if comb not in weoData
      ) |>
      setkey(NULL)
  }

  return(weoData)
}


#' internal function to tidy the data that come from the IMF WEO historical forecast
#' Excel sheets; only works if it is handed a subset of one country and
#' one year (read.sheet takes care of thus)
#'
#' @param weoSheet untidied sheet data from WEOforecasts.xlsx
#'
#' @return tidied data.table
#'
#' @import data.table
#'
#'
tidy.data <- function(weoSheet){

  if(!is.data.table(weoSheet)){
    stop("must be data.table")
  }

  #for piping data.table operations
  .d <- `[`

  tidiedSheet <- weoSheet |>
    #reshape to long (all extra columns)
    melt(id.vars = c("country", "WEO_Country_Code",
                     "ISOAlpha_3Code", "year")) |>

    #exclude combs of year and forecast_date without values
    #e.g. if forecast_date >> year
    .d(value != ".") |>
    .d(, prediction := as.numeric(value)) |>
    #remove value column
    .d(, value := NULL) |>
    #year column is the target date
    setnames(c("year"), c("target_date")) |>

    #get forecast year, season and target from variable column,
    #then remove column
    .d(, forecast_year := as.numeric(
      gsub("([a-z]*[A-Z]*_*)", "",
           variable))) |>
    .d(, forecast_season :=
         gsub("[a-z]*[0-9]*_*", "",
              variable)) |>
    .d(, target :=
         gsub("[A-Z]*[0-9}]*", "",
              variable)) |>
    .d(, variable := NULL) |>

    #make horizon column
    .d(, hor_add := data.table::fcase(
      forecast_season == "S", 0.5,
      forecast_season == "F", 0)) |>
    .d(, horizon := (target_date - forecast_year) + hor_add) |>
    .d(, hor_add := NULL) |>

    #type variable: historical if fc year > target year
    .d(, type := data.table::fcase(
      horizon < 0, "historical",
      horizon >= 0, "prediction"
    ))


  return(tidiedSheet)
}


#' Simple wrapper to bind rows for multiple targets in IMF WEO Data
#'
#' @param sheets which sheets to read in. One (or more) of "ngdp_rpch",
#' "pcpi_pch", "bca_gdp_bp6"
#' @param explicitMissings should missing prediction be explicitly included
#' (coded as NA values), default: TRUE
#' @param filename name of the Excel file containing the WEO forecasts
#'
#' @importFrom purrr map_dfr
#'
#'
#' @return a data.table object of tidied and forecast data of the specified targets
#' @export
#'
#' @examples read.weodata(sheets = c("pcpi_pch"))
read.weodata <- function(sheets = c("ngdp_rpch", "pcpi_pch"),
                         explicitMissings = TRUE,
                         filename = "WEOforecasts.xlsx"){

  if(!file.exists(filename)){
    stop("File doesn't exist")
  }

  if(any(!sapply(sheets,
                function(x) x %in% c("ngdp_rpch", "pcpi_pch", "bca_gdp_bp6")))){
    stop("All sheet names must be one of \"ngdp_rpch\", \"pcpi_pch\", \"bca_gdp_bp6\"")
  }

  # apply to sheets
  weoData <- sheets |>
    purrr::map_dfr(read.sheet,
                   explicitMissings = explicitMissings,
                   filename = filename)

  return(weoData)

}



#' Function to download, tidy and save WEO data
#'
#' @param sheets which sheets to read in. One (or more) of "ngdp_rpch",
#' "pcpi_pch", "bca_gdp_bp6". Default: "ngdp_rpch", "pcpi_pch" (Inflation and
#' GDP growth)
#' @param explicitMissings should missing prediction be explicitly included
#' (coded as NA values), default: TRUE
#' @param target_filename name of the target csv file where tidied data will
#' be stored
#'
#' @return nothing, saves tidied data in directory
#' @export
#'
download.process.weo <- function(sheets = c("ngdp_rpch", "pcpi_pch"),
                                 explicitMissings = TRUE,
                                 target_filename = "WEOforecasts_tidy.csv"){

  #download from WEO source
  download.data()

  message("tidying data")
  tidiedWEO <- read.weodata(sheets = sheets,
                            explicitMissings = explicitMissings)

  message("Saving tidied data")
  data.table::fwrite(tidiedWEO, "WEOforecasts_tidy.csv")

}
