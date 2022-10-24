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
#' Also does some basic cleaning
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
                         includeCountryGroups = TRUE,
                         filename = "WEOforecasts.xlsx"){

  if(!file.exists(filename)){
    stop("File doesn't exist")
  }

  if(any(!sapply(sheets,
                function(x) x %in% c("ngdp_rpch", "pcpi_pch", "bca_gdp_bp6")))){
    stop("All sheet names must be one of \"ngdp_rpch\", \"pcpi_pch\", \"bca_gdp_bp6\"")
  }


  country_groups <- c("Advanced Economies",
                      "Emerging Market and Developing Economies",
                      "Euro area",
                      "World")

  #for piping data.table ops
  .d <- `[`

  # apply to sheets
  weoData <- sheets |>
    purrr::map_dfr(read.sheet,
                   explicitMissings = explicitMissings,
                   filename = filename) |>
   #rename countries
    .d(, country := data.table::fcase(
      country == "Türkiye", "Turkey",
      country == "Montenegro, Rep. of", "Montenegro",
      rep(TRUE, .N), country
    )) |>
    #include info about which "country" is actually a group of countries
    .d(, cgroup := data.table::fcase(
      country %in% country_groups, 1,
      rep(TRUE, .N), 0
    ))


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
                                 target_filename = "WEOforecasts_tidy.csv",
                                 includeCountryGroups = TRUE,
                                 fileCountryCat = "FMEconGroup.xlsx"){

  #download from WEO source
  download.data()

  message("tidying data")
  tidiedWEO <- read.weodata(sheets = sheets,
                            explicitMissings = explicitMissings)

  if(includeCountryGroups){
    message("merging country group info")
    tidiedWEO <- tidiedWEO |>
      incl.country.cat(fileCountryCat = "FMEconGroup.xlsx")

  }

  message("Saving tidied and cleaned data")
  data.table::fwrite(tidiedWEO, "WEOforecasts_tidy.csv")

}



#' This is a function to clean some minor details in the WEO forecasts data
#'
#' @param tidiedWEO tidied WEO data
#' @param fileCountryCat "FMEconGroup.xlsx"
#'
#' @import data.table
#' @importFrom readxl read_xlsx
#'
#' @return data.table with country categorization
#' @export
#'
incl.country.cat <- function(tidiedWEO,
                             fileCountryCat = "FMEconGroup.xlsx"){

  .d <- `[`

  ########read in and clean CountryCat data############
  groupData <- read_xlsx(fileCountryCat,
                         sheet = "Table A. Economy Groupings")

  names(groupData) <- gsub("[\r\n]|[1]", "",  names(groupData))
  #whitespace between words
  names(groupData) <- gsub("([a-z])([A-Z])", "\\1 \\2", names(groupData))

  #rename some countries for matching
  groupData <- groupData |>
    setDT() |>
    .d(, `Emerging Market Economies` := data.table::fcase(
      `Emerging Market Economies` == "The Bahamas", "Bahamas, The",
      rep(TRUE, .N), `Emerging Market Economies`
    )) |>
    .d(, `Low-Income Oil Producers` := data.table::fcase(
      `Low-Income Oil Producers` == "Congo, Rep of.", "Congo, Republic of",
      rep(TRUE, .N), `Low-Income Oil Producers`
    )) |>
    .d(, `Oil producers` := data.table::fcase(
      `Oil producers` == "Congo, Rep of.", "Congo, Republic of",
      rep(TRUE, .N), `Oil producers`
    )) |>
    .d(, `Low-Income Developing Sub-Saharan Africa` := data.table::fcase(
      `Low-Income Developing Sub-Saharan Africa` == "Congo, Rep. of", "Congo, Republic of",
      `Low-Income Developing Sub-Saharan Africa` == "Congo, Dem. Rep. of the", "Congo, Democratic Republic of the",
      rep(TRUE, .N), `Low-Income Developing Sub-Saharan Africa`
    ))


  ##########make new columns based on groups##########
  #categorize based on Advanced/Emerging/Low-Income Emerging (info in cols 1-3)
  tidiedWEO[["economy"]] <- countryGroup.map(groupData,
                                            tidiedWEO$country,
                                            names(groupData)[1:3])
  #euro area
  tidiedWEO[["euro"]] <- countryGroup.map(groupData,
                                         tidiedWEO$country,
                                         "Euro Area")

  #G7 and G20
  tidiedWEO[["g7"]] <- countryGroup.map(groupData,
                                       tidiedWEO$country,
                                         "G7")
  tidiedWEO[["g20"]] <- countryGroup.map(groupData,
                                        tidiedWEO$country,
                                         "G20")

  #geography emerging
  geoEmer <- names(groupData)[grepl("Emerging Market and*", names(groupData))]
  geoNames <- gsub("Emerging Market and Middle-Income ", "", geoEmer)

  tidiedWEO[["geoEmer"]] <- countryGroup.map(groupData,
                                            tidiedWEO$country,
                                            geoEmer,
                                            geoNames)

  #geography low-income
  geolInc <- names(groupData)[grepl("Low-Income Developing [^C]", names(groupData))]
  geoNames <- gsub("Low-Income Developing ", "", geolInc)

  tidiedWEO[["geolInc"]] <- countryGroup.map(groupData,
                                            tidiedWEO$country,
                                            geolInc,
                                            geoNames)

  #Oil countries
  oilC <- names(groupData)[grepl("*Oil*", names(groupData))]
  oilNames <- c("oillInc", "oil")
  tidiedWEO[["oil"]] <- countryGroup.map(groupData,
                                        tidiedWEO$country,
                                        oilC,
                                        oilNames)


  return(tidiedWEO)
}


