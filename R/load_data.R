#' Downloads WEO forecast data from the internet
#'
#' @return nothing, downloads data
#' @export
#'
#' @examples download.data()
download.data <- function(){


  weourl <- "https://www.imf.org/-/media/Files/Publications/WEO/WEO-Database/WEOhistorical.ashx"
  #mode "wb" (otherwise won't be readable)
  download.file(weourl, "WEOforecasts.xlsx", mode = "wb")

  countrydataurl <- "https://www.imf.org/-/media/Files/Publications/fiscal-monitor/2023/April/English/fm-database-april-2023.ashx"
  download.file(countrydataurl, "FMEconGroup.xlsx", mode = "wb")

  gdpdataurl <- "https://api.worldbank.org/v2/en/indicator/NY.GDP.PCAP.KD?downloadformat=excel"
  download.file(gdpdataurl, "WB_GDPpC.xls", mode = "wb")

  lfdataurl <- "https://api.worldbank.org/v2/en/indicator/SL.TLF.CACT.NE.ZS?downloadformat=excel"
  download.file(lfdataurl, "WB_LFP.xls", mode = "wb")


}


#' Title
#'
#' @param sheetname name of sheet
#' @param explicitMissings should missings be explicit, default: TRUE
#' @param filename File name of Excel file
#' @param truevalExpand should the historical "forecasts" (i.e. revisions) be
#' included as extra variables
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
                       filename = "WEOforecasts.xlsx",
                       truevalExpand = TRUE){

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
    .d(order(country, target_year, -horizon), )


  #make explicit missing values
  if(explicitMissings){

    #expand data to include all combinations
    allCombs <- weoData |>
      tidyr::expand(
        tidyr::nesting(country, WEO_Country_Code, ISOAlpha_3Code), #all redundant variables
        target,
        forecast_year,
        horizon) |>
      setDT() |>
      .d(, target_year := floor(forecast_year + horizon)) |>
      .d(, forecast_season := data.table::fcase(
        horizon %% 1 == 0.5, "S",
        horizon %% 1 == 0, "F")) |>
      .d(, type := data.table::fcase(
        horizon < 0, "historical",
        horizon >= 0, "prediction"
      ))

    weoData <- weoData |>
      merge(
        allCombs,
        by = c("country", "WEO_Country_Code", "ISOAlpha_3Code",
               "target",
               "target_year", "forecast_season", "horizon",
               "forecast_year", "type"),
        all.y = TRUE #ensures NA introduction if comb not in weoData
      ) |>
      setkey(NULL) |>
      .d(order(country, target_year, -horizon))

  }

  if(truevalExpand){

    truevals <- weoData[horizon<0,
                        .(country, WEO_Country_Code, target,
                          target_year, prediction, horizon)] |>
      .d(, horizon := paste0("tv_", -horizon)) |>
      dcast(country + WEO_Country_Code + target_year + target ~ horizon,
            value.var = "prediction")

    weoData <- weoData |>
      .d(horizon >= 0, ) |>
      merge(truevals, by = c("country", "WEO_Country_Code", "target",
                             "target_year"),
            all.x = TRUE)


    return(weoData)
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
    setnames(c("year"), c("target_year")) |>

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
    .d(, horizon := (target_year - forecast_year) + hor_add) |>
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
                         truevalExpand = TRUE,
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
                   truevalExpand = truevalExpand,
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
#' @param download Should data be downloaded (default) or read from local directory?
#' Note: Geography data must be read from local directory
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
download.process.weo <- function(download = TRUE,
                                 sheets = c("ngdp_rpch", "pcpi_pch"),
                                 explicitMissings = TRUE,
                                 target_filename = "WEOforecasts_tidy.csv",
                                 truevalExpand = TRUE,
                                 includeCountryGroups = TRUE,
                                 fileCountryCat = "FMEconGroup.xlsx",
                                 includeGDPData = TRUE,
                                 GDPyearLower = 1990,
                                 GDPyearUpper = NULL,
                                 fileGDP = "WB_GDPpc.xls",
                                 includeLFPData = TRUE,
                                 LFPyearLower = 1990,
                                 LFPyearUpper = NULL,
                                 fileLFP = "WB_LFP.xls",
                                 includeGeoData = TRUE,
                                 fileGeo = "CountryLocs.xlsx"){

  #download from WEO source
  if(download){
    download.data()
  }


  .d <- `[`

  message("tidying data")
  tidiedWEO <- read.weodata(sheets = sheets,
                            explicitMissings = explicitMissings,
                            truevalExpand = truevalExpand)

  if(includeCountryGroups){
    message("merging country group info")
    tidiedWEO <- tidiedWEO |>
      incl.country.cat(fileCountryCat = "FMEconGroup.xlsx")

  }

  if(includeGDPData){
    message("merging GDP data")

    tidiedWEO |>
      .d(country == "Euro area", ISOAlpha_3Code := "EMU")

    gdpdat <- read.gdpdat(yearLower = GDPyearLower,
                          yearUpper = GDPyearUpper,
                          fileName = fileGDP)

    #print(names(tidiedWEO))
    tidiedWEO <- tidiedWEO |>
      merge(gdpdat, by = c("ISOAlpha_3Code"), all.x = TRUE)
  }

  if(includeLFPData){
    message("merging LFP data")

    tidiedWEO |>
      .d(country == "Euro area", ISOAlpha_3Code := "EMU")

    lfpdat <- read.lfpdat(yearLower = LFPyearLower,
                          yearUpper = LFPyearUpper,
                          fileName = fileLFP)

    #print(names(tidiedWEO))
    tidiedWEO <- tidiedWEO |>
      merge(lfpdat, by = c("ISOAlpha_3Code"), all.x = TRUE)
  }

  if(includeGeoData){
    message("merging Geography Data")

    geodat <- read.geodat(fileName = fileGeo)

    tidiedWEO <- tidiedWEO |>
      merge(geodat, by = c("ISOAlpha_3Code"), all.x = TRUE)

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
  groupData <- readxl::read_xlsx(fileCountryCat,
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


  #change dummy variables to actual dummies, code in "NA"
  tidiedWEO <-
    tidiedWEO |>
    .d(, g7 := ifelse(g7 == "G7", 1, 0))|>
    .d(, euro := ifelse(euro == "Euro Area", 1, 0))|>
    .d(, g20 := ifelse(g20 == "G20", 1, 0)) |>
    .d(economy == "", economy := "<NA>") |>
    .d(geolInc == "", geolInc := "<NA>") |>
    .d(geoEmer == "", geoEmer := "<NA>") |>
    .d(oil == "", oil := "<NA>")

  return(tidiedWEO)
}

#' This is a function to read in and tidy GDP per capita data from World Bank
#'
#' @param yearRange years to average over to get average GDP
#'
#' @import data.table
#' @importFrom readxl read_excel
#'
#' @return data.table with country categorization
#' @export
#'
read.gdpdat <- function(yearLower = 1990,
                        yearUpper = NULL,
                        fileName = "WB_GDPpc.xls"){

  #for piping data.table
  .d <- `[`

  gdpdat <- readxl::read_excel(here(fileName), sheet = "Data", range = "A4:BN270") |>
    setDT()

  if(is.null(yearUpper)){
    yearUpper <- names(gdpdat)[length(names(gdpdat))] |> as.numeric()
  }

  #check there is only one variable
  if(length(unique(gdpdat$`Indicator Name`)) > 1 | length(unique(gdpdat$`Indicator Code`)) > 1){
    stop("more than one variable")
  } else { #remove redundant variables
    gdpdat[, "Indicator Name" := NULL]
    gdpdat[, "Indicator Code" := NULL]
  }

  #reshape to long
  gdpdat <- gdpdat |>
    melt(id.vars = c("Country Name", "Country Code"),
         value.name = "gdppc",
         variable.name = "year",
         variable.factor = FALSE) |>
    setnames(old = c("Country Name", "Country Code"),
             new = c("country", "ISOAlpha_3Code")) |>
    .d(, year := as.numeric(year)) |>
    .d(year >= yearLower & year <= yearUpper) |>
    .d(, .(meangdppc = mean(gdppc, na.rm = TRUE)), by = c("ISOAlpha_3Code", "country")) |>
    .d(country == "Kosovo", ISOAlpha_3Code := "KOS") |>
    .d(country == "West Bank and Gaza", ISOAlpha_3Code := "WBG") |>
    .d(,country := NULL) |> #remove country (redundant)
    .d()


  return(gdpdat)
}

#' This is a function to read in and tidy Labor Force Participation Data from World Bank
#'
#' @param yearRange years to average over to get average LFP
#'
#' @import data.table
#' @importFrom readxl read_excel
#'
#' @return data.table with country categorization
#' @export
#'
read.lfpdat <- function(yearLower = 1990,
                        yearUpper = NULL,
                        fileName = "WB_LFP.xls"){

  #for piping data.table
  .d <- `[`

  lfpdat <- readxl::read_excel(here(fileName), sheet = "Data", range = "A4:BN270") |>
    setDT()

  if(is.null(yearUpper)){
    yearUpper <- names(lfpdat)[length(names(lfpdat))] |> as.numeric()
  }

  #check there is only one variable
  if(length(unique(lfpdat$`Indicator Name`)) > 1 | length(unique(lfpdat$`Indicator Code`)) > 1){
    stop("more than one variable")
  } else { #remove redundant variables
    lfpdat[, "Indicator Name" := NULL]
    lfpdat[, "Indicator Code" := NULL]
  }

  #reshape to long
  lfpdat <- lfpdat |>
    melt(id.vars = c("Country Name", "Country Code"),
         value.name = "lfp",
         variable.name = "year",
         variable.factor = FALSE) |>
    setnames(old = c("Country Name", "Country Code"),
             new = c("country", "ISOAlpha_3Code")) |>
    .d(, year := as.numeric(year)) |>
    .d(year >= yearLower & year <= yearUpper) |>
    .d(, .(meanlfp = mean(lfp, na.rm = TRUE)), by = c("ISOAlpha_3Code", "country")) |>
    .d(country == "Kosovo", ISOAlpha_3Code := "KOS") |>
    .d(country == "West Bank and Gaza", ISOAlpha_3Code := "WBG") |>
    .d(,country := NULL) |> #remove country (redundant)
    .d()


  return(lfpdat)
}



#' This is a function to read in and tidy Geography data (latitude and longitude)
#'
#' @param fileName = "
#'
#' @import data.table
#' @importFrom readxl read_excel
#'
#' @return data.table with country categorization
#' @export
#'
read.geodat <- function(yearLower = 1990,
                        yearUpper = NULL,
                        fileName = "CountryLocs.xlsx"){

  #for piping data.table
  .d <- `[`

  #read in country codes
  codedat <- readxl::read_excel(here(fileName), sheet = "CCodes", range = "A1:D250") |>
    setDT() |>
    .d(, .(ISOAlpha_2Code, ISOAlpha_3Code)) #keep only code map

  geodat <- readxl::read_excel(here(fileName), sheet = "Data", range = "A1:D246") |>
    setDT() |>
    .d(, .(country, lat, long)) |>
    .d(codedat, on = c("country" = "ISOAlpha_2Code")) |> #merge with code map
    .d(, .(ISOAlpha_3Code, lat, long)) |>
    .d(!is.na(lat))


  return(geodat)
}
