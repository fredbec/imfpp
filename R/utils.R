#' Internal function to Map Countries in IMF Country Categorizations to Columns
#' in Country Data in IMF WEO forecast data
#'
#' @param countryGroups data.table containing
#' @param WEOcountries vector of countries from WEO data
#' @param groupNames which of the columns in CountryGroups should be mapped to
#' new variable
#' @param altNames optional; alternative Names for the groups (defaults to using
#' groupNames)
#'
#' @importFrom purrr map_dfc
#' @importFrom purrr pmap_chr
#' @importFrom purrr map_chr
#'
#' @return a vector with country categorization
#'
#'
countryGroup.map <- function(countryGroups,
                             WEOcountries,
                             groupNames,
                             altNames = NULL){

  if(is.null(altNames)){
    altNames <- groupNames
  }


  #new column with country classification
  #apply function to groupNames that again applies a function to match country
  #names from WEO data to countries in countryGroups

  #setNames(data.frame()) is done to prevent messaging of setting names
  cmap <- purrr::map_dfc(seq_along(groupNames), function(icol)
    setNames(data.frame(purrr::map_chr(WEOcountries, function(ctry)
      ifelse(ctry %in% countryGroups[[groupNames[icol]]], altNames[icol], ""))),
    paste0("name", icol)))|>
    purrr::pmap_chr(paste0)

  return(cmap)
}
