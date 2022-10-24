
<!-- README.md is generated from README.Rmd. Please edit that file -->

# impfpp - tidy WEO forecast data

<!-- badges: start -->
<!-- badges: end -->

Small package to download and tidy forecast data from the IMF World
Economic Outlook.

## Installation

You can install the development version of imfpp from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("fredbec/imfpp")
```

## Example

If you simply want to download, tidy and save all available forecasts
for GDP growth and inflation (and include info on country groups), run
the following

``` r
library(imfpp)
## basic example code
download.process.weo()
```

If you want different variables, exclude country groups and/or don’t
want explicit missing values, alternatively run

``` r
download.process.weo(sheets = c("ngdp_rpch", "bca_gdp_bp6"))
download.process.weo(explicitMissings = FALSE)
download.process.weo(includeCountryGroups = FALSE)
```

The variables included in the dataset are as follows:

| Variable         | Explanation                                                                                                                                                          |
|------------------|:---------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| country          | country names as they appear in the WEO data (with minimal changes)                                                                                                  |
| WEO_Country_Code | country codes as they appear in the WEO data                                                                                                                         |
| ISOAlpha_3Code   | another country code                                                                                                                                                 |
| target           | target variable, one of ngdp_rpch (GDP growth), pcpi_pch (CPI inflation), bca_gdp_bp6 (account balance, percent of GDP)                                              |
| target_date      | the year the forecast is made for                                                                                                                                    |
| forecast_season  | the season the forecast was made in (S for Spring, F for fall)                                                                                                       |
| horizon          | the forecast horizon (e.g. 0.5 refers to a forecast from spring made for the current year, 1 refers to a forecast from fall made for next year)                      |
| forecast_year    | the year the forecast was made (forecast_year + horizon = target_date)                                                                                               |
| type             | prediction or data correction (-\> “historical”)                                                                                                                     |
| prediction       | predicted value for the target                                                                                                                                       |
| cgroup           | is country a country group (e.g. “Advanced Economies”)                                                                                                               |
| economy          | categorization of country economy (Adv. Economies, Emerging Market Economies, Low-Income Developing Countries)                                                       |
| euro             | categorization into Euro Area countries (binary)                                                                                                                     |
| g7               | categoriation into G7 countries (binary)                                                                                                                             |
| g20              | categorization into G20 countries (binary)                                                                                                                           |
| geoEmer          | categorization of Emerging Market and Middle-Income Economies into geographical regions (Asia, Europe, Latin America, Middle East North Africa and Pakistan, Africa) |
| geolInc          | categorization of Low-Income Developing Economies into geographical regions (Asia, Latin America, Sub-Saharan Africa, Others)                                        |
| oil              | categorization of Oil-Producing countries                                                                                                                            |

ToDo: Embed plots for available data.

``` r
library(ggplot2)
library(RColorBrewer)
```
