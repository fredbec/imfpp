
<!-- README.md is generated from README.Rmd. Please edit that file -->

# impfpp - tidy WEO forecast data

<!-- badges: start -->
<!-- badges: end -->

Small package to download and tidy forecast data from the IMF World
Economic Outlook (WEO). The WEO gets published twice a year, once in
spring (April) and once in the fall (October). It contains forecast for
the current year, as well as for each of the next five years, for real
GDP growth, inflation, and a country’s current account balance in
percent of GDP. For each forecast target (combination of the target type
and year), there thus exist forecasts from 12 horizons, as well as 4
“data vintages”, that is, data revisions for past forecast targets. The
longest forecast horizon is the spring forecast for five years in
advance (for instance the spring 2022 forecast for the year 2027).

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

This is what the data will look like (for explanation of variables, see
table below)

    #>    country WEO_Country_Code ISOAlpha_3Code    target target_date
    #> 1:   Italy              136            ITA ngdp_rpch        1988
    #> 2:   Italy              136            ITA ngdp_rpch        1988
    #> 3:   Italy              136            ITA ngdp_rpch        1989
    #> 4:   Italy              136            ITA ngdp_rpch        1989
    #> 5:   Italy              136            ITA ngdp_rpch        1989
    #> 6:   Italy              136            ITA ngdp_rpch        1989
    #>    forecast_season horizon forecast_year       type prediction cgroup
    #> 1:               S    -1.5          1990 historical   4.155704      0
    #> 2:               F    -2.0          1990 historical   4.155704      0
    #> 3:               S    -0.5          1990 historical   3.162780      0
    #> 4:               F    -1.0          1990 historical   3.162780      0
    #> 5:               S    -1.5          1991 historical   3.162780      0
    #> 6:               F    -2.0          1991 historical   3.033403      0
    #>               economy      euro g7 g20 geoEmer geolInc oil
    #> 1: Advanced Economies Euro Area G7 G20                    
    #> 2: Advanced Economies Euro Area G7 G20                    
    #> 3: Advanced Economies Euro Area G7 G20                    
    #> 4: Advanced Economies Euro Area G7 G20                    
    #> 5: Advanced Economies Euro Area G7 G20                    
    #> 6: Advanced Economies Euro Area G7 G20

If you want different variables, exclude country groups, have true
values in long format, and/or don’t want explicit missing values,
alternatively run

``` r
download.process.weo(sheets = c("ngdp_rpch", "bca_gdp_bp6"))
download.process.weo(includeCountryGroups = FALSE)
download.process.weo(truevalExpand = FALSE)
download.process.weo(explicitMissings = FALSE)
```

The variables included in the dataset are as follows:

| Variable         | Explanation                                                                                                                                                                                                                                   |
|------------------|:----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| country          | country names as they appear in the WEO data (with minimal changes)                                                                                                                                                                           |
| WEO_Country_Code | country codes as they appear in the WEO data                                                                                                                                                                                                  |
| ISOAlpha_3Code   | another country code                                                                                                                                                                                                                          |
| target           | target variable, one of ngdp_rpch (GDP growth), pcpi_pch (CPI inflation), bca_gdp_bp6 (account balance, percent of GDP)                                                                                                                       |
| target_date      | the year the forecast is made for                                                                                                                                                                                                             |
| forecast_season  | the season the forecast was made in (S for Spring, F for fall)                                                                                                                                                                                |
| horizon          | the forecast horizon (e.g. 0.5 refers to a forecast from spring made for the current year, 1 refers to a forecast from fall made for next year)                                                                                               |
| forecast_year    | the year the forecast was made (forecast_year + horizon = target_date)                                                                                                                                                                        |
| type             | prediction or data correction (-\> “historical”), only relevant if in long format                                                                                                                                                             |
| prediction       | predicted value for the target                                                                                                                                                                                                                |
| tv_0.5, tv_1,…   | true values for the target, with the number after the underscore refering to the issue date - for instance, tv_0.5 is the realized value reported in April of the following year, tv_1 that reported in September of the following year, etc. |
| cgroup           | is country a country group (e.g. “Advanced Economies”)                                                                                                                                                                                        |
| economy          | categorization of country economy (Adv. Economies, Emerging Market Economies, Low-Income Developing Countries)                                                                                                                                |
| euro             | categorization into Euro Area countries (binary)                                                                                                                                                                                              |
| g7               | categoriation into G7 countries (binary)                                                                                                                                                                                                      |
| g20              | categorization into G20 countries (binary)                                                                                                                                                                                                    |
| geoEmer          | categorization of Emerging Market and Middle-Income Economies into geographical regions (Asia, Europe, Latin America, Middle East North Africa and Pakistan, Africa)                                                                          |
| geolInc          | categorization of Low-Income Developing Economies into geographical regions (Asia, Latin America, Sub-Saharan Africa, Others)                                                                                                                 |
| oil              | categorization of Oil-Producing countries                                                                                                                                                                                                     |

### Some references

Some references that analyze WEO data: Timmermann, A. (2007). An
evaluation of the world economic outlook forecasts. IMF Staff Papers,
54(1):133.

ToDo: Embed plots for available data.

``` r
library(ggplot2)
library(RColorBrewer)
```
