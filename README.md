
<!-- README.md is generated from README.Rmd. Please edit that file -->

# imfpp

<!-- badges: start -->
<!-- badges: end -->

The goal of imfpp is to download and tidy forecast data from the IMF
World Economic Outlook.

## Installation

You can install the development version of imfpp from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("fredbec/imfpp")
```

## Example

If you simply want to download, tidy and save all available forecasts
for GDP growth and inflation, run the following

``` r
library(imfpp)
## basic example code
download.process.weo()
```

If you want different variables and/or don’t want explicit missing
values, alternatively run

``` r
download.process.weo(sheets = c("ngdp_rpch", "bca_gdp_bp6"))
download.process.weo(explicitMissings = FALSE)
```

ToDo: Embed plots for available data.
