
<!-- README.md is generated from README.Rmd. Please edit that file -->

# bpsr

<!-- badges: start -->

[![R-CMD-check](https://github.com/dzulfiqarfr/bpsr/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/dzulfiqarfr/bpsr/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

bpsr allows users to get resources like datasets from Statistics
Indonesia (BPS) from R by wrapping up its application programming
interface (API).

## Installation

You can install the development version of bpsr like so:

``` r
# install.packages("devtools")
devtools::install_github("dzulfiqarfr/bpsr")
```

## Usage

You need to register yourself on [BPS API’s
website](https://https://webapi.bps.go.id/) first to be able to use
bpsr.

``` r
library(bpsr)
```

All functions from bpsr have a `bps_` prefix, so we can benefit from an
automatic code completion.

Most functions to get resources take a string of a resource identifier
(ID) as the first argument. So we typically need to look it up first in
the table of the resource of interest. Functions to get such tables have
a noun-y name. For example, use `bps_dataset()` to request the dataset
table.

``` r
table <- bps_dataset(lang = "eng")
table
#> # A tibble: 10 × 9
#>    dataset_id title              subje…¹ subject def   notes verti…² unit  graph
#>    <chr>      <chr>              <chr>   <chr>   <chr> <chr> <chr>   <lgl> <int>
#>  1 70         Population 5 Year… 2       Commun… <NA>  &lt;… 1       NA        1
#>  2 111        Percentage Househ… 2       Commun… <NA>  Sour… 1       NA        1
#>  3 391        Percentage of Hou… 2       Commun… <NA>  Sour… 1       NA        1
#>  4 392        Active Mobile Pho… 2       Commun… <NA>  Sour… 1       NA        1
#>  5 393        Percentage of Hou… 2       Commun… <NA>  Sour… 1       NA        1
#>  6 395        Percentage of Pop… 2       Commun… <NA>  Sumb… 1       NA        1
#>  7 396        Percentage of Hou… 2       Commun… <NA>  &lt;… 1       NA        1
#>  8 398        Percentage of Hou… 2       Commun… <NA>  &lt;… 1       NA        1
#>  9 402        Percentage Househ… 2       Commun… <NA>  Sour… 152     NA        1
#> 10 403        Household Members… 2       Commun… <NA>  Sour… 1       NA        1
#> # … with abbreviated variable names ¹​subject_id, ²​vertical_var_group_id
```

Functions to request datasets have `bps_get_` prefix. Use
`bps_get_dataset()` to request a dataset.

``` r
data <- bps_get_dataset(table$dataset_id[[1]], lang = "eng")
data
#> # A tibble: 558 × 5
#>    vertical_var derived_var  year period   var
#>    <chr>        <chr>       <int> <chr>  <dbl>
#>  1 INDONESIA    Male         2014 Annual  18.8
#>  2 INDONESIA    Male         2015 Annual  23.7
#>  3 INDONESIA    Male         2016 Annual  27.2
#>  4 INDONESIA    Male         2017 Annual  34.5
#>  5 INDONESIA    Male         2018 Annual  42.3
#>  6 INDONESIA    Male         2019 Annual  50.5
#>  7 INDONESIA    Male         2020 Annual  56.6
#>  8 INDONESIA    Male         2021 Annual  65.0
#>  9 INDONESIA    Female       2014 Annual  15.4
#> 10 INDONESIA    Female       2015 Annual  20.2
#> # … with 548 more rows
#> # ℹ Read the metadata with `bps_metadata()`
```

Under the hood, bpsr parses the JSON returned by the API into a
[tibble](https://tibble.tidyverse.org/index.html). More importantly, the
package arranges the dataset in a [tidy
structure](https://tidyr.tidyverse.org/articles/tidy-data.html), which
is well suited for analysis.

Use `bps_get_trade_hs_*()` to request trade datasets. These trade
functions take the Harmonized System (HS) chapter or code as the first
argument. To look up the HS chapter or code, use `bps_hs_code()`.

``` r
hs <- bps_hs_code()

export <- bps_get_trade_hs_chapter(
  hs$hs_chapter[[1]],
  "export", 
  year = 2021,
  freq = "annual"
)

export
#> # A tibble: 67 × 7
#>    hs_chapter description    port                   partner  year net_w…¹ export
#>    <chr>      <chr>          <chr>                  <chr>   <int>   <dbl>  <dbl>
#>  1 01         Binatang hidup ATAPUPU                EAST T…  2021  1.50e4 1.98e5
#>  2 01         Binatang hidup KUALA NAMU INTERNATIO… JAPAN    2021  5   e0 1.03e2
#>  3 01         Binatang hidup KUALA NAMU INTERNATIO… SINGAP…  2021  1.50e4 1.77e4
#>  4 01         Binatang hidup KUALA TANJUNG          MALAYS…  2021  2.36e4 6.51e4
#>  5 01         Binatang hidup NGURAH RAI (U)         EAST T…  2021  1.93e3 1.12e4
#>  6 01         Binatang hidup NGURAH RAI (U)         HONG K…  2021  2.4 e2 2.05e4
#>  7 01         Binatang hidup SEKUPANG               SINGAP…  2021  2.52e7 5.59e7
#>  8 01         Binatang hidup SOEKARNO-HATTA (U)     AFGHAN…  2021  1.28e2 5.5 e4
#>  9 01         Binatang hidup SOEKARNO-HATTA (U)     AUSTRIA  2021  6   e1 2.07e2
#> 10 01         Binatang hidup SOEKARNO-HATTA (U)     BAHRAIN  2021  1.27e2 8.40e3
#> # … with 57 more rows, and abbreviated variable name ¹​net_weight
#> # ℹ Read the metadata with `bps_metadata()`
```

Some resources are available for download. We can use the download
functions to get those resources. This is especially useful to get a
dataset stored in a spreadsheet, which comes in a Microsoft Excel file
format with the “.xls” extension.

``` r
spreadsheet <- bps_dataset_spreadsheet(lang = "eng")
bps_download_spreadsheet(spreadsheet$dataset_id[[1]], "spreadsheet.xls")
```

Aside from the common usage, bpsr also provides other features,
including:

- requesting multiple datasets at once using `bps_get_datasets()`; and  
- downloading multiple resources at once using `bps_download()`.

Learn more in the Getting Started vignette (`vignette("bpsr")`).
