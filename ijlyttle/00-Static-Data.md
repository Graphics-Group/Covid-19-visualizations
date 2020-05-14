Static Data
================
Ian Lyttle
2020-05-14

The purpose of this document is to download the datasets used from
static sources.

``` r
library("fs")
```

``` r
dir_create("data")

dir_target <- path("data", "download")
dir_create(dir_target)
```

``` r
download.file(
  "https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv",
  path(dir_target, "us-counties.csv")
)
```

``` r
download.file(
  "https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv",
  path(dir_target, "us-states.csv")
)
```

We download a spreadsheet that contains county-population data.

``` r
# we don't need to download this *every* time
file_excel <- path(dir_target, "iowa_counties_population.xls")

if (!file_exists(file_excel)) {
  download.file(
    "https://www.icip.iastate.edu/sites/default/files/uploads/tables/population/popest-annual.xls",
    file_excel
  ) 
}
```
