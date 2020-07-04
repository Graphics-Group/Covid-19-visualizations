Wrangling Data
================
Ian Lyttle
2020-07-04

The purpose of this document is to wrangle the data into useful forms.
We will write out two data frames: `iowa_counties` and
`iowa_county_population`.

``` r
library("fs")
library("tidyverse")
```

    ## ── Attaching packages ────────────────────────────────────────────────────────────────── tidyverse 1.3.0 ──

    ## ✓ ggplot2 3.3.0          ✓ purrr   0.3.3     
    ## ✓ tibble  2.1.3          ✓ dplyr   1.0.0     
    ## ✓ tidyr   1.0.0          ✓ stringr 1.4.0     
    ## ✓ readr   1.3.1.9000     ✓ forcats 0.4.0

    ## ── Conflicts ───────────────────────────────────────────────────────────────────── tidyverse_conflicts() ──
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
library("lubridate")
```

    ## 
    ## Attaching package: 'lubridate'

    ## The following object is masked from 'package:base':
    ## 
    ##     date

``` r
library("readxl")
library("rvest")
```

    ## Loading required package: xml2

    ## 
    ## Attaching package: 'rvest'

    ## The following object is masked from 'package:purrr':
    ## 
    ##     pluck

    ## The following object is masked from 'package:readr':
    ## 
    ##     guess_encoding

``` r
dir_source <- path("data", "download")
dir_target <- path("data", "wrangle")

dir_create(dir_target)
```

## Population data

``` r
iowa_county_population <-
  read_xls(
    path = path(dir_source, "iowa_counties_population.xls"), 
    sheet = "Counties", 
    range = "A7:M107"
  ) %>%
  transmute(
    fips = Fips,
    county = str_replace(Area, " County, Iowa", ""),
    population = `2019`
  ) %>%
  filter(fips > 19) %>%
  arrange(population) %>%
  mutate(
    cumulative_poulation = cumsum(population),
    quartile_population = cumulative_poulation/max(cumulative_poulation),
    population_group = cut(
      quartile_population, 
      breaks = c(0, 0.25, 0.50, 0.78, 1),
      labels = c("small", "mid-small", "mid-large", "large")
      # labels = c("FT-AA", "PW-HN", "ST-DA", "PK-LN")
    ),
    population_group = fct_rev(population_group)
  ) %>%
  arrange(desc(cumulative_poulation)) %>%
  print()
```

    ## # A tibble: 99 x 6
    ##     fips county   population cumulative_poula… quartile_popula… population_group
    ##    <dbl> <chr>         <dbl>             <dbl>            <dbl> <fct>           
    ##  1 19153 Polk         490161           3155070            1     large           
    ##  2 19113 Linn         226706           2664909            0.845 large           
    ##  3 19163 Scott        172943           2438203            0.773 mid-large       
    ##  4 19103 Johnson      151140           2265260            0.718 mid-large       
    ##  5 19013 Black H…     131228           2114120            0.670 mid-large       
    ##  6 19193 Woodbury     103107           1982892            0.628 mid-large       
    ##  7 19061 Dubuque       97311           1879785            0.596 mid-large       
    ##  8 19169 Story         97117           1782474            0.565 mid-large       
    ##  9 19049 Dallas        93453           1685357            0.534 mid-large       
    ## 10 19155 Pottawa…      93206           1591904            0.505 mid-large       
    ## # … with 89 more rows

## Cases data

Let’s read in the county data:

``` r
us_counties <- 
  read_csv(path(dir_source, "us-counties.csv"))
```

    ## Parsed with column specification:
    ## cols(
    ##   date = col_date(format = ""),
    ##   county = col_character(),
    ##   state = col_character(),
    ##   fips = col_character(),
    ##   cases = col_double(),
    ##   deaths = col_double()
    ## )

For now, let’s get the Iowa data, only a few of the columns, and we will
create a column to note the type of aggregation (“none” for the
county-level data).

``` r
iowa_counties_nyt <-
  us_counties %>%
  filter(state == "Iowa") %>%
  select(date, county, cases, deaths) 
```

``` r
date_scrape <- 
  file_info(path(dir_source, "access.html")) %>%
  `[[`("change_time") %>% 
  max() %>%
  as.Date()

html_scrape <- 
  read_html(path(dir_source, "access.html")) 

iowa_counties_content <-
  html_scrape %>%
  html_nodes("td") %>%
  map_chr(html_text)

ind_counties <- which(iowa_counties_content %in% iowa_county_population$county)

iowa_counties_scrape <- 
  tibble(
    date = date_scrape,
    county = iowa_counties_content[ind_counties],
    cases = as.numeric(iowa_counties_content[ind_counties + 2]),
    deaths = as.numeric(iowa_counties_content[ind_counties + 4])
  ) %>%
  mutate(
    cases = ifelse(is.na(cases), 0, cases),
    deaths = ifelse(is.na(deaths), 0, deaths),
  )
```

``` r
iowa_counties_combined <- iowa_counties_nyt

if (date_scrape > max(iowa_counties_nyt$date)) {
  iowa_counties_combined <- bind_rows(iowa_counties_nyt, iowa_counties_scrape)
}
```

``` r
iowa_counties <- 
  iowa_counties_combined %>%
  filter(county %in% iowa_county_population$county) %>%
  group_by(county) %>%
  arrange(desc(date)) %>% # take care of decreasing-counts
  mutate(
    cases = cummin(cases),   
    deaths = cummin(deaths) 
  ) %>%
  arrange(date) %>%
  mutate(
    new_cases = cases - lag(cases, default = 0),
    new_deaths = deaths - lag(deaths, default = 0),
    new_cases_week_avg = (cases - lag(cases, n = 7, default = 0)) / 7,
    new_deaths_week_avg = (deaths - lag(deaths, n = 7, default = 0)) / 7,
    aggregation = "none"
  ) %>%
  ungroup() %>%
  arrange(desc(date), desc(cases)) %>%
  print()
```

    ## # A tibble: 9,205 x 9
    ##    date       county cases deaths new_cases new_deaths new_cases_week_…
    ##    <date>     <chr>  <dbl>  <dbl>     <dbl>      <dbl>            <dbl>
    ##  1 2020-07-04 Polk    6446    179        80          0            80.7 
    ##  2 2020-07-04 Woodb…  3209     44         2          0            10.9 
    ##  3 2020-07-04 Black…  2245     58        10          0            31.7 
    ##  4 2020-07-04 Buena…  1710     11         1          0             3   
    ##  5 2020-07-04 Dallas  1263     29        14          0            16   
    ##  6 2020-07-04 Linn    1255     82         9          0            11.9 
    ##  7 2020-07-04 Johns…  1251      8         8          0            33.6 
    ##  8 2020-07-04 Marsh…  1041     19         0          0             4.57
    ##  9 2020-07-04 Story    757      3         5          0            14.4 
    ## 10 2020-07-04 Scott    752     10         4          0            24.6 
    ## # … with 9,195 more rows, and 2 more variables: new_deaths_week_avg <dbl>,
    ## #   aggregation <chr>

## Write

``` r
write_csv(iowa_counties, path(dir_target, "iowa-counties.csv"))
```

``` r
write_csv(iowa_county_population, path(dir_target, "iowa-county-population.csv"))
```
